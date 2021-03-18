library(magrittr)
library(tidyverse)
library(gibbonsecr)

funs = new.env()
source('simulation_functions.R', funs)


# parameters ------------------------------------

# density of animals/groups N km-2
density = 0.64

# detection function intercept
g0 = 0.75

# detection function scale parameter (m)
sigma = 1250

# bearing error distribution shape parameter (von Mises distribution)
kappa = 70

# proportion of group/animals that are available for detection per sampling occasion
p_available = 0.5

# number of sampling occasions per replicate trap array
n_occasions = 2

# maximum distance (m) at which detection probability is non-zero
buffer = 6000

# distance (m) between points in mask (needed for model fitting - smaller is better but slower)
mask_spacing = 250

# number of replicate arrays (can be replicates in time and/or space)
n_arrays = 13

# relative locations of traps/detectors in a single array
detector_xdim = 4
detector_ydim = 1
detector_spacing = 1.5 * sigma
detectors = tidyr::expand_grid(
    x = 1:detector_xdim * detector_spacing,
    y = 1:detector_ydim * detector_spacing
)

# number of simulations
# n_sims = 1000
n_sims = 10


# quick example ------------------------------

if (0) {

    # deliberately setting n_arrays to 1 and inflating density
    # to get enough points to make a pretty plot
    example_data = detectors %>%
        funs$simulate_survey_data(
            n_arrays = 1,
            density = density * n_arrays,
            g0 = g0,
            sigma = sigma,
            kappa = kappa,
            p_available = p_available,
            n_occasions = n_occasions,
            buffer = buffer
        )

    pop_ = example_data$pop %>% filter(array == 1)
    detectors_ = example_data$detectors %>% filter(array == 1)
    detections_ = example_data$detections %>% filter(array == 1)

    pop_ %>%
        ggplot() +
        geom_point(aes(x, y), col = 'grey') +
        geom_point(aes(x, y), col = 'blue', pch = 15, size = 2,
                   data = detectors_) +
        geom_point(aes(x, y), col = 'red',
                   data = pop_ %>%
                       filter(id %in% detections_$animal_id))

    print(gfit(import_data(
        detections = detections_ %>%
            rename(post = detector_id,
                   group = animal_id,
                   bearing = bearing_estimate),
        posts = detectors_ %>%
            rename(post = id) %>%
            funs$add_detector_usage(n_occasions)
    )))

}


# run simulation --------------------------------

fixed = list()
start = c('D.(Intercept)' = log(density),
          'g0.(Intercept)' = qlogis(g0),
          'sigma.(Intercept)' = log(sigma),
          'pcall.(Intercept)' = qlogis(p_available),
          'bearings.(Intercept)' = log(kappa))
if (n_occasions == 1) {
    start[names(start) == 'D.(Intercept)'] = log(density * p_available)
}
if (n_occasions == 1 | g0 == 1) {
    fixed$g0 = 1
    start = start[names(start) !='g0.(Intercept)']
}
if (n_occasions == 1 | p_available == 1) {
    fixed$pcall = 1
    start = start[names(start) !='pcall.(Intercept)']
}

start_time = Sys.time()
estimates = 1:n_sims %>%
    map_dfr(function(sim){ # sim = 1

        message('\n--------------------------------------------------')
        message('\nsim ', sim, ' of ', n_sims, '\n')

        fit_result = try({

            survey_data = detectors %>%
                funs$simulate_survey_data(
                    n_arrays = n_arrays,
                    density = density,
                    g0 = g0,
                    sigma = sigma,
                    kappa = kappa,
                    p_available = p_available,
                    n_occasions = n_occasions,
                    buffer = buffer
                )

            capthist = suppressMessages(suppressWarnings({
                gibbonsecr::import_data(
                    detections = survey_data$detections %>%
                        rename(
                            post = detector_id,
                            group = animal_id,
                            bearing = bearing_estimate
                        ),
                    posts = survey_data$detectors %>%
                        rename(post = id) %>%
                        funs$add_detector_usage(n_occasions),
                    details = list(
                        bearings = list(units = 'degrees')
                    )
                )
            }))

            mask = suppressMessages(suppressWarnings({
                gibbonsecr::make_mask(
                    capthist,
                    buffer = buffer,
                    spacing = mask_spacing
                )
            }))

            fit_result = suppressMessages(suppressWarnings({
                gibbonsecr::gfit(
                    capthist = capthist,
                    mask = mask,
                    fixed = fixed,
                    start = start,
                    model.options = list(detfunc = 0, bearings = 1)
                )
            }))

            print(fit_result)

            funs$estimate_time_remaining(sim, n_sims, start_time)

            fit_result

        })

        if (inherits(fit_result, 'try-error')) {
            print(paste('fitting failed:', attr(fit_result, 'condition')$message))
            return(NULL)
        }

        if (fit_result$nlm$code >= 3) {
            print(paste('rejecting estimates: nlm code =', fit_result$nlm$code))
            return(NULL)
        }

        if (fit_result$nlm$iterations == 0) {
            print('rejecting estimates: nlm iterations = 0')
            return(NULL)
        }

        coef(fit_result)

    })

n_successful_sims = nrow(estimates)
message('\n--------------------------------------------------')
message('\n', str_glue('{n_successful_sims} / {n_sims} successful simulations'))


# plot results --------------------------------

clean_names = function(x) {
    x %>%
        str_remove('\\.\\(Intercept\\)') %>%
        str_replace('^pcall$', 'p_available') %>%
        str_replace('^bearings$', 'kappa') %>%
        str_replace('^D$', 'density')
}

names(estimates) %<>% clean_names
names(start) %<>% clean_names

process_estimates = function(x) {
    x %>%
        pivot_longer(cols = colnames(.),
                     names_to = 'parameter',
                     values_to = 'linear_predictor_scale') %>%
        mutate(link_scale = linear_predictor_scale %>% {
            case_when(
                parameter == "g0" ~ plogis(.),
                parameter == "pcall" ~ plogis(.),
                TRUE ~ exp(.)
            )})
}

plot_data_estimates = estimates %>%
    process_estimates()

plot_data_mean_estimates = estimates %>%
    summarise(across(everything(), mean)) %>%
    process_estimates()

plot_data_truth = start %>%
    as.list() %>%
    as_tibble() %>%
    process_estimates()

make_plot = function(link_scale = TRUE) {
    scale = if (link_scale) 'link_scale' else 'linear_predictor_scale'
    plot_data_estimates %>%
        ggplot() +
        geom_histogram(aes(x = .data[[scale]]), bins = 30, fill = 'grey75') +
        geom_vline(aes(xintercept = .data[[scale]]), data = plot_data_mean_estimates, col = 'green') +
        geom_vline(aes(xintercept = .data[[scale]]), data = plot_data_truth, col = 'blue') +
        facet_wrap(~ parameter, scales = 'free') +
        labs(x = str_glue('Estimates ({str_replace_all(scale, "_", " ")})'),
             title = str_glue('{n_successful_sims} simulations')) +
        theme_minimal()
}

plot1 = make_plot(link = FALSE)
plot2 = make_plot(link = TRUE)

print(plot1)
print(plot2)




