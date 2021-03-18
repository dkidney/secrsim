simulate_survey_data = function(detectors, n_arrays = 1, ...){

    # checks
    dots = list(...)
    if (dots$n_occasions == 1) {
        if (dots$g0 != 1) {
            stop("g0 can't be estimated from single-occasion survey data")
        }
        if (dots$p_available != 1) {
            warning("p_available can't be estimated from single-occasion survey data",
                    "- density estimates will be in terms of the density of available animals/groups")
        }
    }

    detectors %<>%
        transmute(
            detector_id = 1:nrow(.),
            detector_x = x,
            detector_y = y
        )

    survey_data_list = 1:n_arrays %>%
        set_names() %>%
        map(function(i) {
            detectors %>% simulate_survey_data_(...)
        })

    list(
        pop = survey_data_list %>%
            map_dfr('pop', .id = 'array') %>%
            mutate(across(array, as.numeric)),
        detectors = survey_data_list %>%
            map_dfr('detectors', .id = 'array') %>%
            mutate(across(array, as.numeric)),
        detections = survey_data_list %>%
            map_dfr('detections', .id = 'array') %>%
            mutate(across(array, as.numeric))
    )
}

simulate_survey_data_ = function(detectors,
                                 density = 0.64,
                                 g0 = 1,
                                 sigma = 1250,
                                 kappa = 70,
                                 p_available = 0.5,
                                 n_occasions = 2,
                                 buffer = 6000){

    # simulate population
    pop = detectors %>%
        simulate_home_range_centres(
            density = density,
            buffer = buffer
        )

    # simulate availability
    availability = pop %>%
        simulate_availability(
            n_occasions = n_occasions,
            p_available = p_available
        )

    # remove unavailable animal-occasion combinations and join with true locations
    detections = availability %>%
        filter(available) %>%
        select(-available) %>%
        inner_join(x = pop, by = 'animal_id')

    # expand by detectors
    detections %<>%
        mutate(detectors = 1:nrow(.) %>% map(~detectors)) %>%
        unnest_legacy()

    # calculate true distances
    detections %<>%
        mutate(true_distance = calc_distance(detector_x, detector_y,
                                             animal_x, animal_y))

    # remove undetected animal-occasion-trap combinations
    detections %<>%
        mutate(det_prob = true_distance %>% calc_detprob(g0, sigma)) %>%
        filter(det_prob > runif(nrow(.)))

    # add bearing estimates
    detections %<>%
        mutate(true_bearing = calc_bearing(detector_x, detector_y,
                                           animal_x, animal_y)) %>%
        mutate(bearing_estimate = true_bearing %>%
                   simulate_bearing_estimates(k = kappa, units = "degrees"))

    list(
        pop = pop %>%
            select(id = animal_id, x = animal_x, y = animal_y),
        detectors = detectors %>%
            select(id = detector_id, x = detector_x, y = detector_y),
        detections = detections
    )

}

simulate_home_range_centres = function(detectors, density, buffer) {
    secr::sim.popn(
        D = density / 100,
        core = detectors %>% select(x = detector_x, y = detector_y),
        buffer = buffer,
        covariates = NULL
    ) %>%
        as_tibble() %>%
        mutate(animal_id = 1:nrow(.)) %>%
        select(animal_id, animal_x = x, animal_y = y)
}

simulate_bearing_estimates = function(true_bearing, kappa, units = c('degrees', 'radians')) {
    units = match.arg(units)
    bearing_error = CircStats::rvm(n = length(true_bearing), mean = 0, k = kappa)
    if (units == 'degrees') {
        bearing_error %<>% radians_to_degrees()
        bearing_estimate = (true_bearing + bearing_error) %% 360
    } else {
        bearing_estimate = (true_bearing + bearing_error) %% (2 * pi)
    }
    bearing_estimate
}

simulate_availability = function(pop, n_occasions, p_available) {
    N = nrow(pop)
    S = n_occasions
    array(
        p_available > runif(N * S),
        dim = c(N, S),
        dimnames = list(pop$animal_id, as.character(1:S))
    ) %>%
        as_tibble() %>%
        mutate(animal_id = pop$animal_id) %>%
        pivot_longer(cols = colnames(.) %>% setdiff("animal_id"),
                     names_to = 'occasion',
                     values_to = 'available') %>%
        mutate(across(occasion, as.numeric))
}

calc_detprob = function(true_distance, g0, sigma) {
    g0 * exp(-true_distance^2 / 2 / sigma^2)
}

calc_distance = function(from_x, from_y, to_x, to_y) {
    sqrt((from_x - to_x)^2 + (from_y - to_y)^2)
}

calc_bearing = function(from_x, from_y, to_x, to_y,
                        units = c('degrees', 'radians')) {
    units = match.arg(units)
    opp = to_x - from_x
    adj = to_y - from_y
    bearing = atan(opp / adj)
    i = adj < 0
    bearing[i] = bearing[i] + pi
    j = opp < 0 & adj >= 0
    bearing[j] = bearing[j] + 2 * pi

    # edge-case corrections
    k = opp == 0 & adj == 0
    bearing[k] = NA
    k = opp == 0 & adj > 0
    bearing[k] = 0
    k = opp > 0 & adj == 0
    bearing[k] = 0.5 * pi
    k = opp == 0 & adj < 0
    bearing[k] = 1.0 * pi
    k = opp < 0 & adj == 0
    bearing[k] = 1.5 * pi

    if (units == 'degrees'){
        bearing = bearing * 180 / pi
    }
    bearing
}

radians_to_degrees = function(x) {
    (x * 180 / pi) %% 360
}

degrees_to_radians = function(x) {
    (x * pi / 180) %% (2 * pi)
}

add_detector_usage = function(x, n_occasions) {
    x %>% mutate(usage = str_c(rep('1', n_occasions), collapse = ''))
}

estimate_time_remaining = function(sim, n_sims, start_time) {
    current_time = Sys.time()
    mins_elapsed = difftime(current_time, start_time, units = 'mins')
    mins_per_sim = mins_elapsed / sim
    sims_remaining = n_sims - sim
    mins_remainig = mins_per_sim * sims_remaining
    finish_time = current_time + mins_remainig
    message('\n', str_glue('mean time per sim: {round(mins_per_sim, 1)} mins'))
    if (sim < n_sims) {
        run_time = difftime(finish_time, current_time)
        message(str_glue('estimated run time remaining: {round(run_time, 1)} {attr(run_time, "units")}'))
        message(str_glue('estimated finish time: {finish_time}'))
    } else {
        run_time = difftime(finish_time, start_time)
        prefix = ''
        message(str_glue('run time: {round(run_time, 1)} {attr(run_time, "units")}'))
        message(str_glue('finish time: {finish_time}'))
    }
}



