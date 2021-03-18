# installation instructions -----

browseURL('https://github.com/dkidney/gibbonsecr')


# install dependencies -----
for (pkg in c(
	'CircStats',
	'tidyverse',
	'knitr',
	'maptools',
	'MASS',
	'raster',
	'Rcpp',
	'scales',
	'secr',
	'sp',
	'tcltk2'
)) {
	if (!requireNamespace(pkg)) {
		install.packages(pkg)
	}
}

install.packages("https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/RcppArmadillo_0.9.900.3.0.tar.gz", repos=NULL, type="source")


# install gibbonsecr -----

# recommend running this next line from RStudio
# - follow on-screen prompts / instructions but don't update RcppArmadillo if you get a package update prompt
devtools::install_github("dkidney/gibbonsecr")





# In case it helps, this is my session info:
sessionInfo()
# R version 4.0.4 (2021-02-15)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 10.16
#
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
#
# locale:
# [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#  [1] gibbonsecr_1.0.1 maptools_1.0-2   sp_1.4-5         forcats_0.5.0    stringr_1.4.0
#  [6] dplyr_1.0.4      purrr_0.3.4      readr_1.4.0      tidyr_1.1.2      tibble_3.0.6
# [11] ggplot2_3.3.3    tidyverse_1.3.0  magrittr_2.0.1
#
# loaded via a namespace (and not attached):
#  [1] httr_1.4.2          pkgload_1.1.0       jsonlite_1.7.2      splines_4.0.4       modelr_0.1.8
#  [6] RcppParallel_5.0.2  assertthat_0.2.1    blob_1.2.1          cellranger_1.1.0    remotes_2.2.0
# [11] sessioninfo_1.1.1   pillar_1.4.7        backports_1.1.10    lattice_0.20-41     glue_1.4.2
# [16] digest_0.6.27       rvest_0.3.6         colorspace_2.0-0    Matrix_1.3-2        pkgconfig_2.0.3
# [21] devtools_2.3.2      RcppNumerical_0.4-0 broom_0.7.2         raster_3.4-5        haven_2.3.1
# [26] scales_1.1.1        processx_3.4.5      CircStats_0.2-6     mgcv_1.8-33         generics_0.1.0
# [31] usethis_1.6.3       ellipsis_0.3.1      withr_2.4.1         secr_4.3.3          cli_2.3.0
# [36] crayon_1.4.1        readxl_1.3.1        memoise_1.1.0       ps_1.5.0            fs_1.5.0
# [41] nlme_3.1-152        MASS_7.3-53.1       xml2_1.3.2          foreign_0.8-81      pkgbuild_1.2.0
# [46] tools_4.0.4         prettyunits_1.1.1   hms_0.5.3           tcltk2_1.2-11       lifecycle_1.0.0
# [51] munsell_0.5.0       reprex_0.3.0        callr_3.5.1         compiler_4.0.4      rlang_0.4.10
# [56] grid_4.0.4          rstudioapi_0.13     tcltk_4.0.4         boot_1.3-26         testthat_3.0.2
# [61] gtable_0.3.0        codetools_0.2-18    abind_1.4-5         DBI_1.1.0           R6_2.5.0
# [66] lubridate_1.7.9     rprojroot_2.0.2     desc_1.2.0          stringi_1.5.3       parallel_4.0.4
# [71] Rcpp_1.0.6          vctrs_0.3.6         dbplyr_1.4.4        tidyselect_1.1.0


