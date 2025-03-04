README
================

# Article information

Title: Seasonal timing of ecosystem linkage mediates life-history
variation in a salmonid fish population

Author: Ueda R, Kanaiwa M, Terui A, Takimoto G, Sato T

Journal: Ecology

# Analytical flow

This repository covers statistical procedures for the spatial CJS model.
Run `code/run_model_scjs.R` to reproduce the reported results. Output
data are visualized in scripts with `figure_` prefix.

# File description

| File | Description |
|:---|:---|
| `code/figure_phi.R` | Figure for survival estimates at the end of the experiment |
| `code/figure_phi_timeseries.R` | Figure for seasonal survival estimates |
| `code/format_mrdata.R` | Format mark-recapture data for analysis |
| `code/model_scjs.R` | JAGS model code for the spatial CJS model |
| `code/run_model_scjs.R` | Run the spatial CJS model in JAGS |
| `data_fmt/matrix_ch.rds` | Matrix capture history - data supplied for the spatial CJS model |
| `data_fmt/matrix_dh.rds` | Matrix location history - data supplied for the spatial CJS model |
| `data_fmt/matrix_jh.rds` | Matrix julian-date history - data supplied for the spatial CJS model |
| `data_raw/data_mr.csv` | Raw mark-recapture data |
| `result/re_model_cjs.rds` | JAGS model output |

# Session information

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] runjags_2.2.1-7 lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1  
    ##  [5] dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1    
    ##  [9] tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6      compiler_4.4.2    tidyselect_1.2.1  parallel_4.4.2   
    ##  [5] scales_1.3.0      yaml_2.3.10       fastmap_1.2.0     coda_0.19-4.1    
    ##  [9] lattice_0.22-6    here_1.0.1        R6_2.5.1          generics_0.1.3   
    ## [13] knitr_1.48        munsell_0.5.1     rprojroot_2.0.3   pillar_1.9.0     
    ## [17] tzdb_0.4.0        rlang_1.1.4       utf8_1.2.4        stringi_1.8.4    
    ## [21] xfun_0.47         timechange_0.3.0  cli_3.6.3         withr_3.0.2      
    ## [25] magrittr_2.0.3    digest_0.6.33     grid_4.4.2        rstudioapi_0.14  
    ## [29] hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0  
    ## [33] glue_1.8.0        fansi_1.0.6       colorspace_2.1-1  pacman_0.5.1     
    ## [37] rmarkdown_2.28    tools_4.4.2       pkgconfig_2.0.3   htmltools_0.5.8.1
