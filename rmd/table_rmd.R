#' DESCRIPTION
#' This script creates README table

# setup -------------------------------------------------------------------

rm(list=ls(all.names=T))
library(tidyverse)


# table -------------------------------------------------------------------

df_fnm <- tibble(File = paste0("`",
                               c(list.files("code",
                                            full.names = TRUE),
                                 list.files("data_raw",
                                            full.names = TRUE),
                                 list.files("data_fmt",
                                            full.names = TRUE),
                                 list.files("result",
                                            full.names = TRUE)),
                               "`")) %>% 
  filter(!str_detect(File, "rawcolumn|old|original")) %>% 
  arrange(File)

df_fnm <- df_fnm %>% 
  mutate(Description = c("Figure for survival estimates at the end of the experiment",
                         "Figure for seasonal survival estimates",
                         "Format mark-recapture data for analysis",
                         "JAGS model code for the spatial CJS model",
                         "Run the spatial CJS model in JAGS",
                         "Matrix capture history - data supplied for the spatial CJS model",
                         "Matrix location history - data supplied for the spatial CJS model",
                         "Matrix julian-date history - data supplied for the spatial CJS model",
                         "Raw mark-recapture data",
                         "JAGS model output"))
