#' DESCRIPTION
#' This script creates figure for SCJS output with seasonal patterns

# setup -------------------------------------------------------------------

rm(list=ls(all.names=T))
pacman::p_load(tidyverse)

d0 <- readRDS("result/re_model_cjs.rds") %>% 
  as_tibble(rownames = "param") %>% 
  rename(lower = '2.5%',
         median = '50%',
         upper = '97.5%')

## group match
## CL: cluster - 1st, 2nd, 3rd = c(1,2,3)
## TR: treatment - control, early, late = c(1,2,3)
Grid <- expand.grid(CL = 1:3, TR = 1:3)
Grid$G <- 1:9

## date
JH <- readRDS("data_fmt/matrix_jh.rds")
J <- as.matrix(JH[,which(colnames(JH) == "occasion1"):ncol(JH)])
date <- apply(J, 2, median, na.rm = T) %>% 
  as.Date(origin = as.Date("1970-01-01"))

dat_date <- tibble(occasion = 1:length(date),
                   date = date)


# format ------------------------------------------------------------------

dat <- d0 %>% 
  filter(str_detect(param, "phi")) %>% 
  mutate(x = str_extract(param, pattern = "\\[.{1,}\\]"),
         y = str_remove_all(x, pattern = "\\[|\\]")) %>%
  separate(y, into = c("group", "occasion"), sep = ",") %>% 
  mutate(group = as.numeric(group),
         occasion = as.numeric(occasion)) %>% 
  left_join(Grid, by = c("group" = "G")) %>% 
  left_join(dat_date, by = "occasion") %>% 
  mutate(treatment = case_when(TR == 1 ~ "Control",
                               TR == 2 ~ "Early",
                               TR == 3 ~ "Late"),
         treatment = fct_relevel(treatment,
                                 c("Early", "Late")),
         cluster = case_when(CL == 1 ~ "Fast",
                             CL == 2 ~ "Medium",
                             CL == 3 ~ "Slow"),
         cluster = fct_relevel(cluster,
                               c("Slow", "Medium")))


# plot --------------------------------------------------------------------

g <- ggplot(dat) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper),
              fill = grey(0.9)) +
  geom_line(aes(x = date, y = median), 
            color = grey(0.5)) +
  geom_point(aes(x = date, y = median), 
             size = 1.5) +
  facet_grid(rows = vars(treatment),
             cols = vars(cluster)) + 
  ylab("Survival probability") +
  xlab("Date") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

print(g)  

ggsave(g,
       file = "output/figure_phi_timeseries.eps",
       width = 8,
       height = 6,
       device = "eps")

ggsave(g,
       file = "output/figure_phi_timeseries.pdf",
       width = 8,
       height = 6)