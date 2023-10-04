
# setup -------------------------------------------------------------------

rm(list=ls(all.names=T))
pacman::p_load(tidyverse)

d0 <- readRDS("result/re_model_cjs.rds") %>% 
  as_tibble(rownames = "param") %>% 
  rename(lower = '2.5%',
         median = '50%',
         upper = '97.5%') %>%  
  mutate(x = str_extract(param, pattern = "\\[.{1,}\\]"),
         y = str_remove_all(x, pattern = "\\[|\\]"))

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

df_phi <- d0 %>% 
  filter(str_detect(param, "phi")) %>% 
  separate(y,
           into = c("group", "occasion"),
           sep = ",",
           fill = "left") %>% 
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
                               c("Slow", "Medium"))) %>% 
  filter(occasion == 7)

df_lrr <- d0 %>% 
  filter(str_detect(param, "lrr")) %>% 
  separate(y,
           into = c("CL", "TR"),
           sep = ",",
           fill = "left",
           convert = TRUE) %>% 
  left_join(Grid, by = c("CL", "TR")) %>% 
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

g_phi <- ggplot(df_phi) +
  geom_segment(aes(x = cluster,
                   xend = cluster,
                   y = lower,
                   yend = upper),
               color = grey(0.7)) +
  geom_point(aes(x = cluster, y = median), 
             size = 2) +
  facet_wrap(facets = ~ treatment, ncol = 3) + 
  ylab("Survival probability") +
  xlab("Growth cluster") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect("white"),
        panel.grid = element_blank())

g_lrr <- ggplot(df_lrr) +
  geom_segment(aes(x = cluster,
                   xend = cluster,
                   y = lower,
                   yend = upper),
               color = grey(0.7)) +
  geom_point(aes(x = cluster, y = median), 
             size = 2) +
  facet_wrap(facets = ~ treatment, ncol = 3) + 
  ylab("LRR Survival Probability") +
  xlab("Growth cluster") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.background = element_rect("white"),
        panel.grid = element_blank())


ggsave(g_phi,
       file = "output/figure_phi.eps",
       width = 8,
       height = 3,
       device = "eps")

ggsave(g_phi,
       file = "output/figure_phi.pdf",
       width = 8,
       height = 3)

ggsave(g_lrr,
       file = "output/figure_lrr.eps",
       width = 8,
       height = 3,
       device = "eps")

ggsave(g_lrr,
       file = "output/figure_lrr.pdf",
       width = 8,
       height = 3)