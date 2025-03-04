#' DESCRIPTION
#' This script format mark-recapture data for Bayesian analysis

# setup -------------------------------------------------------------------

rm(list=ls(all.names=T))
library(tidyverse)


# data --------------------------------------------------------------------

d0 <- read_csv("data_raw/data_mr.csv")
dat <- d0 %>%
  mutate(id = as.character(id),
         SecUL = ceiling(distance * 0.1) * 10, # upstream landmark
         SecDL = SecUL - 10, # downstream landmark
         date = paste(year, month, day, sep = "-"),
         ym = format(as.Date(date), format="%Y-%m"), # reformat date
         Julian = julian(as.Date(date))) # Julian date

dat1 <- dat %>%
  drop_na(cluster) %>%
  filter(id %in% names(which(table(id) > 1)) | ym != '2017-10') %>%
  filter(id %in% names(which(table(unlist(tapply(id, treatment, unique))) == 1))) %>% 
  mutate(Occasion = paste0("occasion", as.numeric(factor(ym))))


# data manipulation -------------------------------------------------------
## create history matrices
## - Variable transformation
## - `Occasion` - date (`ym`) transformation: `1...7 = 2016-05...2017-10`
## - `CL` - `cluster` transformation: `c(1, 2, 3) = c('1st', '2nd', '3rd')`
## - `TR` - `treatment` transformation: `c(1, 2, 3) = c('control', 'early', 'late')`

## CH: Capture history - binary information of recaptured or not
CH <- dat1 %>%
  mutate(capture = 1) %>%
  arrange(Occasion) %>%
  pivot_wider(names_from = Occasion,
              values_from = capture,
              id_cols = c(id, cluster, treatment),
              values_fill = list(capture = 0)) %>%
  rename(CL = cluster) %>%
  mutate(CL = as.numeric(factor(CL),
                         levels = c("1st",
                                    "2nd",
                                    "3rd"))) %>%
  rename(TR = treatment) %>%
  mutate(TR = as.numeric(factor(TR,
                                levels = c("control",
                                           "early",
                                           "late")))) %>% # 1: control, 2: early, 3: late
  arrange(id)

## put NA until the first capture
ch <- CH %>%
  select(sort(unique(dat1$Occasion)))

for(i in 1:nrow(ch)) {
  x <- min(which(ch[i,] == 1)) - 1
  if(x != 0) ch[i, 1:x] <- NA
}

CH[, which(colnames(CH) %in% sort(unique(dat1$Occasion)))] <- ch

print(CH)

## DH: Location history - distance from the downstream end to the upstream landmark of a (re)capture subsection
DH <- dat1 %>%
  arrange(Occasion) %>%
  pivot_wider(names_from = Occasion,
              values_from = SecUL,
              id_cols = c(id, cluster, treatment)) %>%
  rename(CL = cluster) %>%
  mutate(CL = as.numeric(factor(CL),
                         levels = c("1st",
                                    "2nd",
                                    "3rd"))) %>%
  rename(TR = treatment) %>%
  mutate(TR = as.numeric(factor(TR,
                                levels = c("control",
                                           "early",
                                           "late")))) %>% # 1: control, 2: early, 3: late
  arrange(id)

print(DH)

## JH: Julian history - Julian date of capture for each occasion
JH <- dat1 %>%
  arrange(Occasion) %>%
  pivot_wider(names_from = Occasion,
              values_from = Julian,
              id_cols = c(id, cluster, treatment)) %>%
  rename(CL = cluster) %>%
  mutate(CL = as.numeric(factor(CL),
                         levels = c("1st",
                                    "2nd",
                                    "3rd"))) %>%
  rename(TR = treatment) %>%
  mutate(TR = as.numeric(factor(TR,
                                levels = c("control",
                                           "early",
                                           "late")))) %>% # 1: control, 2: early, 3: late
  arrange(id)

print(JH)


# export ------------------------------------------------------------------

saveRDS(DH, "data_fmt/matrix_dh.rds")
saveRDS(CH, "data_fmt/matrix_ch.rds")
saveRDS(JH, "data_fmt/matrix_jh.rds")
