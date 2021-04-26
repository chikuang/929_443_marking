setwd("~/../Desktop/Forecasting/443/")

library(purrr)
library(dplyr)
library(readr)
source("./load_S1_443.R")
source("./../load_S2_443.R")
source("./../load_S3_443.R")
source("./../load_S4_443.R")
source("./../load_S5_443.R")
setwd("~/../Desktop/Forecasting/443/")
S1 <- readRDS("./S1_result_443.Rds")
S2 <- readRDS("./S2_result_443.Rds")
S3 <- readRDS("./S3_result_443.Rds")
S4 <- readRDS("./S4_result_443.Rds")
S5 <- readRDS("./S5_result_443.Rds")

df <- list(S1, S2, S3, S4, S5) %>% reduce(left_join, by = "id")

N_submission <- nrow(df)
# If missing submission, then rank then the last in class
df_temp <-
  mutate(df, rank_S1 = ifelse(is.na(rank_S1), N_submission, rank_S1),
         rank_S2 = ifelse(is.na(rank_S2), N_submission, rank_S2),
         rank_S3 = ifelse(is.na(rank_S3), N_submission, rank_S3),
         rank_S4 = ifelse(is.na(rank_S4), N_submission, rank_S4),
         rank_S5 = ifelse(is.na(rank_S5), N_submission, rank_S5),
         id = as.double(id)) 

df_final_443 <- df_temp %>% 
  mutate(rank_avg = rowMeans(dplyr::select(df_temp, starts_with("rank_")), 
                                 na.rm = T)) %>% 
  arrange(rank_avg) %>% mutate(rank_final = dense_rank(rank_avg))

read_csv("./443_list.csv") %>% left_join(df_final_443, by = "id") %>% 
  write_csv("./443_result.csv")
