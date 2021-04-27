course_num <- readline(prompt="Enter course number: ")
my_dir <- paste0("~/../Desktop/Forecasting/", course_num)

setwd(my_dir)

library(purrr)
library(dplyr)
library(readr)
source("./load_S1.R")
source("./load_S2.R")
source("./load_S3.R")
source("./load_S4.R")
source("./load_S5.R")
setwd(my_dir)

df <- list(result_S1, result_S1, result_S1, result_S1, result_S1) %>% 
  reduce(left_join, by = "id")

N_submission <- nrow(df)
# If missing submission, then rank then the last in class
df_temp <-
  mutate(df, rank_S1 = ifelse(is.na(rank_S1), N_submission, rank_S1),
         rank_S2 = ifelse(is.na(rank_S2), N_submission, rank_S2),
         rank_S3 = ifelse(is.na(rank_S3), N_submission, rank_S3),
         rank_S4 = ifelse(is.na(rank_S4), N_submission, rank_S4),
         rank_S5 = ifelse(is.na(rank_S5), N_submission, rank_S5),
         id = as.double(id)) 

df_final <- df_temp %>% 
  mutate(rank_avg = rowMeans(dplyr::select(df_temp, starts_with("rank_")), 
                                 na.rm = T)) %>% 
  arrange(rank_avg) %>% mutate(rank_final = dense_rank(rank_avg))

read_csv(paste0("./",  course_num, "_list.csv")) %>% left_join(df_final, by = "id") %>% 
  write_csv(paste0("./", course_num, "_result.csv"))