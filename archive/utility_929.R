setwd("~/../Desktop/Forecasting/929/")

library(purrr)
library(dplyr)
library(readr)
S1 <- readRDS("./S1_result.Rds")
S2 <- readRDS("./S2_result.Rds")
S3 <- readRDS("./S3_result.Rds")
S4 <- readRDS("./S4_result.Rds")
S5 <- readRDS("./S5_result.Rds")

df <- list(S1, S2, S3, S4, S5) %>% reduce(left_join, by = "id") 
df_final <- mutate(df, rank_avg = rowMeans(dplyr::select(df, starts_with("rank_")), na.rm = TRUE)) %>% 
  arrange(rank_avg) %>% mutate(rank_final = dense_rank(rank_avg), id = as.double(id))
  

read_csv("./929_list.csv") %>% left_join(df_final, by = "id") %>% write_csv("./929_result.csv")
