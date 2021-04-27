rm(list=ls())
setwd("~/../Desktop/Forecasting/929/scenario5/")
# not run
# source("rename_to_id.R")


# load each files ---------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(pbapply)
library(rlist)
my_files <- list.files()
n_forecast <- 3*336
n_submission <- length(my_files)
student_id <- substr(my_files, 1, 8)


true_val <- 
  lapply(1:3, function(i){
    read_csv(file = paste0("~/../Desktop/Forecasting/solution/pollutionCity", i, "true.txt"),
             col_types = cols(.default = col_double())) %>%
      rename(id = X1, true_val= x) %>% mutate(city_num = i)
  }) %>% bind_rows() %>% pull(true_val)


df_submissions <- lapply(seq_along(student_id), function(k){
  val <- read_csv(file = my_files[k], col_names = FALSE,  col_types= cols(.default = col_double())) %>% 
    unlist()
  df_temp <- tibble(pred_val= val) 
  names(df_temp) <- student_id[k]
  df_temp
}) %>% rlist::list.cbind() %>% t()

evaluation <- rep(NA, n_submission)
for(i in 1:n_submission){
  evaluation[i] <- sum((df_submissions[i, ] - true_val)^2)
}


colnames(df_submissions) <- paste0("V", 1:n_forecast)
df_S5 <- cbind(student_id, df_submissions) %>% as_tibble() %>%
  rename(id = student_id) %>% mutate(id = as.double(id))
read_csv("~/../Desktop/Forecasting/929/929_list.csv") %>% left_join(df_S5, by = "id") %>% 
  write_csv("~/../Desktop/Forecasting/929/929_pred_S5.csv")

result_S5 <- tibble(id = student_id, MSE_S5 = evaluation) %>% 
  mutate(rank_S5 = dense_rank(MSE_S5)) %>% arrange(rank_S5)
saveRDS(result_S5, "~./../Desktop/Forecasting/929/S5_result.Rds")