rm(list=ls())
setwd("~/../Desktop/Forecasting/929/scenario1/")
# names_old <- list.files()
# 
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# 
# names_new <- substrRight(names_old, 12)
# 
# file.rename(names_old, names_new)


# load each files ---------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(pbapply)
library(rlist)
my_files <- list.files()
n_submission <- length(my_files)
n_forecast <- 24
student_id <- substr(my_files, 1, 8)


true_val <- read_csv(file = "~/../Desktop/Forecasting/solution/hyd_true.txt",
                   col_types = cols(.default = col_double())) %>% 
  rename(id = X1, true_val = x) %>% 
  slice_tail(n = n_forecast) %>% 
  pull(true_val)

df_submissions <- lapply(seq_along(student_id), function(k){
  read_csv(file = my_files[k],
           col_names = student_id[k],
           col_types = cols())
}) %>% rlist::list.cbind() %>% t()

evaluation <- rep(NA, n_submission)
for(i in 1:n_submission){
  evaluation[i] <- sum((df_submissions[i, ] - true_val)^2)
}


# Save all forecasts into a csv  ------------------------------------------
colnames(df_submissions) <- paste0("V", 1:n_forecast)
df_S1 <- cbind(student_id, df_submissions) %>% as_tibble() %>%
  rename(id = student_id) %>% mutate(id = as.double(id))
read_csv("~/../Desktop/Forecasting/929/929_list.csv") %>% left_join(df_S1, by = "id") %>% 
  write_csv("~/../Desktop/Forecasting/929/929_pred_S1.csv")


# Save the ranking --------------------------------------------------------
result_S1 <- tibble(id = student_id, MSE_S1 = evaluation) %>% 
  mutate(rank_S1 = dense_rank(MSE_S1)) %>% arrange(rank_S1)

saveRDS(result_S1, "~./../Desktop/Forecasting/929/S1_result.Rds")