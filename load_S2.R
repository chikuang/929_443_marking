rm(list=ls())
setwd("~/../Desktop/Forecasting/929/scenario2/")
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
n_forecast <- 10*40
student_id <- substr(my_files, 1, 8)

Nfile <- 40
true_val <- lapply(1:Nfile, function(i){
  read_csv(file = paste0("~/../Desktop//Forecasting/solution/stock_true/stock_true", i, ".txt"),
                         col_types = cols(.default = col_double())) %>%
   rename(id = X1, true_val = x) %>% mutate(stock_id = i)
}) %>% bind_rows() %>% pull(true_val)


df_submissions <- lapply(seq_along(student_id), function(k){
  val <- read_csv(file = my_files[k],
           col_names = paste0("stock_", 1:40),
           col_types = cols(.default = col_double())) %>% unlist()
  df_temp <- tibble(pred_val= val) 
  names(df_temp) <- student_id[k]
  df_temp
}) %>% rlist::list.cbind() %>% t()


evaluation <- rep(NA, n_submission)
for(i in 1:n_submission){
  evaluation[i] <- mean((df_submissions[i, ] - true_val)*(ifelse(df_submissions[i, ] - true_val<=0, 1, 0) - 0.15))
}

# Save all forecasts into a csv  ------------------------------------------
colnames(df_submissions) <- paste0("V", 1:n_forecast)
df_S2 <- cbind(student_id, df_submissions) %>% as_tibble() %>%
  rename(id = student_id) %>% mutate(id = as.double(id))
read_csv("~/../Desktop/Forecasting/929/929_list.csv") %>% left_join(df_S2, by = "id") %>% 
  write_csv("~/../Desktop/Forecasting/929/929_pred_S2.csv")

result_S2 <- tibble(id = student_id, MSE_S2 = evaluation) %>% 
  mutate(rank_S2 = dense_rank(MSE_S2)) %>% arrange(rank_S2)
saveRDS(result_S2, "~./../Desktop/Forecasting/929/S2_result.Rds")