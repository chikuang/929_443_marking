# Rename student submitted csv to student_id.csv
names_old <- list.files()

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

names_new <- substrRight(names_old, 12)

file.rename(names_old, names_new)