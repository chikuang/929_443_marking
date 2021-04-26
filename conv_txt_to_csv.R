filelist <- list.files(pattern = ".txt")
someVar <- lapply(filelist, function(x) { 
  textfile <- read.table(x)
  write.csv(textfile, 
            file = sub(pattern = "\\.txt$", replacement = ".csv", x = x))
})
