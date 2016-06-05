complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  complete.table <- data.frame()
  for (i in seq_along(id)){
    path <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv")
    tempdata <- read.csv(path)
    tempdata <- na.omit(tempdata)
    tempdata <- c(id[i], nrow(tempdata))
    complete.table <- rbind(complete.table, tempdata)
  }
  colnames(complete.table) <- c("id", "nobs")
  return(complete.table)
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

