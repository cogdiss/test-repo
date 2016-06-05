corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  filestoread <- list.files(directory)
  cor.vec <- vector()
  for (i in seq_along(filestoread)){
    path <- paste0(directory, "/", filestoread[i])
    tempdata <- read.csv(path)
    # Test threshold
    if (length(which(complete.cases(tempdata)==TRUE)) > threshold){
      cor.vec[i] <- cor(x=tempdata["sulfate"], y=tempdata["nitrate"], 
			use="complete.obs")
    }
  }
  return(na.omit(cor.vec))
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


