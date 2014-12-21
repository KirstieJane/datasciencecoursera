corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Set up a vector for all the files you're going to run through
  id <- 1:332
  
  # Set up a cr vector
  cr <- numeric()
  
  # Now loop through all the remaining ids
  for (i in id) {
    
    # Create the filename and full path
    filename <- paste(sprintf("%03d", i), ".csv", sep='')
    full_filename <- file.path(directory, filename)
      
    # Read in the data from that first file
    df <- read.csv(full_filename)
    
    summary(df)
    # Only keep lines that are complete cases
    df <- df[complete.cases(df),]
    
    if (nrow(df) > threshold) {
      
        r <- cor(df['sulfate'], df['nitrate'])
        cr <- c(cr, r)
    
    }
    
    
  }
  
  # And return the cr list
  return(cr)
}