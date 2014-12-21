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
  
  # Set up an empty data frame
  df <- data.frame(id = numeric(length(id)),
                   nobs = numeric(length(id)))
  
  # Loop through all the ids
  for (i in 1:length(id)) {
      
    # Create the filename and full path
    filename <- paste(sprintf("%03d", id[i]), ".csv", sep='')
    full_filename <- file.path(directory, filename)
    
    # Read in the data from that first file
    df_temp <- read.csv(full_filename)

    # Create a logicalvector indicating which
    # cases are complete
    ok <- complete.cases(df_temp)
    
    # Fill in the data frame
    df[i, "id"] <- id[i]
    df[i, "nobs"] <- sum(ok)
        
  }
  
  return(df)
}