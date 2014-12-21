pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Define the filename for the very first file (id[1])
  filename <- paste(sprintf("%03d", id[1]), ".csv", sep='')
  # Make that file name the full path
  full_filename <- file.path(directory, filename)
  
  # Read in the data from that first file
  df <- read.csv(full_filename)

  # Mask all NA values
  mask <- is.na(df[pollutant])
  
  # and only keep ones that are not NA
  df <- df[!mask,]
  
  # Now loop through all the remaining ids
  for (i in id[-1]) {
    
    # Create the filename and full path
    filename <- paste(sprintf("%03d", i), ".csv", sep='')
    full_filename <- file.path(directory, filename)
    
    # Read in the data, mask and only keep non-NAs
    df_temp <- read.csv(full_filename)
    mask <- is.na(df_temp[pollutant])
    df_temp <- df_temp[!mask,]
    
    # Merge together this data with all the ones
    # that have come before
    df <- rbind(df, df_temp)
        
  }
  
  # Calculate the mean of the pollutant column
  means <- colMeans(df[pollutant], na.rm = TRUE)
  
  # And return that value
  return(means[[1]])
}

