#' Find which soundings are missing from the processed sounding data
#' @description Find out where expected upper air sounding data are missing.
#' @param processed_sounding_data a list object containing processed sounding data as produced from the 'process_sounding_data' function.
#' @export find_missing_processed_data

find_missing_processed_data <- function(processed_sounding_data){
  
  # Create a data frame with sequential timestamps
  for (i in 1:length(processed_sounding_data)){
    if (i == 1){
      times <- as.data.frame(mat.or.vec(nr = length(processed_sounding_data), nc = 2))
      colnames(times) <- c("number", "timestamp")
    }
    
    times[i, 1] <- i
    times[i, 2] <- ISOdatetime(processed_sounding_data[i][[1]][[1]]$year,
                               processed_sounding_data[i][[1]][[1]]$month,
                               processed_sounding_data[i][[1]][[1]]$day,
                               processed_sounding_data[i][[1]][[1]]$hour,
                               min = 0, sec = 0, tz = 'GMT')
    if (i == length(processed_sounding_data)){
      times[, 2] <- as.POSIXct(times[, 2], origin = "1970-01-01", tz = "GMT")
    }
  }
  
  # Check for completeness of the dataset by obtaining a vector of time intervals
  for (i in 1:nrow(times)){
    if (i == 1) intervals <- vector(mode = "numeric", length = 0)
    if (i == nrow(times)){break} 
    
    an_interval <- (times[(i + 1),2] - times[i,2])[[1]]
    intervals <- c(intervals, an_interval)
    
  }
  
  # If all of the time intervals are between 10-14 hours, then there are no
  # missing data
  if (all(intervals > 10 & intervals < 14) == TRUE){
    missing_indices <- NA
  }
  
  # Obtain a data frame containing information on indices where the time intervals
  # are 14 hours or greater and the interval value itself
  if (length(which(intervals >= 14)) > 0){
    
    # Obtain the indices for which the time intervals are >= 14 hours
    high_interval_indices <- which(intervals >= 14)
    
    for (i in 1:length(high_interval_indices)){
      
      if (i == 1){
        missing_info <- as.data.frame(mat.or.vec(nr = length(high_interval_indices), nc = 2))
        colnames(missing_info) <- c("index", "interval_hours")
      }
      
      missing_info[i,1] <- high_interval_indices[i]
      missing_info[i,2] <- intervals[high_interval_indices[i]]
      
    } 
  }
  
  return(missing_info)
  
}
