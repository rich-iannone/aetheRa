#' Export a CALMET UP.DAT file
#' @description Using upper air sounding data, create an UP.DAT input file for the CALMET model.
#' @param processed_sounding_data the return object from the `get_sounding_data` function.
#' @param export_all_times providing TRUE for this will create an UP.DAT file with time bounds matching those in the 'sounding_list' list object (created after use of the 'process.sounding.data' function). The default is set to FALSE, in which case values for the arguments 'start_date', 'start_hour', 'end_date', and 'end_hour' must be supplied.
#' @param start_date a starting date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".
#' @param start_hour a starting hour must accompany the entry for 'start_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'start_hour' is numeric.
#' @param end_date an ending date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".
#' @param end_hour an ending hour must accompany the entry for 'end_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'end_hour' is numeric.
#' @param top_pressure_level the top pressure level to which sounding data should be constrained. A numeric value, representing atmospheric pressure in hPa units, should be supplied.
#' @export export_data_to_CALMET
#' @examples
#' \dontrun{
#' # After generating the 'sounding_list' object (from use of the 'process.sounding.data' function),
#' # generate a CALMET UP.DAT file for 2013, constraining the output to the 500 hPa pressure level
#' export_data_to_CALMET(processed_sounding_data = the_processed_sounding_data,
#'                       export_all_times = FALSE
#'                       start_date = "2013-01-01"
#'                       start_hour = 0
#'                       end_date = "2013-12-31"
#'                       end_hour = 0
#'                       top_pressure_level = 500)
#'}

export_data_to_CALMET <- function(processed_sounding_data,
                                  export_all_times = FALSE,
                                  start_date,
                                  start_hour,
                                  end_date,
                                  end_hour,
                                  top_pressure_level){
  
  # Include require statements
  require(RCurl)
  require(stringr)
  require(lubridate)
      
  # Generate requested start and end POSIXct time objects
  req_start_date_time <- as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT") +
    (start_hour * 3600)
  
  req_end_date_time <- as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT") +
    (end_hour * 3600)
  
  # Generate header for UP.DAT file
  header_1 <- "UP.DAT          2.0             Header structure with coordinate parameters"
  header_2 <- "1"
  header_3 <- "Produced using R"
  header_4 <- "NONE"
  header_5 <- paste(year(as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT")),
                    "  ",
                    yday(as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT")),
                    "    ",
                    "1 ",
                    year(as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT")),
                    "    ",
                    yday(as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT")),
                    "    ",
                    "1 ",
                    top_pressure_level, ".",
                    "    2    2",
                    sep = '')
  header_6 <- "F    F    F    F"
  
  # Remove objects from global environment
  rm(start_date, start_hour, end_date, end_hour)
  
  # Determine whether the selected time interval is available in the
  # 'sounding_list' list of data frames; if not, stop function with message
  sounding_list_start_date_time <- 
    ISOdatetime(year = sounding_list[[1]][[1]][[4]],
                month = sounding_list[[1]][[1]][[3]],
                day = sounding_list[[1]][[1]][[2]],
                hour = sounding_list[[1]][[1]][[1]],
                min = 0, sec = 0, tz = "GMT")
  
  sounding_list_end_date_time <-
    ISOdatetime(year = sounding_list[[length(sounding_list)]][[1]][[4]],
                month = sounding_list[[length(sounding_list)]][[1]][[3]],
                day = sounding_list[[length(sounding_list)]][[1]][[2]],
                hour = sounding_list[[length(sounding_list)]][[1]][[1]],
                min = 0, sec = 0, tz = "GMT")
  
  if (req_start_date_time < sounding_list_start_date_time |
        req_end_date_time > sounding_list_end_date_time  ) {
    stop("Requested time frame for data is not entirely available in processed dataset.")
  }
  
  ####
  # Subset the list object
  ####
  
  # Find how many values of the list need to be trimmed from the beginning then
  # trim those list items and save as a new list object
  for (i in 1:length(sounding_list)) {
    if (i == 1) above_req_date_time <- mat.or.vec(nr = length(sounding_list), nc = 1)
    above_req_date_time[i] <- 
      ISOdatetime(year = sounding_list[[i]][[1]][[4]],
                  month = sounding_list[[i]][[1]][[3]],
                  day = sounding_list[[i]][[1]][[2]],
                  hour = sounding_list[[i]][[1]][[1]],
                  min = 0, sec = 0, tz = "GMT") >= req_start_date_time
    trim_number_from_left <- length(above_req_date_time) -
      sum(above_req_date_time, na.rm = TRUE)   
  }

  # Find how many values of the list need to be trimmed from the end   
  for (i in 1:length(sounding_list)) {
    if (i == 1) below_req_date_time <- mat.or.vec(nr = length(sounding_list), nc = 1)
    below_req_date_time[i] <- 
      ISOdatetime(year = sounding_list[[i]][[1]][[4]],
                  month = sounding_list[[i]][[1]][[3]],
                  day = sounding_list[[i]][[1]][[2]],
                  hour = sounding_list[[i]][[1]][[1]],
                  min = 0, sec = 0, tz = "GMT") <= req_end_date_time
    trim_number_from_right <- length(below_req_date_time) -
      sum(below_req_date_time, na.rm = TRUE)
  }
  
  # Make copy of sounding_list before trimming it
  trimmed_sounding_list <- sounding_list
  
  # Trim the 'trimmed_sounding_list' object at the beginning
  if (trim_number_from_left > 0) {
    for (i in 1:trim_number_from_left) {
      trimmed_sounding_list[1] <- NULL
    }
  }
  
  # Trim the 'trimmed_sounding_list' object at the end
  if (trim_number_from_right > 0) {
    for (i in 1:trim_number_from_right) {
      trimmed_sounding_list[length(trimmed_sounding_list)] <- NULL
    }
  }
  
  # Remove objects from global environment
  rm(req_start_date_time, req_end_date_time,
     sounding_list_start_date_time, sounding_list_end_date_time,
    trim_number_from_left, trim_number_from_right,
     above_req_date_time, below_req_date_time)
  
  # Construct the first portion of header line, which is constant throughout
  header_line_constant <- paste("   6201     94240   ")
  
  #
  # Loop through the list, outputting lines at 0z and at 12z
  # Check that the next item in the list is within 10-14 h of the previous
  # If there is no list item available, then get the sounding data from the
  # previous period ~24 h earlier
  #
  
  # Generate a file for writing
  cat(file = "test_output.txt")
  
  # Add header to top of output file
  cat(header_1, header_2, header_3, header_4, header_5, header_6,
      sep = "\n", file = "test_output.txt", append = TRUE)
  
  # Remove objects from global environment
  rm(header_1, header_2, header_3, header_4, header_5, header_6)
  
  # Start loop for header line
  for (i in 1:length(trimmed_sounding_list)) {
    header_line <- 
      paste(header_line_constant,
            trimmed_sounding_list[[i]][[1]][[4]],
            formatC(trimmed_sounding_list[[i]][[1]][[3]], # month
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[2]], # day
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[1]], # hour
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[14]] - 3, # lines
                    width = 7, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[14]] - 3, # lines
                    width = 33, flag = " "),
            sep = '')
    
    # Write header_line to file
    cat(header_line, sep = "\n", file = "test_output.txt", append = TRUE)
    
    # Start loop for data lines
    for (j in 1:nrow(trimmed_sounding_list[[i]][[2]])) {
      if (j == 1) data_line <- mat.or.vec(nr = nrow(trimmed_sounding_list[[i]][[2]]),
                                          nc = 1) 
      data_line[j] <-
        paste(formatC(trimmed_sounding_list[[i]][[2]][[j, 2]], # pressure
                      width = 9, format = "f", digits = 1, flag = " "),
              ",",
              formatC(trimmed_sounding_list[[i]][[2]][[j, 3]], # height
                      width = 6, format = "f", digits = 0, flag = " "),
              ",",
              ifelse(trimmed_sounding_list[[i]][[2]][[j, 4]] > 900, 
                     formatC(999.9, width = 5, format = "f",
                             digits = 1, flag = " "),
                     formatC(trimmed_sounding_list[[i]][[2]][[j, 4]] + 273, # temp
                             width = 2, format = "f", digits = 1, flag = " ")),
              ",",
              ifelse(trimmed_sounding_list[[i]][[2]][[j, 6]] > 900,
                     formatC(999, width = 3, format = "f",
                             digits = 0, flag = " "),
                     formatC(trimmed_sounding_list[[i]][[2]][[j, 6]], # WD
                             width = 3, format = "f", digits = 0, flag = " ")),
              ",",
              ifelse(trimmed_sounding_list[[i]][[2]][[j, 7]] > 900,
                     formatC(999.9, width = 5, format = "f",
                             digits = 1, flag = " "),                
                     formatC(trimmed_sounding_list[[i]][[2]][[j, 7]], # WS
                             width = 5, format = "f", 
                             digits = 1, flag = " ")),
              ifelse(j == nrow(trimmed_sounding_list[[i]][[2]]), "", ","),
              sep = '')
           
      # Close loop for data lines
    }
    
    # Scan for entries with negative height and delete such records
    for (k in 1:length(data_line)) {
      if (trimmed_sounding_list[[i]][[2]][[k, 3]] < 0) data_line <- data_line[-k]
    }
    
    # Write data lines to file
    cat(data_line, sep = "\n", file = "test_output.txt", append = TRUE)

    # Close loop for header line
  }
  
  # Remove objects from global environment
  rm(header_line_constant, header_line, data_line,
     export_all_times, top_pressure_level, i, j, k)
  
  # Add notification that UP.DAT file generated and placed in the working folder
  print(paste("An UP.DAT file was generated and placed in the working folder"))
  
}
