export.for.CALMET <- function(sounding_list = sounding_list,
                              export_all_times = FALSE,
                              start_date,
                              start_hour,
                              end_date,
                              end_hour){
  
  # Include require statements
  require(RCurl)
  require(stringr)
  require(lubridate)
  
  # Check for existance of 'sounding_list' object, created by the
  # 'process.sounding.data' function; if the object doesn't exist, stop the function
  # with a message
  if (!exists("sounding_list")) {
    stop("Processed sounding data is not available. Use the 'process.sounding.data' function")
  }
  
  # Test parameter inputs
  #
  #
  export_all_times <- FALSE
  start_date <- "2013-01-01"
  start_hour <- 0
  end_date <- "2013-12-31"
  end_hour <- 0
  #
  #
  ####
  
  # Generate requested start and end POSIXct time objects
  req_start_date_time <- as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT") +
    (start_hour * 3600)
  
  req_end_date_time <- as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT") +
    (end_hour * 3600)
  
  # Generate header for UP.DAT file
  
#   UP.DAT          2.0             Header structure with coordinate parameters                     
#   1
#   Produced by READ62 Version: 5.54  Level: 070627                                 
#   NONE    
#   2004  336    1 2006   10    1 500.    2    2
#     (1) start year
#     (2) starting julian day
#     (3) starting hour
#     (4) ending year
#     (5) ending julian day
#     (6) ending hour
#     (7) top pressure level (850 mb, 700 mb, or 500 mb)
#     (8) input file format: 1 is TD-6201 format, 2 is NCDC CD-ROM format
#     (9) delimiter used in UP.DAT file
#   T    F    F    F
  
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
                    "500.",
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
  
  # Subset the list object
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
  
  
  header_line_constant <- paste("   6201     94240   ")
  
  # Loop through the list, outputting lines at 0z and at 12z
  # Check that the next item in the list is within 10-14 h of the previous
  # If there is no list item available, then get the sounding data from the
  # previous period ~24 h earlier
  
  # Start loop for header line
  
  for (i in 1:length(trimmed_sounding_list)) {
    header_line <- 
      paste(header_line_constant,
            trimmed_sounding_list[[i]][[1]][[4]],
            formatC(trimmed_sounding_list[[i]][[1]][[3]], # month
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[3]], # day
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[2]], # hour
                    width = 2, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[14]] - 3, # lines
                    width = 7, flag = " "),
            formatC(trimmed_sounding_list[[i]][[1]][[14]] - 3, # lines
                    width = 33, flag = " "),
            sep = '')
    
    # Start loop for data lines
    for (j in 1:nrow(trimmed_sounding_list[[i]][[2]])) {
      
      
      
      
    # Close loop for data lines
    }
    
    
    # Close loop for header line
  }
  
  # Close the function
}
