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
  start_date <- "2012-01-01"
  start_hour <- 0
  end_date <- "2012-12-31"
  end_hour <- 12
  #
  #
  ####
  
  # Generate header for UP.DAT file
  
#   UP.DAT          2.0             Header structure with coordinate parameters                     
#   1
#   Produced by READ62 Version: 5.54  Level: 070627                                 
#   NONE    
#   2004  336    1 2006   10    1 500.    2    2
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
  
  # Determine whether the selected time interval is available in the
  # 'sounding_list' list of data frames
  sounding_list_start_time <- 
    ISOdatetime(year = sounding_list[[1]][[1]][[4]],
                month = sounding_list[[1]][[1]][[3]],
                day = sounding_list[[1]][[1]][[2]],
                hour = sounding_list[[1]][[1]][[1]],
                min = 0, sec = 0, tz = "GMT")
  
  sounding_list_end_time <-
    ISOdatetime(year = sounding_list[[length(sounding_list)]][[1]][[4]],
                month = sounding_list[[length(sounding_list)]][[1]][[3]],
                day = sounding_list[[length(sounding_list)]][[1]][[2]],
                hour = sounding_list[[length(sounding_list)]][[1]][[1]],
                min = 0, sec = 0, tz = "GMT")
  
  
  # Close the function
}
