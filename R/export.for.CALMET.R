export.for.CALMET <- function(sounding_list = sounding_list,
                              export_all = FALSE,
                              start_date,
                              start_hour,
                              end_date,
                              end_hour){
  
  # Include require statements
  require(RCurl)
  require(stringr)
  
  # Check for existance of 'sounding_list' object, created by the
  # 'process.sounding.data' function; if the object doesn't exist, stop the function
  # with a message
  if (!exists("sounding_data")) {
    stop("Processed sounding data is not available. Use the 'process.sounding.data' function")
  }
  
  # Generate header for UP.DAT file
  
#   UP.DAT          2.0             Header structure with coordinate parameters                     
#   1
#   Produced by READ62 Version: 5.54  Level: 070627                                 
#   NONE    
#   2004  336    1 2006   10    1 500.    2    2
#   T    F    F    F
#   6201     94240   200412 112     67                               39
  
  header_1 <- "UP.DAT          2.0             Header structure with coordinate parameters"
  header_2 <- "1"
  header_3 <- "Produced using R"
  header_4 <- "NONE"
  
  
  # Close the function
}
