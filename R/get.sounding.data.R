get.sounding.data <- function(start_date,
                              end_date,
                              hour_type = "0z,12z",
                              level_type = "all",
                              wind_units = "tenths_ms",
                              station_number = NULL,
                              station_wban_wmo = NULL,
                              output_file_path = "working",
                              output_file_name = "FSL-Sounding.txt",
                              details_in_file_name = TRUE
){
  
  # Include require statements
  require(RCurl)
  require(stringr)
    
  # Test parameter inputs
  #
  #
#   start_date <- "2013-01-01"
#   end_date <- "2013-12-31"
#   hour_type <- "all"
#   level_type <- "all"
#   wind_units <- "tenths_ms"
#   
#   # Request station by line number in station list
#   station_number <- 1000
#   
#   # Request station by WBAN-WMO pair
#   station_wban_wmo <- NULL
#   #station_list_wban_wmo <- "94151-71203"
#   
#   # Set the output file path
#   output_file_path <- "working"
#   #output_file_path <- "/Users/riannone/Dropbox/R Projects/"
#   
#   # Output file name
#   output_file_name <- "FSL-Sounding.txt"
#   
#   # Append details to file name
#   details_in_file_name <- TRUE
  #
  #
  ####
  
  # Get formatted beginning date
  bdate <- paste(str_replace_all(start_date, "-", ""), "00", sep = '') 
  
  # Get formatted ending date
  edate <- paste(str_replace_all(end_date, "-", ""), "23", sep = '') 
  
  # Get formatted 'shour' string
  if (hour_type == "all") shour <- "All+Times"
  
  # Get formatted 'ltype' string
  if (level_type == "all") ltype <- "All+Levels"
  
  # Get formatted 'wunits' string
  if (wind_units == "tenths_ms") wunits <- "Tenths+of+Meter%2FSecond" 
  
  # Resolve the output file path based on whether "working" is set (setting absolute path to
  # current working directory of the R process) and an absolute file path is specified
  resolved_output_file_path <- ifelse(output_file_path == "working",
                                      paste(getwd(), "/", sep = ''),
                                      output_file_path)
  
  # Combine resolved output file path with protocol
  output_file_path_with_protocol <- paste("file://", 
                                          resolved_output_file_path, 
                                          sep = '')
  
  # Get a current list of sounding stations
  df_soundings <- get.sounding.stations()
  
  # Get Station information
  if (!is.null(station_number) & is.null(station_wban_wmo)) {
    station_list_position <- station_number
  } else if (is.null(station_number) & !is.null(station_wban_wmo)) {
    wban_wmo_list <- as.data.frame(cbind(df_soundings$wban, df_soundings$wmo))
    wban_wmo_list$V3 <- do.call(paste, c(wban_wmo_list[c("V1", "V2")], sep = "-"))
    wban_wmo_list$V1 <- NULL
    wban_wmo_list$V2 <- NULL
    station_list_position <- match(station_wban_wmo,wban_wmo_list$V3)
    rm(wban_wmo_list)
  }
    
  # Construct 'station_list' string based on requested station
  station_list <- paste(df_soundings[station_list_position,1],
                        df_soundings[station_list_position,2],
                        df_soundings[station_list_position,3],
                        df_soundings[station_list_position,4],
                        df_soundings[station_list_position,5],
                        sprintf("%05s", df_soundings[station_list_position,6]),
                        str_replace_all(df_soundings[station_list_position,7], " ", "+"),
                        df_soundings[station_list_position,8],
                        df_soundings[station_list_position,9], sep = '+')
  
  # Construct request for data from NOAA
  noaa_cgi_message <- getURL(paste(
    "http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
    "bdate=", bdate,
    "&",
    "edate=", edate,
    "&",
    "access=All+Sites",
    "&",
    "view=NO",
    "&",
    "States=States",
    "&",
    "Countries=Countries",
    "&",
    "shour=", shour,
    "&",
    "ltype=", ltype,
    "&",
    "wunits=", wunits,
    "&",
    "stationlist=YES",
    "&",
    "station_list=", station_list,
    "&",
    "osort=Station+Series+Sort",
    "&",
    "oformat=FSL+format+%28ASCII+text%29", sep = ''))
  
  # Parse message and construct URI for data
  data_URI <- paste("http://www.esrl.noaa.gov/raobs/temp",
                    str_match(string = noaa_cgi_message,
                              pattern = "temp(.*)(tmp)")[1,2], "tmp", sep = '')
  
  # Get the data as a large character object
  sounding_data <- getURL(data_URI)
  
  # Append additional details to the output file name if request for such details is TRUE
  if (details_in_file_name == TRUE) {
    if (str_detect(output_file_name, fixed(".txt"))) {
      output_file_name <- str_replace(output_file_name, fixed(".txt"), "")
    }
    output_file_name <-
      paste(output_file_name, "__",
            "WBAN-", df_soundings[station_list_position,2], "_",
            "WMO-", df_soundings[station_list_position,3], "_",
            "Station-", strtrim(df_soundings[station_list_position,7], 10),
            ifelse(nchar(df_soundings[station_list_position,7]) > 10, "...", "_"),
            bdate, "-",
            edate,
            ".txt",
            sep = "")
  }
  
  # Write the data to the output file
  writeLines(sounding_data,
             con = paste(output_file_path_with_protocol,
                         output_file_name, sep = ''),
             sep = "\n")
  
  # Read back the file as lines
  sounding_data <- readLines(con = paste(output_file_path_with_protocol,
                                           output_file_name, sep = ''))
  
  # Return the 'sounding_data' object
  sounding_data
  
  # Assign object to global environment
  assign("sounding_data", sounding_data, envir = .GlobalEnv)
  
  # Close the function
}
