#' Get sounding data
#' @description Get upper air sounding data as a text file in FSL format.
#' @param stations_df a data frame containing identification and location parameters for sounding stations. 
#' @param start_date the starting date of the sounding data request in the format "YYYY-MM-DD". The earliest sounding for the specified date will be included in the request.
#' @param end_date the end date of the sounding data request in the format "YYYY-MM-DD". The last sounding for the specified date will be included in the request.
#' @param hour_type the types of hours to be supplied in the returned dataset. The default is "0z,12z", which includes sounding data from launches near 12:00 UTC and those near 24:00 UTC. The option "all" provides all of the soundings available from the dataset, and the options "12z" and "0z" provide data from those soundings launched near 12:00 UTC or 24:00 UTC, respectively.
#' @param level_type the types of levels to be supplied in the returned dataset. The default is "all", which includes all sounding types. Other options are "mandatory" (includes only the mandatory-type levels) and "mandatory_and_significant" (includes only the mandatory- and significant-type levels).
#' @param wind_units the desired wind units for the sounding data request. Options are tenths of meters per second ("tenths_ms", the default) and "knots".
#' @param station_number the selection of the desired sounding site can be performed by identifying a line number from the dataframe supplied by the 'get.sounding.stations' function.
#' @param station_wban_wmo the selection of the desired sounding site for the data request can be achieved by supplying WBAN and WMO numbers in the form of "XXXXX-YYYYY" (WBAN first, WMO second).
#' @param output_file_path the output file path for the retrieved sounding data can be specified here by using "working", which sets the path to the current working directory, or, by specifying an absolute path.
#' @param output_file_name the desired filename for the retrieved sounding data, including an extension such as ".txt" or ".dat". The default is "FSL-Sounding.txt", so, not modifying this argument over several uses (especially in conjunction with a "FALSE" setting for the 'details_in_file_name' argument) may result in overwriting previously generated files.
#' @param details_in_file_name incorporates details about the sounding data into the filename. Set to "TRUE" by default to provide self-describing filenames and reducing the risk of overwriting files. Setting to "FALSE" strictly uses the filename specified in the 'output_file_name' argument.
#' @export get_sounding_data
#' @examples
#' \dontrun{
#' # Get all FSL sounding data for the station with WBAN 99999 and
#  # WMO 01001 (Jan Mayen) for the year 2012
#' sounding_data <- get_sounding_data(start_date = "2012-01-01",
#'                                    end_date = "2012-12-31",
#'                                    hour_type = "all",
#'                                    level_type = "all",
#'                                    wind_units = "tenths_ms",
#'                                    station_wban_wmo = "99999-01001",
#'                                    output_file_path = "working",
#'                                    output_file_name = "FSL-Sounding.txt",
#'                                    details_in_file_name = TRUE)
#'}

get_sounding_data <- function(stations_df = NULL,
                              start_date,
                              end_date,
                              hour_type = "0z,12z",
                              level_type = "all",
                              wind_units = "tenths_ms",
                              station_number = NULL,
                              station_wban_wmo = NULL,
                              output_file_path = "working",
                              output_file_name = "FSL-Sounding.txt",
                              details_in_file_name = TRUE){
  
  # Include require statements
  require(RCurl)
  require(stringr)
    
  # If 'stations_df' supplied, choose only the first row of it by default
  if (!is.null("stations_df")){
    stations_df <- stations_df[1,]
  }
  
  # Get formatted beginning date
  bdate <- paste(str_replace_all(start_date, "-", ""), "00", sep = '') 
  
  # Get formatted ending date
  edate <- paste(str_replace_all(end_date, "-", ""), "23", sep = '') 
  
  # Get formatted 'shour' string
  if (hour_type == "all") shour <- "All+Times"
  if (hour_type == "0z") shour <- "0z+ONLY"
  if (hour_type == "12z") shour <- "12z+ONLY"
  if (hour_type == "0z,12z") shour <- "0z%2C+12z+ONLY"
  
  # Get formatted 'ltype' string
  if (level_type == "all") ltype <- "All+Levels"
  if (level_type == "mandatory") ltype <- "Mandatory"
  if (level_type == "mandatory_and_significant") ltype <- "Mand+%26+Sigs"
  
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
  if (is.null(station_number) & is.null(station_wban_wmo)) {
    if (exists("target_station")) station_list_position <- as.numeric(row.names(target_station))
  } else if (!is.null(station_number) & is.null(station_wban_wmo)) {
    # If a 'target_station' was set using the 'select.sounding.station' function,
    # get the 'station_list_position' value from that
    if (exists("target_station")) station_list_position <- as.numeric(row.names(target_station))
    # If no 'target_station' set, defer to using the 'station_number' value
    if (!exists("target_station")) station_list_position <- station_number
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
  return(sounding_data)
  
  # Assign object to global environment
  assign("sounding_data", sounding_data, envir = .GlobalEnv)
  
  # Add notification that data was downloaded and assigned to the global workspace
  if (exists("sounding_data")) {
    print(paste("Sounding data was downloaded and assigned as object 'sounding_data'."))
  }
  
}
