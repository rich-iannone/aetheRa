select.sounding.station <- function(df_soundings = df_soundings,
                                    id_by_wban_wmo = NULL,
                                    search_station_name = NULL,
                                    search_init = NULL,
                                    search_wban = NULL,
                                    search_wmo = NULL,
                                    search_prov_state = NULL,
                                    search_country = NULL,
                                    lower_lat = NULL,
                                    upper_lat = NULL,
                                    lower_long = NULL,
                                    upper_long = NULL,
                                    lower_elev = NULL,
                                    upper_elev = NULL
){

  # Test parameter inputs
  #
  #
  
#   #id_by_wban_wmo <- NULL
#   
#   #search_station_name = "the pas"
#   
#   search_init = NULL
#   search_wban = "21603"
#   search_wmo = NULL
#   
#   search_prov_state = NULL
#   search_country = NULL
#   
#   lower_lat = NULL
#   upper_lat = NULL
#   lower_long = NULL
#   upper_long = NULL
#   
#   lower_elev = NULL
#   upper_elev = NULL
  
  #
  #
  ####
  
  #####
  # Search the data frame using the search parameters
  #####
  
  # Check that only one of the search parameters of 'search_init', 'search_wban',
  # or 'search_wmo' are used; otherwise stop function and advise to only one of these
  if (!is.null(search_init) & !is.null(search_wban)) {
    stop("Only use one of 'search_init', 'search_wban', or 'search_wmo' search parameters")
  }
  
  if (!is.null(search_wban) & !is.null(search_wmo)) {
    stop("Only use one of 'search_init', 'search_wban', or 'search_wmo' search parameters")
  }
  
  if (!is.null(search_wmo) & !is.null(search_init)) {
    stop("Only use one of 'search_init', 'search_wban', or 'search_wmo' search parameters")
  }
  
  # If a search by 'init' is requested, subset the stations data frame
  if (!is.null(search_init)) df_soundings.subset <- subset(df_soundings,
                                                           df_soundings$init == search_init)
  if (!is.null(search_init) &
        exists("df_soundings.subset") & 
        nrow(df_soundings.subset) == 1) target_station <- df_soundings.subset
  if (exists("target_station")) {
    assign("target_station", target_station, envir = .GlobalEnv)
    return(paste("The following station was identified and set as the target station: wmo ",
                 target_station$wmo, ", wban ", target_station$wban, " (",
                 target_station$station_name, ")", sep = ''))
  }
  
  # If a search by 'wban' is requested, subset the stations data frame
  if (!is.null(search_wban)) df_soundings.subset <- subset(df_soundings,
                                                           df_soundings$wban == search_wban)
  if (!is.null(search_wban) &
        exists("df_soundings.subset") & 
        nrow(df_soundings.subset) == 1) target_station <- df_soundings.subset
  if (exists("target_station")) {
    assign("target_station", target_station, envir = .GlobalEnv)
    return(paste("The following station was identified and set as the target station: wmo ",
               target_station$wmo, ", wban ", target_station$wban, " (",
               target_station$station_name, ")", sep = ''))
  }
 
  # Close the function
}


