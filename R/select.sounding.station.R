select.sounding.station <- function(id_by_wban_wmo = NULL,
                                    search_station_name = NULL,
                                    search_init = NULL,
                                    search_wban = NULL,
                                    search_wmo = NULL,
                                    search_prov_state = NULL,
                                    search_country = NULL,
                                    lower_lat = NULL,
                                    upper_lat = NULL,
                                    lower_lon = NULL,
                                    upper_lon = NULL,
                                    lower_elev = NULL,
                                    upper_elev = NULL
){
  
  #####
  # Select a station using the combination of WBAN and WMO numbers
  ##### 
  
  # Stop the function if the supplied search string for 'id_by_wban_wmo' is not
  # properly formed
  if (!is.null(id_by_wban_wmo)) {
    if (regexpr("^[0-9]+?-[0-9]+?$", id_by_wban_wmo)[1] != 1) {
      stop("Use a string in the form of 'XXXXX-YYYYY' in the order of WBAN and WMO")
    }
  }
  
  # Initial subset
  if (!is.null(id_by_wban_wmo)) {
    df_soundings.subset <- subset(df_soundings,
                                  df_soundings$wban ==
                                    strsplit(id_by_wban_wmo, "-")[[1]][[1]] &
                                    df_soundings$wmo ==
                                    strsplit(id_by_wban_wmo, "-")[[1]][[2]])
  }
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(id_by_wban_wmo) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified."))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(id_by_wban_wmo) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The target station is now available with the following ",
                   "identifiers: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
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
  
  
  #####
  # If a search by 'init' is requested, subset the stations data frame
  ####
  
  # Initial subset
  if (!is.null(search_init)) df_soundings.subset <- subset(df_soundings,
                                                           df_soundings$init == search_init)
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(search_init) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(search_init) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The following station was identified and set ",
                   "as the target station: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(search_init) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
  
  #####
  # If a search by 'wban' is requested, subset the stations data frame
  ####
  
  # Initial subset
  if (!is.null(search_wban)) df_soundings.subset <- subset(df_soundings,
                                                           df_soundings$wban == search_wban)
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(search_wban) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(search_wban) &
        exists("df_soundings.subset")) {
        if (nrow(df_soundings.subset) == 1) {
          assign("target_station", df_soundings.subset, envir = .GlobalEnv)
          return(paste("The following station was identified and set ",
                       "as the target station: wmo ",
                       target_station$wmo, ", wban ", target_station$wban,
                       " (", target_station$station_name, ")", sep = ''))
        }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(search_wban) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
 
  #####
  # If a search by 'wmo' is requested, subset the stations data frame
  ####
  
  # Initial subset
  if (!is.null(search_wmo)) df_soundings.subset <- subset(df_soundings,
                                                           df_soundings$wmo == search_wmo)
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(search_wmo) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(search_wmo) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The following station was identified and set ",
                   "as the target station: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(search_wmo) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
  
  #####
  # If a search by 'prov/state' and/or 'country' is requested, subset the
  # stations data frame
  ####
  
  # Initial subset
  if (!is.null(search_prov_state) & is.null(search_country)) {
    df_soundings.subset <- subset(df_soundings, df_soundings$prov_state == search_prov_state)
  } else if (is.null(search_prov_state) & !is.null(search_country)) {
    df_soundings.subset <- subset(df_soundings, df_soundings$country == search_country)
  } else if (!is.null(search_prov_state) & !is.null(search_country)) {
    df_soundings.subset <- subset(df_soundings,
                                  df_soundings$prov_state == search_prov_state &
                                    df_soundings$country == search_country)
  }
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(search_prov_state) | !is.null(search_country) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(search_prov_state) | !is.null(search_country) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The following station was identified and set ",
                   "as the target station: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(search_prov_state) | !is.null(search_country) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
  
  #####
  # If a search by bounding box is requested, subset the stations data frame
  ####
  
  # Initial subset
  if (!is.null(lower_lat) & !is.null(upper_lat) &
        !is.null(lower_lon) & !is.null(upper_lon)) {
    df_soundings.subset <- subset(df_soundings,
                                  df_soundings$lat >= lower_lat &
                                    df_soundings$lat <= upper_lat &
                                    df_soundings$lon >= lower_lon &
                                    df_soundings$lon <= upper_lon)
  }
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(lower_lat) & !is.null(upper_lat) &
        !is.null(lower_lon) & !is.null(upper_lon) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(lower_lat) & !is.null(upper_lat) &
        !is.null(lower_lon) & !is.null(upper_lon) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The following station was identified and set ",
                   "as the target station: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(lower_lat) & !is.null(upper_lat) &
        !is.null(lower_lon) & !is.null(upper_lon) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
  
  #####
  # If a search by elevation is requested, subset the stations data frame
  ####
  
  # Initial subset
  if (!is.null(lower_elev) & is.null(upper_elev)) {
    df_soundings.subset <- subset(df_soundings, df_soundings$elev >= lower_elev)
  } else if (is.null(lower_elev) & !is.null(upper_elev)) {
    df_soundings.subset <- subset(df_soundings, df_soundings$elev <= upper_elev)
  } else if (!is.null(lower_elev) & !is.null(upper_elev)) {
    df_soundings.subset <- subset(df_soundings,
                                  df_soundings$elev >= lower_elev &
                                    df_soundings$elev <= upper_elev)
  }
  
  # If a subset was generated and is of zero length, return notification that
  # no stations were found
  if (!is.null(lower_elev) | !is.null(upper_elev) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 0) {
      return(paste("No stations were identified from this search"))
    }
  }
  
  # If a subset was generated and is of single length, then a match has occurred
  # Need to assign the subset as 'target_station' in the global environment and
  # return a notification that a match was found
  if (!is.null(lower_elev) | !is.null(upper_elev) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) == 1) {
      assign("target_station", df_soundings.subset, envir = .GlobalEnv)
      return(paste("The following station was identified and set ",
                   "as the target station: wmo ",
                   target_station$wmo, ", wban ", target_station$wban,
                   " (", target_station$station_name, ")", sep = ''))
    }
  }
  
  # If a subset was generated that contains >100 records, return a notification
  # stating the number of stations found (but don't return a df object)
  # If the generated subset contains 2-100 records, return 'df_soundings.subset' 
  if (!is.null(lower_elev) | !is.null(upper_elev) &
        exists("df_soundings.subset")) {
    if (nrow(df_soundings.subset) > 100) {
      return(paste("A total of ", nrow(df_soundings.subset),
                   " stations were identified from this search",
                   sep = ''))
    }
    if (nrow(df_soundings.subset) > 1 & nrow(df_soundings.subset) <= 100) {
      return(df_soundings.subset)
    }
  }
  
  # Close the function
}
