process.sounding.stations <- function(){
#' Process sounding data
#' @description Using upper air sounding data, create a list object that can be used for analysis.
#' @export process.sounding.stations
#' @examples
#' \dontrun{
#' # Process sounding data created by the 'get.sounding.data' function
#' processed_data <- process.sounding.stations(sounding_data = sounding_data)
#'}


  # Include require statements
  require(RCurl)
  require(stringr)
  
  # Check for existance of 'sounding_data' object, created by the
  # 'get.sounding.data' function; if the object doesn't exist, stop the function
  # with a message
  if (!exists("sounding_data")) {
  stop("Sounding data is not available. Use the 'get.sounding.data' function")
  }
  
  # Determine number of soundings in file, necessary for initialization of list object
  # Check for equal numbers of header lines (254, 1, 2, and 3)
  soundings_254 <- length(grep(pattern = "^    254", x = sounding_data))
  soundings_1 <- length(grep(pattern = "^      1", x = sounding_data))
  soundings_2 <- length(grep(pattern = "^      2", x = sounding_data))
  soundings_3 <- length(grep(pattern = "^      3", x = sounding_data))
  
  # The condition of full headers (having line types 254, 1, 2, and 3) is verified
  # by seeing if the counts are all equal
  full_headers <- ifelse(soundings_254 == soundings_1 &
                           soundings_1 == soundings_2 &
                           soundings_2 == soundings_3, TRUE, FALSE)
  
  correct_first_line <- ifelse(1 %in% grep(pattern = "^    254",
                                           x = sounding_data),
                               TRUE, FALSE)
  
  # If the full headers condition passes and the first line contains the first header line,
  # the number of soundings is passed to the 'soundings' object
  if (full_headers == TRUE & correct_first_line == TRUE) soundings <- soundings_254
  
  # Remove 'soundings_*' objects
  rm(soundings_254, soundings_1, soundings_2, soundings_3)
  
  # Remove other objects from global workspace
  rm(full_headers, correct_first_line)
  
  # Create list of soundings
  sounding_list <- vector("list", soundings)
  
  # Each list element in sounding_list will have two data frames:
  # (1) header information (e.g., time, sounding launch coordinates, etc.), and
  # (2) sounding data (e.g., pressure, height, temperature, DPT, WD, WS, and
  # sounding type)
  
  # Set iterator index (i) to 1; this corresponds the line number in 'sounding_data'
  # vector list
  i <- 1
  
  # Set iterator index (list) to 1; this corresponds to the sounding number (and the
  # list number)
  list_item <- 0
  
  # Use a while loop to cycle through the 'sounding_data' object and extract elements
  while (i < length(sounding_data)) {
    header_254 <- read.table(textConnection(sounding_data[i]),
                             stringsAsFactors = FALSE)
    colnames(header_254) <- c("lintyp_254", "hour", "day",
                              "month", "year")
    # Recode month from 3-letter char object to a month number
    header_254$month <- switch(header_254$month,
                               "JAN" = 1,
                               "FEB" = 2,
                               "MAR" = 3,
                               "APR" = 4,
                               "MAY" = 5,
                               "JUN" = 6,
                               "JUL" = 7,
                               "AUG" = 8,
                               "SEP" = 9,
                               "OCT" = 10,
                               "NOV" = 11,
                               "DEC" = 12)
      
    header_1 <- read.table(textConnection(sounding_data[i + 1]),
                           stringsAsFactors = FALSE)
    
    # Need to check and correct for those situations where the lat value is merged with
    # the lon value
    if (grepl("W", header_1$V4) | grepl("E", header_1$V4)) {
      # Get regex pattern using look-around assertions
      if (grepl("N", header_1$V4)) split_pattern <- "(?<=N)(?=[0-9])"
      if (grepl("S", header_1$V4)) split_pattern <- "(?<=S)(?=[0-9])"
      # Apply 'strsplit' to vector item and obtain the separated lat/lon values
      lat <- unlist(strsplit(header_1$V4, split_pattern, perl = TRUE))[1]
      lon <- unlist(strsplit(header_1$V4, split_pattern, perl = TRUE))[2]
      # Add column 7 to 'header_1'
      V7 <- 0
      header_1 <- cbind(header_1, V7)
      # Move columns 5 & 6 to columns 6 & 7
      header_1$V7 <- header_1$V6
      header_1$V6 <- header_1$V5
      # Copy lat and lon values to columns 4 & 5
      header_1$V4 <- lat
      header_1$V5 <- lon
      # Remove variables
      rm(split_pattern, lat, lon, V7)
    }
    
    colnames(header_1) <- c("lintyp_1", "wban", "wmo", "lat",
                            "lon", "elev", "rtime")
    
    header_2 <- read.table(textConnection(sounding_data[i + 2]),
                           stringsAsFactors = FALSE)
    colnames(header_2) <- c("lintyp_2", "hydro", "mxwd", "tropl",
                            "lines", "tindex", "source")
    
    header_3 <- read.table(textConnection(sounding_data[i + 3]),
                           stringsAsFactors = FALSE)
    colnames(header_3) <- c("lintyp_3", "staid", "sonde", "wsunits")
    
    header <- cbind(header_254, header_1, header_2, header_3)
    header$lintyp_254 <- NULL
    header$lintyp_1 <- NULL
    header$lintyp_2 <- NULL
    header$lintyp_3 <- NULL
    
    # Increment the 'list_item' vector, corresponding to the sounding number
    list_item <- list_item + 1
    
    # Store the header information as a data frame in the first list slot
    sounding_list[[list_item]][[1]] <- header
    
    for (j in (i + 4):(i + header$lines - 1)) {
      
      # Initialize the data frame for the sounding data
      if (j == (i + 4)) {data <- as.data.frame(mat.or.vec(
        nr = header$lines - 4, nc = 7))
      }
      
      data[(j - 3 - i),] <- read.table(textConnection(sounding_data[j]),
                                   stringsAsFactors = FALSE)
      
      if (j == (i + header$lines - 1)) {
        colnames(data) <- c("lintyp", "pressure", "height", "temp",
                            "dewpt", "wind_dir", "wind_speed")
        data$temp <- data$temp / 10
        data$dewpt <- data$dewpt / 10
        data$wind_speed <- data$wind_speed / 10
      }
    }
    
    # Store the sounding data information as a data frame in the
    # second slot of the list item
    sounding_list[[list_item]][[2]] <- data
    
    # increment the index of i to the first header line of the next sounding
    i <- i + header$lines
    
    # Remove the 'header' and 'header_*' items
    rm(header, header_254, header_1, header_2, header_3)
    
    # Remove the j index object
    rm(j)
    
    # Remove the 'data' object
    rm(data)
    
    # Create a progress bar
    pb <- txtProgressBar(min = 1, max = length(sounding_data), style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
    Sys.sleep(1)
    close(pb)
    
    # Close while loop
  }
  
  # Remove items from workspace
  rm(i, soundings, list_item, pb)
  
  # Return the 'sounding_list' object
  sounding_list
  
  # Assign object to global environment
  assign("sounding_list", sounding_list, envir = .GlobalEnv)
  
  # Add notification that data was downloaded and assigned to the global workspace
  if (exists("sounding_list")) {
    print(paste("Sounding data was processed and assigned as object 'sounding_list'."))
  }
  
# Close the function
}
