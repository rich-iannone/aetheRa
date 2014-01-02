process.sounding.stations <- function(sounding_data_vector){

  # Include require statements
  require(RCurl)
  require(stringr)
  
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
  
  # Use a while loop to cycle through the 'sounding_data' object and extract element
  while ( i < length(sounding_data)) {
    header_254 <- read.table(textConnection(sounding_data[i]),
                             stringsAsFactors = FALSE)
    colnames(header_254) <- c("lintyp_254", "hour", "day",
                              "month", "year")
    
    header_1 <- read.table(textConnection(sounding_data[i + 1]),
                           stringsAsFactors = FALSE)
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
      
    }
    
    
  }
  
}
