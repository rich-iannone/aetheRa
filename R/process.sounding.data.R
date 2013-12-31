process.sounding.stations <- function(sounding_data_vector){

  # Include require statements
  require(RCurl)
  require(stringr)
  
  # This function parses the 'sounding_data' object and compose a list of data frames
  
  
  # Verify that a new record is identified in the current line
  is_254 <- str_detect(sounding_data_2[1], "^    254")
  is_1 <- str_detect(sounding_data_2[1], "^      1")
  is_2 <- str_detect(sounding_data_2[1], "^      2")
  is_3 <- str_detect(sounding_data_2[1], "^      3")
  is_4_9 <- str_detect(sounding_data_2[1], "^      [4-9]")
  
  header <- read.table(textConnection(sounding_data_2[1]),
                       stringsAsFactors = FALSE)
  
  # If line is a header line, read it as such
  if (is_254 == TRUE) {
    header <- read.table(textConnection(sounding_data_2[1]),
                         stringsAsFactors = FALSE)
    colnames(header) <- c("linetyp", "hour", "day", "month", "year")
  }
  
}
