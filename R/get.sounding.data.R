

# Include require statements
require(RCurl)
require(stringr)


# Get a current list of sounding stations
df_soundings <- get.sounding.stations()

# Get the necessary parameters
#
# Beginning date
bdate <- "2013010100"

# Ending date
edate <- "2013123123"

# Access type
access <-

# View the stations?
view <- "NO"

# Show states?
States <- "States"

# Show countries
Countries <- "Countries"

# Types of hours to show
shour <- "All+Times"

# Types of levels to show
ltype <- "All+Levels"

# Wind units
wunits <- "Tenths+of+Meter%2FSecond"

# Show station list?
stationlist <- "YES"

# Requested station
#
# Request by WBAN-WMO pair

# Request by line number in 'df_soundings' data frame
station_list_id <- 1


station_list <- "YLW+94151+71203+49.97+-119.38+00454+KELOWNA+APT+BC+CA"



# Make request for data, store return message
noaa_cgi_message <- getURL(paste(
  "http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
  "bdate=2013010100",
  "&",
  "edate=2013123123",
  "&",
  "access=All+Sites",
  "&",
  "view=NO",
  "&",
  "States=States",
  "&",
  "Countries=Countries",
  "&",
  "shour=All+Times",
  "&",
  "ltype=All+Levels",
  "&",
  "wunits=Tenths+of+Meters%2FSecond",
  "&",
  "stationlist=YES",
  "&",
  "station_list=YLW+94151+71203+49.97+-119.38+00454+KELOWNA+APT+BC+CA",
  "&",
  "osort=Station+Series+Sort",
  "&",
  "oformat=FSL+format+%28ASCII+text%29", sep = ''))

# Parse message and construct URI for data
data_URI <- paste("http://www.esrl.noaa.gov/raobs/temp", str_match(string = noaa_cgi_message, pattern = "temp(.*)(tmp)")[1,2], "tmp", sep = '')

# Get the data as a large character object
sounding_data <- getURL(data_URI)

# Write the data to a file
writeLines(sounding_data, con = "file:///Users/riannone/Dropbox/R Projects/test_FSL_sounding.txt", sep = "\n")

# Read back the file as lines
sounding_data_2 <- readLines(con = "file:///Users/riannone/Dropbox/R Projects/test_FSL_sounding.txt")

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


