

# Include require statements
require(RCurl)
require(stringr)


# Get a current list of sounding stations
df_soundings <- get.sounding.stations()

# Get the necessary parameters
#
# Beginning date
bdate <-

# Ending date
edate <-

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

}


