

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


}


