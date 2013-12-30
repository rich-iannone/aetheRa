get.sounding.stations <- function(){
  
  require(RCurl)
  require(stringr)
  
  # Obtain the HTML source from a URI containing a query
  URI <- getURL("http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?shour=All+Times&ltype=All+Levels&wunits=Tenths+of+Meters%2FSecond&bdate=1990010100&edate=2013122523&access=All+Sites&view=YES&osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29")
  
  # Create a 'pattern' string object containing the regex pattern for extracting sounding data strings
  # from the URI
  pattern <- "<OPTION> [0-9A-Z]*[ ]*[0-9]* [0-9]{5} [0-9/.-]* [0-9/.-]* [0-9-]{5,6}  [.]*  [0-9A-Z]{2} [0-9A-Z]{2}"
  
  # Generate vector list of strings from URI page source
  lines <- gsub(pattern = pattern, replacement = "\\1", x = URI)
  lines <- gsub(pattern = ".*MULTIPLE SIZE=\"10\">\n", replacement = "", x = lines)
  lines <- gsub(pattern = "\n\n</SELECT>.*", replacement = "", x = lines)
  lines <- gsub(pattern = "<OPTION> ", replacement = "", x = lines)
  lines <- str_split(lines, "\n\n")
  lines <- unlist(lines)
  
  # Initialize the data objects
  # Loop through list of strings, extract and clean the substrings corresponding to data elements
  # Create a data frame with vector lists and coerce some objects into numeric objects
  for (i in 1:length(lines)){
    if (i == 1) {
      init <- mat.or.vec(nr = length(lines), nc = 1)
      wban <- mat.or.vec(nr = length(lines), nc = 1)
      wmo <- mat.or.vec(nr = length(lines), nc = 1)
      lat <- mat.or.vec(nr = length(lines), nc = 1)
      lon <- mat.or.vec(nr = length(lines), nc = 1)
      elev <- mat.or.vec(nr = length(lines), nc = 1)
      station_name <- mat.or.vec(nr = length(lines), nc = 1)
      prov_state <- mat.or.vec(nr = length(lines), nc = 1)
      country <- mat.or.vec(nr = length(lines), nc = 1)
    }
    init[i] <- str_match(string = lines[i], pattern = "^([0-9A-Z]*)")[1,2]
    wban[i] <- str_match(string = lines[i], pattern = "^[0-9A-Z]+[ ]+([0-9]*)")[1,2]
    wmo[i] <- str_match(string = lines[i], pattern = "^[0-9A-Z]+[ ]+[0-9]* ([0-9]{5})")[1,2]
  # Close function
}
