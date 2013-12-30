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
  # Close function
}
