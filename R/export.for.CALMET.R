export.for.CALMET <- function(sounding_list = sounding_list){
  
  # Include require statements
  require(RCurl)
  require(stringr)
  
  # Check for existance of 'sounding_list' object, created by the
  # 'process.sounding.data' function; if the object doesn't exist, stop the function
  # with a message
  if (!exists("sounding_data")) {
    stop("Processed sounding data is not available. Use the 'process.sounding.data' function")
  }
  
  
  
  # Close the function
}
