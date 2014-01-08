\name{Get sounding data}
\alias{get.sounding.data}
\title{
Get upper air sounding data as a text file in FSL format
}

\description{
This function allows for the request and retrieval of upper air sounding data in FSL format from the NOAA RAOB database.
}

\usage{
get.sounding.data(start_date,
                  end_date,
                  hour_type = "all",
                  level_type = "all",
                  wind_units = "tenths_ms",
                  station_number = NULL,
                  station_wban_wmo = NULL,
                  output_file_path = "working",
                  output_file_name = "FSL-Sounding.txt",
                  details_in_file_name = TRUE)
}

\arguments{
  \item{start_date}{
the starting date of the sounding data request in the format "YYYY-MM-DD". The earliest sounding for the specified date will be included in the request.
}
  \item{end_date}{
the end date of the sounding data request in the format "YYYY-MM-DD". The last sounding for the specified date will be included in the request.
}
  \item{hour_type}{
the types of hours to be supplied in the returned dataset. The default is "all", which includes sounding data from launches near 12:00 UTC and those near 24:00 UTC.
}
  \item{level_type}{
the types of levels to be supplied in the returned dataset. The default is "all", which includes all sounding types (mandatory and significant types). Other options are "mandatory" (includes only the mandatory-type levels) and "significant" (includes only the significant-type levels).
}
  \item{wind_units}{
the desired wind units for the sounding data request. Options are tenths of meters per second ("tenths_ms", the default) and "knots".
}
  \item{station_number}{
the selection of the desired sounding site can be performed by identifying a line number from the dataframe supplied by the 'get.sounding.stations' function.
}
  \item{station_wban_wmo}{
the selection of the desired sounding site for the data request can be achieved by supplying WBAN and WMO numbers in the form of "XXXXX-YYYYY" (WBAN first, WMO second).
}
  \item{output_file_path}{
the output file path for the retrieved sounding data can be specified here by using "working", which sets the path to the current working directory, or, by specifying an absolute path.
}
  \item{output_file_name}{
the desired filename for the retrieved sounding data, including an extension such as ".txt" or ".dat". The default is "FSL-Sounding.txt", so, not modifying this argument over several uses (especially in conjunction with a "FALSE" setting for the 'details_in_file_name' argument) may result in overwriting previously generated files.
}
  \item{details_in_file_name}{
incorporates details about the sounding data into the filename. Set to "TRUE" by default to provide self-describing filenames and reducing the risk of overwriting files. Setting to "FALSE" strictly uses the filename specified in the 'output_file_name' argument.
}
}

\details{

}

\value{

}

\references{

}

\author{
Richard Iannone
}

\note{

}

\seealso{

}
\examples{
# Get all FSL sounding data for the station with WBAN 99999 and
# WMO 01001 (Jan Mayen) for the year 2012

sounding_data <- get.sounding.data(start_date = "2012-01-01",
                                   end_date = "2012-12-31",
                                   hour_type = "all",
                                   level_type = "all",
                                   wind_units = "tenths_ms",
                                   station_wban_wmo = "99999-01001",
                                   output_file_path = "working",
                                   output_file_name = "FSL-Sounding.txt",
                                   details_in_file_name = TRUE)
}
