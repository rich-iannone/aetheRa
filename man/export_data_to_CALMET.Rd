% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/export_data_to_CALMET.R
\name{export_data_to_CALMET}
\alias{export_data_to_CALMET}
\title{Export a CALMET UP.DAT file}
\usage{
export_data_to_CALMET(processed_sounding_data, export_all_times = FALSE,
  start_date, start_hour, end_date, end_hour, top_pressure_level,
  output_file_name = "up.txt", details_in_file_name = TRUE)
}
\arguments{
\item{processed_sounding_data}{the return object from the `get_sounding_data` function.}

\item{export_all_times}{providing TRUE for this will create an UP.DAT file with time bounds matching those in the 'processed_sounding_data' list object (created after use of the 'process.sounding.data' function). The default is set to FALSE, in which case values for the arguments 'start_date', 'start_hour', 'end_date', and 'end_hour' must be supplied.}

\item{start_date}{a starting date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".}

\item{start_hour}{a starting hour must accompany the entry for 'start_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'start_hour' is numeric.}

\item{end_date}{an ending date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".}

\item{end_hour}{an ending hour must accompany the entry for 'end_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'end_hour' is numeric.}

\item{top_pressure_level}{the top pressure level to which sounding data should be constrained. A numeric value, representing atmospheric pressure in hPa units, should be supplied.}
}
\description{
Using upper air sounding data, create an UP.DAT input file for the CALMET model.
}
\examples{
\dontrun{
# After generating the 'processed_sounding_data' object (from use of the 'process.sounding.data' function),
# generate a CALMET UP.DAT file for 2013, constraining the output to the 500 hPa pressure level
export_data_to_CALMET(processed_sounding_data = the_processed_sounding_data,
                      export_all_times = FALSE
                      start_date = "2013-01-01"
                      start_hour = 0
                      end_date = "2013-12-31"
                      end_hour = 0
                      top_pressure_level = 500)
}
}

