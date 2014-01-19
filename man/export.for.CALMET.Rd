\name{Export a CALMET UP.DAT file}
\alias{export.for.CALMET}
\title{
Using upper air sounding data, create an UP.DAT input file for the CALMET model
}

\description{
This function creates an UP.DAT file for use in the CALMET meteorological model.
}

\usage{
export.for.CALMET(export_all_times = FALSE,
                  start_date,
                  start_hour,
                  end_date,
                  end_hour,
                  top_pressure_level)
}

\arguments{
  \item{export_all_times}{
providing TRUE for this will create an UP.DAT file with time bounds matching those in the 'sounding_list' list object (created after use of the 'process.sounding.data' function). The default is set to FALSE, in which case values for the arguments 'start_date', 'start_hour', 'end_date', and 'end_hour' must be supplied.
}
  \item{start_date}{
a starting date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".
}
  \item{start_hour}{
a starting hour must accompany the entry for 'start_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'start_hour' is numeric.
}
  \item{end_date}{
an ending date intended for the UP.DAT output should be supplied if 'export_all_times' is set to FALSE (the default). The date should be supplied as a string in the format "YYYY-MM-DD".
}
  \item{end_hour}{
an ending hour must accompany the entry for 'end_date'. With 'export_all_times' set to FALSE, explicit dates and times for starting and ending periods must be set. The format for 'end_hour' is numeric.
}
  \item{top_pressure_level}{
the top pressure level to which sounding data should be constrained. A numeric value, representing atmospheric pressure in hPa units, should be supplied.
}
}

\details{

}

\author{
Richard Iannone
}

\examples{
# After generating the 'sounding_list' object (from use of the 'process.sounding.data' function),
# generate a CALMET UP.DAT file for 2013, constraining the output to the 500 hPa pressure level
export.for.CALMET(export_all_times = FALSE
                  start_date = "2013-01-01"
                  start_hour = 0
                  end_date = "2013-12-31"
                  end_hour = 0
                  top_pressure_level = 500)
}
