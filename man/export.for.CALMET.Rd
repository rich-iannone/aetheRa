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
 
}
  \item{end_date}{
 
}
  \item{end_hour}{
 
}
  \item{top_pressure_level}{
 
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
#

export.for.CALMET(export_all_times = FALSE
                  start_date = "2013-01-01"
                  start_hour = 0
                  end_date = "2013-12-31"
                  end_hour = 0
                  top_pressure_level = 500)
}
