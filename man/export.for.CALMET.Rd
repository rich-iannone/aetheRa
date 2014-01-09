\name{Export a CALMET UP.DAT file}
\alias{export.for.CALMET}
\title{
Using upper air sounding data, create an UP.DAT input file for the CALMET model
}

\description{
This function creates an UP.DAT file for use in the CALMET meteorological model.
}

\usage{
export.for.CALMET(sounding_list = sounding_list,
                  export_all_times = FALSE,
                  start_date,
                  start_hour,
                  end_date,
                  end_hour,
                  top_pressure_level)
}

\arguments{
  \item{sounding_list}{
 
}
  \item{export_all_times}{
 
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
