\name{Search for and select a sounding station}
\alias{export.for.CALMET}
\title{
Using upper air sounding data, create an UP.DAT input file for the CALMET model
}

\description{
This function creates an UP.DAT file for use in the CALMET meteorological model.
}

\usage{
select.sounding.station(df_soundings = df_soundings,
                        id_by_wban_wmo = NULL,
                        search_station_name = NULL,
                        search_init = NULL,
                        search_wban = NULL,
                        search_wmo = NULL,
                        search_prov_state = NULL,
                        search_country = NULL,
                        lower_lat = NULL,
                        upper_lat = NULL,
                        lower_long = NULL,
                        upper_long = NULL,
                        lower_elev = NULL,
                        upper_elev = NULL
}

\arguments{
  \item{df_soundings}{
 
}
  \item{id_by_wban_wmo}{
 
}
  \item{search_station_name}{
 
}
  \item{search_init}{
 
}
  \item{search_wban}{
 
}
  \item{search_wmo}{
 
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
