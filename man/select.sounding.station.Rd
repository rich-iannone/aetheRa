\name{Search for and select a sounding station}
\alias{select.sounding.station}
\title{
Filter and select a sounding station from a larger collection
}

\description{
This function allows for the isolation of a single station which, upon successful matching, becomes the target station for subsequent analyses.
}

\usage{
select.sounding.station(id_by_wban_wmo = NULL,
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
                        upper_elev = NULL)
}

\arguments{
  \item{id_by_wban_wmo}{
  using the combination of WBAN and WMO numbers (with a string in the form of 'XXXXX-YYYYY' in the order of WBAN and WMO), it is possible to uniquely identify a sounding station. When searching with this keyword, all other arguments for which values were supplied will be disregarded.
}
  \item{search_station_name}{
 supplying search text in the form of a string will filter the list of available sounding stations and provide feedback on which stations were found. When searching with this keyword, all other arguments for which values were supplied will be disregarded.
}
  \item{search_init}{
 
}
  \item{search_wban}{
 
}
  \item{search_wmo}{
 
}
  \item{search_prov_state}{

}
  \item{search_country}{

}
  \item{lower_lat}{

}
  \item{upper_lat}{

}
  \item{lower_long}{
 
}
  \item{upper_long}{
 
}
  \item{lower_elev}{
 
}
  \item{upper_elev}{
 
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

select.sounding.station(id_by_wban_wmo = NULL,
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
                        upper_elev = NULL)
}
