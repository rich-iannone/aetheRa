<img src="inst/AetheRa.png" width="75%">

AetheRa allows you to acquire and work with upper air data. Discover where the data is, read it in, process that data.

## Installation

Installation of AetheRa from GitHub can be done with the `devtools` package:

```R
require(devtools)
install_github('aetheRa', 'rich-iannone')
```

### Features

- fetch a current listing of sounding sites and metadata from the NOAA Radiosonde database
- search and filter through the available sites using various keywords
- select and retrieve data for a sounding location over a specified time interval
- export the sounding data into a format suitable for atmospheric dispersion modelling (i.e., CALMET `UP.DAT` file)

# How to Use AetheRa

First off, get a fresh listing of all the available soundings, all over the world:

```R
all_soundings <- get_sounding_stations()
```

This returns a data frame in the object `all_soundings`. As of August 30, 2014 at 8:30 PM, 1102 unique station names were retrieved (using `sort(unique(all_soundings$station_name))`). This object contains columns for station identifiers (`init`, `wban`, `wmo`, and `station_name`), for location and elevation (`lat`, `lon`, and `elev`), for country (as 2-letter country codes), and for province or state (Canada and U.S. only). 

To obtain subsets from this collection of all global soundings, the `select_sounding_station` function can be used. With this function, sounding sites can be isolated by station name, station identifiers, by country and/or by state/province, get latitude and longitude bounds, and by elevation limits. Searching by station name is possible by using the following call of `select_sounding_station`:

```R
Quillayute_sounding <- select_sounding_station(all_soundings,
                                               search_station_name = "Quillayute")
```

If the combination of WBAN and WMO numbers is known, the same station can be subset by supplying a hyphenated pair of WBAN and WMO identifier numbers to the `id_by_wban_wmo` argument as in the following:

```R
Quillayute_sounding <- select_sounding_station(all_soundings,
                                               id_by_wban_wmo = "94240-72797")
```

To find all US sounding stations, the following statement could be used:

```R
US_soundings <- select_sounding_station(all_soundings,
                                        search_country = "US",
                                        search_prov_state = "WA")
```

To get those soundings residing within 40ºN and 50ºN, and 110ºW and 120ºW: 

```R
soundings_within_latlon <- select_sounding_station(all_soundings,
                                                   lower_lat = 40,
                                                   upper_lat = 50,
                                                   lower_lon = -120,
                                                   upper_lon = -110)
```

To get a collection of sounding sites that are between 3000 and 5000 m ASL:

```R
high_soundings <- select_sounding_station(all_soundings,
                                          lower_elev = 3000,
                                          upper_elev = 5000)
```

### Future Additions

- create a visualization of the sounding data for specific days or periods
- determine the data completeness of a site's dataset during a specified time interval
- repair a dataset using sounding site substitution and/or interpolation/extrapolation
- create skewT plots and logP diagrams
