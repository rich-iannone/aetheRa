<img src="inst/AetheRa.png" width="75%">

AetheRa allows you to acquire and work with upper air data. Discover where the data is, read it in, process that data.

## Installation

Installation of AetheRa from GitHub can be done with the `devtools` package:

```R
require(devtools)
install_github('rich-iannone/aetheRa')
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

To obtain the sounding data, use the `get_sounding_data` function by supplying the data frame that contains the subset of sounding sites (generated with call to `select_sounding_station`). The sounding data is obtained between supplied beginning and ending dates.

Note that station data will be supplied for the station in only the first row of the data frame specified in the `stations_df` argument. Thus, it is best to generate the data frame object with only a single record. This is typically done with a call to `select_sounding_station` that specifies either the name of the sounding site (using the `search_station_name` argument) or the set of station identifiers (using the `id_by_wban_wmo` argument).

Here is an example that downloads sounding observations from the Quillayute, WA station for the year 2012, saving to the file 'FSL-Sounding.txt':

```R
Quillayute_data <- get_sounding_data(stations_df = Quillayute_sounding,
                                     start_date = "2012-01-01",
                                     end_date = "2012-02-01",
                                     hour_type = "all",
                                     level_type = "all",
                                     wind_units = "tenths_ms",
                                     output_file_path = "working",
                                     output_file_name = "FSL-Sounding.txt",
                                     details_in_file_name = TRUE)
```

Now the data should be processed. That can be done with the `process_sounding_data` function, using the return object from the `get_sounding_data` function as the sole argument.

```R
Quillayute_data_processed <- process_sounding_data(Quillayute_data)
```

If you need CALMET-ready input from upper air data, the object created from the `process_sounding_data` call can be used to create an UP.DAT file. Simply use the `export_data_to_CALMET` function to generate that file. With that function, beginning and ending dates and hours should be supplied to define the time period for the UP.DAT file. Additionally, a maximum pressure level (in hPa) should be provided. 

```R
export_data_to_CALMET(processed_sounding_data = Quillayute_data_processed,
                      export_all_times = FALSE,
                      start_date = "2012-01-01",
                      start_hour = 0,
                      end_date = "2012-02-01",
                      end_hour = 0,
                      top_pressure_level = 500)
```

### Future Additions

- create a visualization of the sounding data for specific days or periods
- determine the data completeness of a site's dataset during a specified time interval
- repair a dataset using sounding site substitution and/or interpolation/extrapolation
- create skewT plots and logP diagrams
