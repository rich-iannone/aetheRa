<img src="inst/AetheRa.png" width="75%">

AetheRa allows you to acquire and work with upper air data. Discover where the data is, read it in, process that data.

## Installation

Installation of AetheRa from GitHub can be done with the `devtools` package:

```coffee
require(devtools)
install_github('aetheRa', 'rich-iannone')
```

### Features

- fetch a current listing of sounding sites and metadata from the NOAA Radiosonde database
- search and filter through the available sites using various keywords
- select and retrieve data for a sounding location over a specified time interval
- export the sounding data into a format suitable for atmospheric dispersion modelling (i.e., CALMET `UP.DAT` file)

### Future Additions

- create a visualization of the sounding data for specific days or periods
- determine the data completeness of a site's dataset during a specified time interval
- repair a dataset using sounding site substitution and/or interpolation/extrapolation
- create skewT plots and logP diagrams
