aetheRa
=======

Tools to acquire and work with upper air data. Discover where the data is, read it in, process that data.

## Installation

Easy installation of aetheRa from GitHub is possible with the `devtools` package:

```coffee
require(devtools)
install_github('aetheRa', 'rich-iannone')
```

### Features

- fetching a current listing of sounding sites from the NOAA Radiosonde database
- selection and retrieval of data for a sounding location over a specified time interval
- exporting the sounding data into a format suitable for atmospheric dispersion modelling (i.e., CALMET `UP.DAT` file)

### Future Additions

- finding the nearest sounding location or a set within geographic bounds
- creating a visualization of the sounding data for specific days or periods
- determining the data completeness of a site's dataset during a specified time interval
- repairing a dataset using sounding site substitution and/or interpolation/extrapolation
- creating skewT plots and logP diagrams
