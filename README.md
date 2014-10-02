# SWMPr repository for estuarine monitoring data

This repository contains materials to retrieve, organize, and analyze estuarine monitoring data from the System Wide Monitoring Program (<a href="http://nerrs.noaa.gov/RCDefault.aspx?ID=18">SWMP</a>) implemented by the National Estuarine Research Reserve System (<a href="http://nerrs.noaa.gov/">NERRS</a>).  SWMP was initiated in 1995 to provide continuous monitoring data at over 300 stations in 28 estuaries across the United States.  SWMP data are maintained online by the Centralized Data Management Office (CDMO). This R package will provide several functions to retrieve, organize, and analyze SWMP data from the CDMO.  Information on the CDMO web services are available <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  Your computer's IP address must be registered with the CDMO website to use most of the data retrieval functions, see contact info in the link.  All other functions can be used after obtaining data from the CDMO, as described below. 

The package has many dependencies, the most important being the SSOAP package for retrieving data from the CDMO using a SOAP client interface. The SSOAP package is not required to use the package but is necessary for using most of the data retrieval functions, see below.  The SSOAP package is currently removed from CRAN but accessible at <a href="http://www.omegahat.org/SSOAP/">http://www.omegahat.org/SSOAP/</a>.  It can be installed as follows:


```r
install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T,  type =  "source")
```

All data obtained from the CDMO should be <a href="http://cdmo.baruch.sc.edu/data/citation.cfm">cited</a> using the format:

National Estuarine Research Reserve System (NERRS). 2012. System-wide Monitoring Program. Data accessed from the NOAA NERRS Centralized Data Management Office website: http://cdmo.baruch.sc.edu/; accessed 12 October 2012.

To cite this package:

Beck MW. 2014. SWMPr: An R package for the National Estuarine Research Reserve System.  Beta version. https://github.com/fawda123/SWMPr

##Accessing the repository

This repository is currently under development and will later be uploaded as a package repository.  This will allow use of the `install.packages` function for direct install within R.  For now, Github users can fork and pull the materials to create a local clone of the current form.  Otherwise, the `funcs.r` file contains all currently developed (and partially tested) functions.  Package dependencies can be found in `.RProfile`.  Files ending in `retrieval.r` were used to test the functions.   

##Data retrieval

SWMP data can be obtained directly from the CDMO through an online query or by using the retrieval functions provided in this package.  In the latter case, the IP address for the computer making the request must be registered with CDMO.  This can be done by following instructions <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  The <a href="http://cdmo.baruch.sc.edu/data/metadata.cfm">metadata</a> should also be consulted for available data, including the parameters and date ranges for each monitoring station.  Metadata are included as a .csv file with data requested from the CDMO and can also be obtained using the `site_codes` (all sites) or `site_codes_ind` (individual site) functions.  


```r
# retrieve metadata for all sites
site_codes()

# retrieve metadata for a single site
site_codes_ind('apa')
```

Due to rate limitations on the server, the retrieval functions in this package return a limited number of records.  The functions are more amenable to analyses with short time periods, although these functions could be used iteratively (i.e., with `for` or `while` loops) to obtain longer time series.  Data retrieval functions to access the CDMO include `all_params`, `all_params_dtrng`, and `single_param`.  These are functions that call the available methods on the CDMO SOAP interface.  `all_params` returns the most recent 100 records of all parameters at a station, `all_params_dtrng` returns all records within a date range for all parameters or a single parameter, and `single_param` is identical to `all_params` except that a single parameter is requested.    


```r
# all parameters for a station, most recent
all_params('sfbfmwq')

# get all parameters within a date range
all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'))

# get single parameter within a date range
all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013')),
  param = 'do_mgl')

# single parameter for a station, most recent
single_param('tjrtlmet', 'wspd')
```

For larger requests, it is easier to obtain data outside of R using the CDMO query system.  Data can be retrieved from the CDMO several ways.  Data from single stations can be requested from the <a href="http://cdmo.baruch.sc.edu/get/export.cfm">data export system</a>, whereas data from multiple stations can be requested from the <a href="http://cdmo.baruch.sc.edu/aqs/">advanced query system</a>.  The `import_local` function is used to import local data into R that were downloaded from the CDMO with the <a href="http://cdmo.baruch.sc.edu/aqs/zips.cfm">zip downloads</a> feature within the advanced query system.  The downloaded data will include multiple .csv files by year for a given data type (e.g., apacpwq2002.csv, apacpwq2003.csv, apacpnut2002.csv, etc.).  It is recommended that all stations at a site and the complete date ranges are requested to avoid repeated requests to CDMO.  The `import_local` function can be used once the downloaded files are extracted to a local path. 


```r
# import data for apaebmet from 'zip_ex' path
import_local('zip_ex', 'apaebmet') 
```

In all cases, the imported data need to assigned to an object in the workspace for use with other functions:




```r
# import data and assign to dat
dat <- import_local('zip_ex', 'apaebmet', trace = F) 

# view first six rows
head(dat$station_data)
```

```
##         datetimestamp atemp f_atemp rh f_rh   bp f_bp wspd f_wspd maxwspd
## 1 2011-01-01 00:00:00  15.4    <0>  94 <0>  1019 <0>   2.6   <0>      3.4
## 2 2011-01-01 00:15:00  15.2    <0>  95 <0>  1019 <0>   2.7   <0>      4.0
## 3 2011-01-01 00:30:00  15.2    <0>  95 <0>  1019 <0>   2.8   <0>      3.5
## 4 2011-01-01 00:45:00  15.3    <0>  95 <0>  1019 <0>   3.1   <0>      4.2
## 5 2011-01-01 01:00:00  15.3    <0>  95 <0>  1018 <0>   3.2   <0>      4.4
## 6 2011-01-01 01:15:00  15.3    <0>  95 <0>  1018 <0>   3.6   <0>      4.9
##   f_maxwspd wdir f_wdir sdwdir f_sdwdir totpar  f_totpar totprcp f_totprcp
## 1      <0>   145   <0>       8     <0>     0.8 <1> (CSM)       0      <0> 
## 2      <0>   146   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 3      <0>   139   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 4      <0>   140   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 5      <0>   144   <0>       6     <0>     0.8 <1> (CSM)       0      <0> 
## 6      <0>   141   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
##   cumprcp f_cumprcp totsorad f_totsorad
## 1       0      <0>        NA      <-1> 
## 2       0      <0>        NA      <-1> 
## 3       0      <0>        NA      <-1> 
## 4       0      <0>        NA      <-1> 
## 5       0      <0>        NA      <-1> 
## 6       0      <0>        NA      <-1>
```

##swmpr object class

All data retrieval functions return a swmpr object that includes relevant data and several attributes describing the dataset.  The data include a datetimestamp column in the appropriate timezone for a station.  Note that the datetimestamp is standard time for each timezone and does not include daylight savings. Additional columns include parameters for a given data type (weather, nutrients, or wtaer quality) and correspondingg QAQC columns if returned from the initial data request.  The attributes for a swmpr object include `names` of the dataset, `class` (swmpr) `station name` (7 or 8 characters), `qaqc_cols` (logical), `date_rng` (POSIXct vector), `timezone` (text string in country/city format), and `parameters` (character vector).  Attributes of a swmpr object can be viewed as follows:


```r
# verify that dat is swmpr class
class(dat)
```

```
## [1] "swmpr"
```

```r
# all attributes of dat
attributes(dat)
```

```
## $names
## [1] "station_data"
## 
## $class
## [1] "swmpr"
## 
## $station
## [1] "apaebmet"
## 
## $qaqc_cols
## [1] TRUE
## 
## $date_rng
## [1] "2011-01-01 00:00:00 EST" "2013-12-31 23:45:00 EST"
## 
## $timezone
## [1] "America/Jamaica"
## 
## $parameters
##  [1] "atemp"    "rh"       "bp"       "wspd"     "maxwspd"  "wdir"    
##  [7] "sdwdir"   "totpar"   "totprcp"  "cumprcp"  "totsorad"
```

```r
# a single attribute of dat
attr(dat, 'station')
```

```
## [1] "apaebmet"
```

The swmpr object class was created for use with specific methods and it is suggested that these methods be used for data organization and analysis.  The actual data for a swmpr object (e.g., `dat$station_data` as a data frame) can be assigned to an object in the workspace if preferred, although this is not recommended.  Available methods for the swmpr class are described below and can also be viewed:


```r
# available methods for swmpr class
methods(class = 'swmpr')
```

```
## [1] comb.swmpr    qaqc.swmpr    setstep.swmpr subset.swmpr
```

##swmpr methods

Three categories of functions are available: retrieve, organize, and analyze.  The retrieval functions import the data into R as a swmpr object for use with the organize and analyze functions.  Methods defined for swmpr objects can be applied with the organize and analyze functions.  These methods are available for generic functions specific to this package, in addition to methods for existing generic functions available from other packages.  S3 methods are implemented in all cases.  

The organize functions are used to clean or prepare the data for analysis, including removal of QAQC flags, subsetting, creating a standardized time series vector, and combining data of different types.  The `qaqc` function is a simple screen to retain values from the data with specified QAQC flags, described <a, href="http://cdmo.baruch.sc.edu/data/qaqc.cfm">here</a>.  Each parameter in the swmpr data typically has a corresponding QAQC column of the same name with the added prefix 'f_'.  Values in the QAQC column specify a flag from -5 to 5.  Generally, only data with the '0' QAQC flag should be used, which is the default option for the `qaqc` function.  Data that do not satisfy QAQC criteria are converted to NA values.  Processed data will have QAQC columns removed, in addition to removal of values in the actual parameter columns that do not meet the criteria. 


```r
# qaqc screen for a swmpr object, retain only '0'
qaqc(dat)

# retain all data regardless of flag
qaqc(dat, qaqc_keep = NULL)

# retain only '0' and '-1' flags
qaqc(dat, qaqc_keep = c(0, -1))
```

A subset method added to the existing `subset` function is available for swmpr objects.  This function is used to subset the data by date and/or a selected parameter.  The date can be a single value or as two dates to select records within the range. The former case requires a binary operator input as a character string passed to the argument, such as `>` or `<`.  The subset argument for the date(s) must also be a character string of the format YYYY-mm-dd HH:MM for each element (i.e., %Y-%m%-%d %H:%M in POSIX standards).


```r
# select two parameters from dat
subset(dat, select = c('rh', 'bp'))

# subset records greater than or equal to a date
subset(dat, subset = '2013-01-01 0:00', operator = '>=')

# subset records within a date range
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'))

# subset records within a date range, select two parameters
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
  select = c('atemp', 'totsorad'))
```

The `setstep` function formats a swmpr object to a continuous time series at a given time step.  This function is not necessary for most stations but can be useful for combining data, see below.

The `comb` function

##Functions

Three main categories of functions are available: retrieve, organize, and analyze.  Other miscellaneous functions are helpers/wrappers to these  functions or those used to obtain metadata.

<b>retrieve</b>

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

`import_local` Import files from a local path.  The files must be in a specific format, specifically those returned from the CDMO using the <a href="http://cdmo.baruch.sc.edu/aqs/zips.cfm">zip downloads</a> option for a reserve.

<b>organize</b>

`qaqc.swmpr` Remove QAQC columns and remove data based on QAQC flag values for a swmpr object.  Only applies if QAQC columns are present.  

`subset.swmpr` Subset by dates and/or columns for a swmpr object.  This is a method passed to the generic `subset' function provided in the base package.

`setstep.swmpr` Format data from a swmpr object to a continuous time series at a given timestep.  The function is used in `comb.swmpr` and can also be used with individual stations.

`comb.swmpr` Combines swmpr objects to a common time series using setstep, such as combining the weather, nutrients, and water quality data for a single station. Only different data types can be combined.

<b>analyze</b> 

Not yet available.

<b>miscellaneous</b>

`swmpr` Creates object of swmpr class, used internally in retrieval functions.

`parser` Parses html returned from CDMO web services, used internally in retrieval functions.

`time_vec` Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions.

`site_codes` Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services.

`site_codes_ind` Metadata for all stations at a single site, wrapper  to `NERRFilterStationCodesXMLNew` function on web services.

`param_names` Vector of column names for a given parameter type (nutrients, weather, or water quality).  Includes QAQC columns with 'F_' prefix. Used internally in retrieval functions.

##Files

`funcs.r` Required functions, all categories.

`dev.r` Temporary file used for development.

`test_retrieval.r` Evaluation of retrieval functions.

`test_organize.r` Evaluation of organize funcitons.

`.Rprofile` File that is run after opening the project in R, contains all package dependencies.

`.gitignore` List of files that Git ignores during commits/build, not on repository.

`SWMPr.Rproj` RStudio project file used to create the package.

##Forthcoming

Actual 'package' repository after all functions are complete.

Organize functions...  test setstep, com, remove time_dum variable from setstep after testing

Analysis functions... EDA, metab, trend analysis, tidal decomp, etc.

DOI/release info when done (see <a href="http://computationalproteomic.blogspot.com/2014/08/making-your-code-citable.html">here</a>)
