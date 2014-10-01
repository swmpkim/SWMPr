# SWMPr package for estuarine monitoring data

This repository contains materials to retrieve, organize, and analyze estuarine monitoring data from the System Wide Monitoring Program (<a href="http://nerrs.noaa.gov/RCDefault.aspx?ID=18">SWMP</a>) implemented by the National Estuarine Research Reserve System (<a href="http://nerrs.noaa.gov/">NERRS</a>).  SWMP was initiated in 1995 to provide continuous monitoring data at over 300 stations in 28 estuaries across the United States.  SWMP data are maintained online by the Centralized Data Management Office (CDMO). This R package provides several functions to retrieve, organize, and analyze estuary data from the CDMO.  Information on the CDMO web services are available <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  Your computer's IP address must be registered with the CDMO website to use most of the data retrieval functions, see contact info in the link.  All other functions can be used after obtaining data from the CDMO, as described below. 

The package has many dependencies, the most important being the SSOAP package for retrieving data from the CDMO using a SOAP client interface. The SSOAP package is not required to use the package but is necessary for using most of the data retrieval functions, see below.  The SSOAP package is currently removed from CRAN but accessible at <a href="http://www.omegahat.org/SSOAP/">http://www.omegahat.org/SSOAP/</a>.  It can be installed as follows:


```r
install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T,  type =  "source")
```

All data obtained from the CDMO should be <a href="http://cdmo.baruch.sc.edu/data/citation.cfm">cited</a> using the format:

National Estuarine Research Reserve System (NERRS). 2012. System-wide Monitoring Program. Data accessed from the NOAA NERRS Centralized Data Management Office website: http://cdmo.baruch.sc.edu/; accessed 12 October 2012.

To cite this package:

Beck MW. 2014. SWMPr: An R package for the National Estuarine Research Reserve System.  Beta version. https://github.com/fawda123/SWMPr

##Install the Package

This package is currently under development and will later be uploaded as an official package repository.  This will allow use of the `install.packages` function for direct install within R.  For now, Github users can fork and pull the materials to create a local clone of the current form.  Otherwise, the `funcs.r` file contains all currently developed (and partially tested) functions.  Package dependencies can be found in `.RProfile`.  Files ending in `retrieval.r` were used to test the functions.   

##Data retrieval

SWMP data can be obtained directly from the CDMO through an online query or by using the retrieval functions provided in this package.  In the latter case, the IP address for the computer making the request must be registered with CDMO.  This can be done by following instructions <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  The <a href="http://cdmo.baruch.sc.edu/data/metadata.cfm">metadata</a> should also be consulted to determine the parameters and date ranges that are available for each station.  Metadata are included as a .csv file with data requested from the CDMO and can also be obtained using the `site_codes` (all sites) or `site_codes_ind` (individual site) functions.  


```r
# retrieve metadata for all sites
site_codes()

# retrieve metadata for a single site
site_codes_ind('apa')
```

Due to rate limitations on the server, the retrieval functions in this package return a limited number of records.  The functions are more amenable to analyses with short time periods, although these functions could be used iteratively (i.e., with `for` or `while` loops) to obtain longer time series.  Data retrieval functions to access the CDMO directly include `all_params`, `all_params_dtrng`, and `single_param`.  `all_params` returns the most recent 100 records of all parameters at a station.  `all_params_dtrng` returns all records within a date range for all parameters or a single parameter.  `single_param` is identical to `all_params` except that a single parameter is requested.    


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

For larger requests, it is easier to obtain data outside of R using the online query system on the CDMO.  Data can be retrieved from the CDMO several ways.  Data from single stations can be requested from the <a href="http://cdmo.baruch.sc.edu/get/export.cfm">data export system</a>, whereas data from multiple stations can be requested from the <a href="http://cdmo.baruch.sc.edu/aqs/">advanced query system</a>.  The `import_local` function is used to import data into R that were downloaded from the CDMO with the <a href="http://cdmo.baruch.sc.edu/aqs/zips.cfm">zip downloads</a> feature available within the advanced query system.  The downloaded data will include multiple .csv files by year for a given data type (e.g., apacpwq2002.csv, apacpwq2003.csv, apacpnut2002.csv, etc.).  It is recommended that all parameters and the complete date range are requested to avoid repeated requests to CDMO.  The `import_local` function can be used once the downloaded files are extracted to a local path. 


```r
# import data from a folder with csv files from CDMO
import_local('zip_ex', 'apaebmet') 
```

In all cases, the imported data need to assigned to an object in the workspace for use with other functions:




```r
# import data
dat <- import_local('zip_ex', 'apaebmet', trace = F) 
```

##swmpr object class

All data retrieval functions return a swmpr object that includes relevant data and several attributes describing the dataset.  The data include a datetimestamp column converted to the appropriate timezone for a station.  Note that the datetimestamp is standard time for each timezone and does not include daylight savings. Additional columns include parameters for a given data type and correspondingg QAQC columns if returned from the initial data request.  The attributes for a swmpr object include `names` of the dataset (`station_data`), `class` (swmpr) `station name` (7 or 8 characters), `qaqc_cols` (logical), and `date_rng` (POSIXct vector), and `parameters` (character vector).  Attributes can be viewed as follows:


```r
# all attributes
attributes(dat)

# a single attribute
attr(dat, 'station')
```

The data for a swmpr object can be assigned to an additional object in the workspace if working directly with the data is preferred (e.g., as a data frame).  However, the swmpr object class was created for use with specific methods and it is suggested that these methods be used for organization and analysis.  Available methods can be viewed:


```r
methods(class = 'swmpr')
```

##swmpr methods

The organize and analyze functions are methods that can be applied to a swmpr object.  The organize functions are used to clean or prepare the data for analysis, including removal of QAQC flags, subsetting, or combining data of different types.  The `qaqc` function is a simple screen to retain values from the data with specified QAQC flags, described <a, href="http://cdmo.baruch.sc.edu/data/qaqc.cfm">here</a>.  Each parameter in the swmpr data typically has a corresponding QAQC column of the same name with the added prefix 'f_'.  Values in the QAQC column specify a flag from -5 to 5.  Generally, only data with the '0' QAQC flag should be used, which is the default option for the `qaqc` function.  Data that do not satisfy QAQC criteria are converted to NA values.  


```r
# qaqc screen for a swmpr object, retain only '0'
qaqc(dat)

# retain all
qaqc(dat, qaqc_keep = NULL)

# retain only '0' and '-1'
qaqc(dat, qaqc_keep = c(0, -1))
```

Note the `qaqc_cols` attribute for the data before and after use of the `qaqc` function.  Processed data will have QAQC columns removed, in addition to removal of values in the actual parameter columns that do not meet the criteria.

The `subset` function is a method for swmpr objects added to the existing generic subset function.  This function is used to subset the data by date and/or a selected parameter.  The date can be a single value or as two dates to select records within the range. The former case requires a binary operator input as a character string passed to the argument, such as `>` or `<`.  The subset argument for the date(s) must also be a character string of the format `YYYY-mm-dd HH:MM`.


```r
# subset by two parameters
subset(dat, select = ')

# subset records greater than a date
subset(dat, subset = '2014

# subset records within a date range
```

##Functions

Three main categories of functions are available: retrieve, organize, and analyze.  Other miscellaneous functions are helpers/wrappers to these  functions or those used to obtain site/station metadata.

<b>retrieve</b>

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

`import_local` Import files from a local path.  The files must be in a specific format, specifically those returned from the CDMO using the <a href="http://cdmo.baruch.sc.edu/aqs/zips.cfm">zip downloads</a> option for a reserve.

<b>organize</b>

`qaqc.swmpr` Remove QAQC columns and remove data based on QAQC flag values for a swmpr object.  This function is used on data returned from any of the retrieval functions.  Only applies if QAQC columns are present.  

`subset.swmpr` Subset by dates and/or columns for a swmpr object dataset.  This is a method passed to the generic `subset' function provided in the base package.

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

Organize functions... test/debug subset function, combine functions with common time step, CDMO and remote data

Analysis functions... EDA, metab, trend analysis, etc.
