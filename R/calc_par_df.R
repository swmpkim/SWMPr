# this is not yet set up as a function
# originally it was a function that applied calc_modeled_par() to a 
# whole data frame
# but i wonder if they could be combined into one function?

# possible problem: column names are hard-coded to swmp column names
# which is great for those of us in swmp
# but is there a way to allow more flexible inputs, without requiring 
# a zillion function arguments?



# inputs need to be:
# data: data frame, with swmp column names.... may not have to be an actual swmpr object?
# station_code: SWMP station code; if NULL, lat, long, and longTZ must be provided
# hourly: option to only use readings at the top of the hour; better for lots of data; set default to TRUE

dat <- data


##### location information for model

# lat, long, longTZ; based on SWMP station code
longTZoffsets <- data.frame(gmt_off = c(-4, -5, -6, -7, -8, -9, -10),
                            longTZ = c(60, 75, 90, 105, 120, 135, 150))

if(!is.null(station_code)){
  # NEED A TEST HERE - MAKE SURE STATION CODE IS KNOWN BY SWMPr
  # and if it's not, user must enter lat_decdeg, long_decdeg, and longTZ
  short_code <- substr(station_code, start = 1, stop = 5)
  loc_dat <- stat_locs[stat_locs$station_code == short_code, ]
  lat_decdeg <- loc_dat$latitude
  long_decdeg <- -1 * loc_dat$longitude
  longTZ <- longTZoffsets$longTZ[longTZoffsets$gmt_off == loc_dat$gmt_off]
}


#### modify data frame

# dat <- qaqc(apaebmet)


cols_to_keep <- c("datetimestamp", "atemp", "rh", "bp", "totpar")
dat <- dat[ , names(dat) %in% cols_to_keep]

dat$date <- as.Date(substr(as.character(dat$datetimestamp), start = 1, stop = 10))
dat$time_hrs <- as.numeric(substr(as.character(dat$datetimestamp), start = 12, stop = 13))
dat$time_mins <- as.numeric(substr(as.character(dat$datetimestamp), start = 15, stop = 16))
dat$time_hrs <- dat$time_hrs + dat$time_mins/60

# only keep hourly readings if an option is set
if(hourly == TRUE){
  dat2 <- dat[dat$time_mins == 0, ]
} else {dat2 <- dat}


# pull out julian day of year
# leap years go from 1-366 (doy 60 = Feb 29); 
# others go 1-365 (doy 60 = Mar 1)
dat2$doy <- as.numeric(format(dat2$date, "%j"))

# convert SWMP PAR into model units
dat2$comp_par = dat2$totpar * 1.11

dat2$lat_decdeg <- lat_decdeg
dat2$long_decdeg <- long_decdeg
dat2$longTZ <- longTZ

# is it really this simple??? getting some NaNs where i wouldn't expect them
# but otherwise looks okay
calc_par <- with(dat2, mapply(calc_modeled_par,
                  doy = doy,
                  time_hrs = time_hrs,
                  rh = rh,
                  temp = atemp,
                  bp_mb = bp,
                  lat_decdeg = lat_decdeg,
                  long_decdeg = long_decdeg,
                  longTZ = longTZ)
     )

return(calc_par)

# this is working
# dat2 %>%
#   dplyr::mutate(modpar = purrr::pmap(list(doy = doy,
#                             time_hrs = time_hrs,
#                             rh = rh,
#                             temp = atemp,
#                             bp_mb = bp,
#                             lat_decdeg = lat_decdeg,
#                             long_decdeg = long_decdeg,
#                             longTZ = longTZ),
#                        calc_modeled_par)) %>%
#   tidyr::unnest() %>%
#   View()

