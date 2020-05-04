#' Calculate modeled photosynthetically active radiation  
#' 
#' Based on the clearskycalculator.com quantum sensor model
#'
#' @param doy Julian day of year  
#' @param time_hrs time of day, as whole or decimal hours  
#' @param rh relative humidity, percent
#' @param temp temperature, degrees Celsius
#' @param bp_mb barometric pressure, millibars
#' @param lat_decdeg latitude, in decimal degrees
#' @param long_decdeg longitude, in decimal degrees, but POSITIVE FOR WEST longitudes
#' @param longTZ center longitude of the time zone: 75, 90, 105, 120 for eastern, central, mountain, and pacific, respectively. Others can be found here: http://clearskycalculator.com/longitudeTZ.htm 
#'
#' @concept analyze
#'
#' @return
#' @export
#' 
#' @details This function models photosynthetically active radiation (PAR) for a given location, at a given point in time, based on other meteorological parameters that can affect radiation: day of year (which influences sun angle), temperature, relative humidity, and barometric pressure.  I ASSUMED SEA LEVEL; ELEVATION IS SOMETHING THAT MATTERS IN THE MODEL MORE BROADLY
#'
#' @return a single value. units are mmol/m^2/s
#' 
#' @references This is based on the model used on clearskycalculator.com.
#' website: http://clearskycalculator.com/quantumsensor.htm
#' ASCE report with derivation of model:
#' https://www.apogeeinstruments.com/content/EWRI-ASCE-Reference-ET.pdf
#' Appendices (Appendix D has equations used here):
#' https://www.apogeeinstruments.com/content/EWRI-ASCE-Reference-ET-Appendices.pdf
#'
#'
#' @examples
calc_modeled_par <- function(doy, time_hrs,
                             rh, temp, bp_mb,
                             lat_decdeg, long_decdeg, longTZ){
  
  # doy and time_hrs need to be pre-calculated
  # maybe later I can get this to use POSIXct to automate it, but not now
  
  # unit conversions
  bp_kPa <- bp_mb / 10
  lat_rad <- lat_decdeg * pi / 180
  
  # intermediate calculations
  # easy ones
  eT <- 0.6108 * exp( (17.27*temp) / (temp + 237.3) )
  ea <- (rh / 100) * eT
  prcp_water <- (0.14 * ea * bp_kPa) + 2.1
  
  # intermediate: Ra
  solar_decl <- 0.409 * sin( ((2*pi*doy)/365) - 1.39 )  # solar angle
  dr <- 1 + 0.033 * cos(2*pi*doy / 365)
  Gsc <- 1361  # solar constant, W/m^2
  ws <- acos( -tan(lat_rad) * tan(solar_decl) )   # sunset hour angle, radians
  b <- 2*pi*(doy-81)/364
  Sc <- 0.1645*sin(2*b) - 0.1255*cos(b) - 0.025*sin(b)
  w <- pi/12 * ( (time_hrs + 0.06667*(longTZ - long_decdeg) + Sc) - 12 )  # eq 54
  w1 <- w - (pi/24)  # this was pi * calc_period/24, but it works best on an hourly scale
  w2 <- w + (pi/24)
  if(w1 < -ws){w1 <- -ws}
  if(w2 < -ws){w2 <- -ws}
  if(w1 > ws){w1 <- ws}
  if(w2 > ws){w2 <- ws}
  if(w1 > w2){w1 <- w2}
  Ra <- 12/pi * Gsc * dr * ( ((w2-w1)*sin(lat_rad)*sin(solar_decl)) + (cos(lat_rad)*cos(solar_decl)*(sin(w2)-sin(w1)) ) )
  
  # Ra to Rso
  sin_sun <- sin(lat_rad)*sin(solar_decl) + cos(lat_rad)*cos(solar_decl)*cos(w)
  aa <- -0.00146*bp_kPa/sin_sun
  bb <- 0.075*(prcp_water/sin_sun)^0.4
  KB <- 0.98 * exp( aa - bb )
  KD <- 0.35 - 0.36*KB
  Rso <- (KB + KD) * Ra
  
  # unit conversion: Rso above is W/m^2 total solar radiation
  # we need micromoles/m^2/sec; one conversion is 4.57
  # the other is, only 45% of solar radiation is PAR
  
  Rso * 4.57 * 0.45
}