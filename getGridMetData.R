# get gridmet data using geoknife
# MAC 08/26/19
# see https://owi.usgs.gov/R/training-curriculum/usgs-packages/geoknife-intro/

library(geoknife)

# First, you need to query for all web data
all_webdata <- query("webdata")
# Use the all_webdata object to create the fabric
us_meteorology <- webdata(all_webdata["University of Idaho Daily Meteorological data for continental US"])
# Now use query to see what variables are available
meteoro_vars <- query(us_meteorology, "variables")
meteoro_vars
# get times
#variables(us_meteorology) <- meteoro_vars[4]
#query(us_meteorology, "times")

# var names
 # "precipitation_amount"                      "max_relative_humidity"                     "min_relative_humidity"                    
 # "specific_humidity"                         "surface_downwelling_shortwave_flux_in_air" "min_air_temperature"                      
 # "max_air_temperature"                       "wind_speed"   

# Setup the fabric using the URL in all_webdata - variables can be named one by one if needed
gridmet_title <- "University of Idaho Daily Meteorological data for continental US"
gridmet_url <-  url(all_webdata[gridmet_title])
gridmet_fabric <- webdata(list(
  url = gridmet_url,
  variables = c("precipitation_amount"),
  times = as.POSIXct(c("1979-01-01", "2018-12-31"), tz = "UTC")
  #times = as.POSIXct(c("1979-01-01 12:01", "2018-12-31 11:59"), tz = "UTC")
))

# Now setup the stencil
coords<-c(-110.596385, 31.801464) # this can be a point or polygon
location <- simplegeom(coords)

# Execute the geoknife job
datatemp_job <- geoknife(stencil = location, fabric = gridmet_fabric, wait=FALSE)

# may take a couple of mins, you can check on job here; once it say 100% complete you can move on to download
check(datatemp_job)

# Download the data
gridmet_data <- result(datatemp_job)
