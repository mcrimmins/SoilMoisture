# Get, plot, analyze Walnut Gulch data
# ftp://www-ftp.tucson.ars.ag.gov/metDAP/
# MAC 02/21/2019

# 3001 Lucky Hills Meteorological Site
# Years 1990 - present
# Data
# Meteorological: Air Temperature Ta
# Relative Humidity RH
# Wind Speed Ws
# Wind Direction Wd
# Barometric Pressure Bar
# Solar Radiation Sol
# Photosynthetically Active Radiation PAR
# Net Radiation Rnet
# Soil Hydrology: Soil Heat Flux SHF
# Soil Moisture 5 cm SM5
# Soil Moisture 15 cm SM15
# Soil Temperature 1 cm ST1
# Soil Temperature 2 cm ST2
# Soil Temperature 6 cm ST6
# Soil Temperature surface STsur

library(data.table)

# Get Met Data
lm9001 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm9001.out')
lm0107<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm0107.out')
lm0809<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm0809.out')
lm1011<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm1011.out')
lm12<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm12.out')
lm13<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm13.out')
lm14<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm14.out')
lm15<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm15.out')
lm16<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm16.out')
lm17<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalmiteData/lm17.out')

# combine all met files
allMetData<-rbind(lm9001,lm0107,lm0809,lm1011,lm12,lm13,lm14,lm15,lm16,lm17)
# add col names
colnames(allMetData)<-c('Site','Year','Day','Hour','Min','Ta','RH','Ws','Wd','Bar','Sol','PAR','Rnet')
# add date/time column
#allMetData$DateTime<-as.POSIXct(paste0(allMetData$Year,'-',allMetData$Day,' ',allMetData$Hour,':',allMetData$Min,':00'), format="%Y-%j %H:%M:%S", tz='MST')
allMetData$DateTime<-as.Date(paste0(allMetData$Year,'-',allMetData$Day,' ',allMetData$Hour,':',allMetData$Min,':00'), format="%Y-%j %H:%M:%S", tz='MST')
# replace missing vals
allMetData[allMetData==6999] <- NA
# summarize by day
aggregate(allMetData["value"], by=energy["Date"], sum)

# Get Soil Water Data
ls9601 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls9601.out')
ls0107<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls0107.out')
ls0809<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls0809.out')
ls1011<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls1011.out')
ls12<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls12.out')
ls13<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls13.out')
ls14<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls14.out')
ls15<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls15.out')
ls16<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls16.out')
ls17<-fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/MeteorologicalSiteData/ls17.out')

# combine all met files
allSWData<-rbind(ls9601,ls0107,ls0809,ls1011,ls12,ls13,ls14,ls15,ls16,ls17)
# add col names
colnames(allSWData)<-c('Site','Year','Day','Hour','Min','SHF','SM5','SM15','ST1','ST2','ST6','STsur')
# add date/time column



