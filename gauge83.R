# get Gauge 83 precip and soil moisture
# MAC 02/25/19

library(data.table)
library(dplyr)
library(SPEI)

# Get Met Data
gg83 <- fread('https://cals.arizona.edu/climate/misc/WGEW_Gauge83.csv')
# trim extra cols
gg83<-gg83[,-c(4,5,6)]
# add dates
gg83$dates<-as.Date(paste0(gg83$Year,"/",gg83$Month,"/1"), format = "%Y/%m/%d")

# create date sequence
## first days of years
beg<-paste0(gg83$Year[1],"/",gg83$Month[1],"/1")
end<-paste0(gg83$Year[nrow(gg83)],"/",gg83$Month[nrow(gg83)],"/1")
allDates<-seq(as.Date(beg), as.Date(end), "months")

allDates<-as.data.frame(allDates)
colnames(allDates)[1]<-"dates"

# merge with dates
precipData<-merge(gg83, allDates, by="dates", all.y = TRUE)

# Get SM Data rg*vt020715.out
rg83vt020715 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt020715.out')
rg83vt080915 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt080915.out')
rg83vt101115 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt101115.out')
rg83vt1215 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1215.out')
rg83vt1315 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1315.out')
rg83vt1415 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1415.out')
rg83vt1515 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1515.out')
rg83vt1615 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1615.out')
rg83vt1715 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1715.out')

# Get SM Data rg*vt020715.out
rg83vt020730 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt020730.out')
rg83vt080930 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt080930.out')
rg83vt101130 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt101130.out')
rg83vt1230 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1230.out')
rg83vt1330 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1330.out')
rg83vt1430 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1430.out')
rg83vt1530 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1530.out')
rg83vt1630 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1630.out')
rg83vt1730 <- fread('ftp://www-ftp.tucson.ars.ag.gov/metDAP/RaingageSiteData/rg83vt1730.out')

# combine all SM15 files
allSM15<-rbind(rg83vt020715,rg83vt080915,rg83vt101115,rg83vt1215,rg83vt1315,rg83vt1415,rg83vt1515,rg83vt1615,rg83vt1715)
rm(rg83vt020715,rg83vt080915,rg83vt101115,rg83vt1215,rg83vt1315,rg83vt1415,rg83vt1515,rg83vt1615,rg83vt1715)
# add col names
colnames(allSM15)<-c('Site','Year','Day','HR','MN','SM15','ST15')

# combine all SM30 files
allSM30<-rbind(rg83vt020730,rg83vt080930,rg83vt101130,rg83vt1230,rg83vt1330,rg83vt1430,rg83vt1530,rg83vt1630,rg83vt1730)
rm(rg83vt020730,rg83vt080930,rg83vt101130,rg83vt1230,rg83vt1330,rg83vt1430,rg83vt1530,rg83vt1630,rg83vt1730)
# add col names
colnames(allSM30)<-c('Site','Year','Day','HR','MN','SM30','ST30')

# add 30SM to 15SM
allSM<-cbind(allSM15,allSM30)
# drop redundant cols
allSM<-allSM[,-c(8:12)]

# add 
allSM$DateTime<-as.Date(paste0(allSM$Year,'-',allSM$Day,' ',allSM$HR,':',allSM$MN,':00'), format="%Y-%j %H:%M:%S", tz='MST')
# replace missing vals
allSM[allSM==6999] <- NA

# add month
allSM$month<-as.numeric(format(allSM$DateTime, "%m"))
# monthly averages
allSMbyMonth<-allSM %>% group_by(month, Year) %>% summarise(numObs=n(),SM15=mean(SM15, na.rm = TRUE),SM30=mean(SM30, na.rm=TRUE))
# add in date sequence
allSMbyMonth$date<-as.Date(paste0(allSMbyMonth$Year,"/",allSMbyMonth$month,"/1"),format = "%Y/%m/%d")
# reorder
allSMbyMonth <- allSMbyMonth[order(allSMbyMonth$date),] 
# drop first ob
allSMbyMonth<-allSMbyMonth[-1,]
# add in precip data
allSMbyMonth$precip<-precipData$total[465:654]
# add in monthly standardized SM
SM15_SI<-spi(ts(allSMbyMonth$SM15, freq=12, start=c(2002,3)), 1, na.rm = TRUE)
  allSMbyMonth$MSMI15<-SM15_SI$fitted
SM30_SI<-spi(ts(allSMbyMonth$SM30, freq=12, start=c(2002,3)), 1, na.rm = TRUE)
  allSMbyMonth$MSMI30<-SM30_SI$fitted
# convert precip to SPI
spi1<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 1, na.rm = TRUE)
  allSMbyMonth$spi1<-spi1$fitted
spi2<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 2, na.rm = TRUE)
  allSMbyMonth$spi2<-spi2$fitted
spi3<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 3, na.rm = TRUE)
  allSMbyMonth$spi3<-spi3$fitted
spi6<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 6, na.rm = TRUE)
  allSMbyMonth$spi6<-spi6$fitted
spi12<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 12, na.rm = TRUE)
  allSMbyMonth$spi12<-spi12$fitted
spi18<-spi(ts(allSMbyMonth$precip, freq=12, start=c(2002,3)), 18, na.rm = TRUE)
  allSMbyMonth$spi18<-spi18$fitted
  
# cor matrix
  cor(allSMbyMonth[,8:15], use = "na.or.complete", method = "spearman")
# plot high corr pair
  plot(allSMbyMonth$spi18,allSMbyMonth$MSMI30)
    
