# read in HYDRUS output, calculate standardized soil moisture indices
# calculate correlation maps
# 10/17/16 MAC
# updated 11/4/16

library(ggplot2)
library(gridExtra)
library(plyr)
library(ggplot2)
library(reshape)
library(dplyr)
library(SPEI)

# set working dir
setwd("./hydrus/Tucson")

# read in Obs_Node.out
theta<-read.table("Obs_Node.out", skip=10, fill=TRUE)
theta<-theta[,seq(3,84,3)] # select theta columns
colnames(theta)<-sprintf('X%i',c(seq(5,100,by=5),seq(150,500,by=50)))
theta<-theta[-(1:length(seq(as.Date("1950/1/1"), as.Date("1959/12/31"), "days"))),]
theta<-theta[-nrow(theta),]# trim last row
theta$date<-seq(as.Date("1950/1/1"), as.Date("2015/12/31"), "days")
theta$month<-as.numeric(format(as.Date(theta$date), "%m"))
theta$year<-as.numeric(format(as.Date(theta$date), "%Y"))

# read in Meteo.obs
names<-scan("Meteo.out", skip = 1, nlines = 1,what = character())    
meteo<-read.fwf("Meteo.out", skip = 4, width = c(9,10,10,9,rep(10,7)),col.names = names)
meteo<-meteo[-nrow(meteo),]# trim last row
meteo<-meteo[-(1:length(seq(as.Date("1950/1/1"), as.Date("1959/12/31"), "days"))),]
meteo$date<-seq(as.Date("1950/1/1"), as.Date("2015/12/31"), "days")

# create dates
theta$Date<-as.Date(strptime(paste0(theta$Month,"-",theta$Day,"-",theta$Year), format="%m-%d-%Y"))
theta$precip<-meteo$Prec # add in precip...in mm
theta$ET<-meteo$ET # in mm

# pivot to get monthly summary...monthly stats? median/sum...variability metrics?
moSoil<-group_by(theta,month,year)
moSoil<-summarise(moSoil,sumTheta=sum(X10),avgTheta=mean(X10), sumPrecip=sum(precip, na.rm=TRUE),sumET=sum(ET, na.rm=TRUE),
                  rainDays=sum(precip>0, na.rm=TRUE))
moSoil<- moSoil[order(moSoil$year,moSoil$month),]
moSoil$sdii<-moSoil$sumPrecip/moSoil$rainDays
moSoil$avgThetaDiff<-c(0,diff(moSoil$avgTheta,1))

yrSoil<-group_by(theta,year)
yrSoil<-summarise(yrSoil,sumTheta=sum(X10),avgTheta=mean(X10), sumPrecip=sum(precip, na.rm=TRUE),sumET=sum(ET, na.rm=TRUE),
                  rainDays=sum(precip>0, na.rm=TRUE))
yrSoil<- yrSoil[order(yrSoil$year),]

moSoil2<-group_by(theta,month,year)
moSoil2<-summarise(moSoil2,sumTheta=sum(X30),avgTheta=mean(X30), sumPrecip=sum(precip, na.rm=TRUE),sumET=sum(ET, na.rm=TRUE))
moSoil2<- moSoil2[order(moSoil2$year,moSoil2$month),]
moSoil2$avgThetaDiff<-c(0,diff(moSoil2$avgTheta,1))

moSoil3<-group_by(theta,month,year)
moSoil3<-summarise(moSoil3,sumTheta=sum(X50),avgTheta=mean(X50), sumPrecip=sum(precip, na.rm=TRUE),sumET=sum(ET, na.rm=TRUE))
moSoil3<- moSoil3[order(moSoil3$year,moSoil3$month),]
moSoil3$avgThetaDiff<-c(0,diff(moSoil3$avgTheta,1))

# using mix of log-logistic and max lik
ztheta1<-spei(ts(moSoil$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'log-Logistic', fit =   'max-lik')
ztheta2<-spei(ts(moSoil2$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'log-Logistic', fit =   'max-lik')
ztheta3<-spei(ts(moSoil3$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'log-Logistic', fit =   'max-lik' )
# using spi defaults --- does not work well
# ztheta1<-spi(ts(moSoil$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'Gamma', fit =   'ub-pwm')
# ztheta2<-spi(ts(moSoil2$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'Gamma', fit =   'ub-pwm')
# ztheta3<-spi(ts(moSoil3$avgTheta,freq=12,start=c(1950,1)),1,distribution = 'Gamma', fit =   'ub-pwm')


# SPI/SPEI plots
# spi6<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),6,distribution = 'log-Logistic')
# par(mfrow=c(2,1))
# plot(spi6)
# plot(ztheta2)

# histograms by month
# p <- ggplot(moSoil, aes(as.factor(month), sumPrecip))
# plot1<-p + geom_boxplot()+labs(title = "El Paso Avg Monthly Precip Stats and 10cm Theta by Month")
# p <- ggplot(moSoil, aes(as.factor(month), rainDays))
# plot2<-p + geom_boxplot()
# p <- ggplot(moSoil, aes(as.factor(month), sdii))
# plot3<-p + geom_boxplot()
# p <- ggplot(moSoil, aes(as.factor(month), sumET))
# plot4<-p + geom_boxplot()
# p <- ggplot(moSoil, aes(as.factor(month), avgTheta))
# plot5<-p + geom_boxplot()
# p <- ggplot(moSoil, aes(as.factor(month), avgThetaDiff))
# plot6<-p + geom_boxplot()
# grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=6)

# month to month difference histograms
# p <- ggplot(moSoil, aes(as.factor(month), avgThetaDiff)) +
#   geom_hline(yintercept=0)
# plot1<-p + geom_boxplot()+labs(title = "Winslow Theta Diffs by Month (10/30/50cm)")
# p <- ggplot(moSoil2, aes(as.factor(month), avgThetaDiff)) +
#   geom_hline(yintercept=0)
# plot2<-p + geom_boxplot()
# p <- ggplot(moSoil3, aes(as.factor(month), avgThetaDiff)) +
#   geom_hline(yintercept=0)
# plot3<-p + geom_boxplot()
# grid.arrange(plot1,plot2,plot3,nrow=3)


# develop heat map correlations of all SPI/SPEI timescales against different soil depths ----   
# loop through SPI time scales
# model diagnostics
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2,na.rm = TRUE))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error), na.rm = TRUE)
}
# add in option to subset data -- early vs late period...
# 03/13/19 -- correlation results depend on distribution and fit parameters Gamma/log-logistic and max-lik/ub-pwm

fullCorr<-data.frame(scale=rep(0,48), corr=rep(0,48))
for(i in 1:48){
  spiValues<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),i,distribution = 'log-Logistic', fit = "max-lik") # changed to max-lik on 2/26/18
  #spiValues<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),i,distribution = 'Gamma', fit = "ub-pwm") #
    #spiValues<-spei(ts(moSoil$sumPrecip-moSoil$sumET,freq=12,start=c(1950,1)),i) # should this be log-logistic?
  temp<-as.data.frame(cbind(moSoil$month,ztheta2$fitted,spiValues$fitted))
  colnames(temp)<-c("month","theta","spi")
  tempCorr<-ddply(temp, .(month), summarise, "corr" = cor(theta, spi, method = "pearson", use = "na.or.complete"))
  tempCorr$scale<-i
  # normality tests - following Wu et al. 2007 https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.1371
  tempDiagsSPI<-ddply(temp, .(month), summarise, "medianSPI" = median(spi, na.rm = TRUE))
    tempDiagsSPI$scale<-i
  tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(spi)[2])
    tempSW$swSPIp<-unlist(tempSW$value)
  tempDiagsSPI$swSPIp<-tempSW$swSPIp  
  tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(spi)[1])
    tempSW$swSPI<-unlist(tempSW$value)
    tempDiagsSPI$swSPI<-tempSW$swSPI 
  # normality of theta obs
  DiagsTheta<-ddply(temp, .(month), summarise, "medianTheta" = median(theta, na.rm = TRUE))   
    DiagsTheta$scale<-i 
    tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(theta)[2])
    tempSW$swSPI<-unlist(tempSW$value)
    tempSW$scale<-i
  DiagsTheta$swThetaP<-tempSW$swSPI
    tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(theta)[1])
    tempSW$swSPI<-unlist(tempSW$value)
    tempDiagsSPI$swSPI<-tempSW$swSPI 
  DiagsTheta$swTheta<-tempSW$swSPI
    
  # full record corr
  fullCorr$scale[i]<-i
  fullCorr$corr[i]<-cor(temp$theta,temp$spi,method = "pearson", use = "na.or.complete")
  fullCorr$rmse[i]<-rmse(temp$theta-temp$spi)
  fullCorr$mae[i]<-mae(temp$theta-temp$spi)
  fullCorr$bias[i]<-mean(temp$spi, na.rm=TRUE)-mean(temp$theta, na.rm=TRUE)
  fullCorr$medianSPI[i]<-median(temp$spi, na.rm=TRUE)
  fullCorr$swSPI[i]<-shapiro.test(temp$spi)[["statistic"]][["W"]]
  fullCorr$swSPIp[i]<-unlist(shapiro.test(temp$spi)[2])
  
  # add in rmse, bias
  if(i==1){
    allCorrs<-tempCorr
    allDiagsSPI<-tempDiagsSPI
  }else{
    allCorrs<-rbind(allCorrs,tempCorr)
    allDiagsSPI<-rbind( allDiagsSPI,tempDiagsSPI)
  }
}

# loop through SPEI time scales
fullCorrSPEI<-data.frame(scale=rep(0,48), corr=rep(0,48))
for(i in 1:48){
  #spiValues<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),i,distribution = 'log-Logistic')
  spiValues<-spei(ts(moSoil$sumPrecip-moSoil$sumET,freq=12,start=c(1950,1)),i) # should this be log-logistic?
  temp<-as.data.frame(cbind(moSoil$month,ztheta2$fitted,spiValues$fitted))
  colnames(temp)<-c("month","theta","spi")
  tempCorr<-ddply(temp, .(month), summarise, "corr" = cor(theta, spi, method = "pearson", use = "na.or.complete"))
  tempCorr$scale<-i
  # normality tests - following Wu et al. 2007 https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.1371
  tempDiagsSPI<-ddply(temp, .(month), summarise, "medianSPI" = median(spi, na.rm = TRUE))
  tempDiagsSPI$scale<-i
  tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(spi)[2])
  tempSW$swSPIp<-unlist(tempSW$value)
  tempDiagsSPI$swSPIp<-tempSW$swSPIp  
  tempSW<-ddply(temp, .(month), summarise, "value" = shapiro.test(spi)[1])
  tempSW$swSPI<-unlist(tempSW$value)
  tempDiagsSPI$swSPI<-tempSW$swSPI
  
  # full record corr
  fullCorrSPEI$scale[i]<-i
  fullCorrSPEI$corr[i]<-cor(temp$theta,temp$spi,method = "pearson", use = "na.or.complete")
  fullCorrSPEI$rmse[i]<-rmse(temp$theta-temp$spi)
  fullCorrSPEI$mae[i]<-mae(temp$theta-temp$spi)
  fullCorrSPEI$bias[i]<-mean(temp$spi, na.rm=TRUE)-mean(temp$spi, na.rm=TRUE)
  fullCorrSPEI$medianSPI[i]<-median(temp$spi, na.rm=TRUE)
  fullCorrSPEI$swSPI[i]<-shapiro.test(temp$spi)[["statistic"]][["W"]]
  fullCorrSPEI$swSPIp[i]<-unlist(shapiro.test(temp$spi)[2])
  
  if(i==1){
    allCorrsSPEI<-tempCorr
    allDiagsSPEI<-tempDiagsSPI
  }else{
    allCorrsSPEI<-rbind(allCorrsSPEI,tempCorr)
    allDiagsSPEI<-rbind( allDiagsSPEI,tempDiagsSPI)
  }
}

# ggplot heat map
#allCorrs$month <- factor(allCorrs$month, levels=(allCorrs$month)[order(c("10","11","12","1","2","3","4","5","6","7","8","9"))])
allCorrs<-allCorrs[which(allCorrs$scale<=24),]
ggplot(allCorrs, aes(scale, month)) +
  geom_tile(aes(fill = corr), color="grey") +
  scale_fill_gradient2(low = "white",mid="yellow",high = "red",midpoint = 0.5, limits=c(0,1), name="Pearson") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(1,12,1), labels = seq(1,12,1)) +
  labs(title = "Tucson - 30cm Monthly Avg Theta vs SPI - Pearson Correlations")+
  xlab("timescale (months)")+
  ylab("month")+
  theme_bw()



# heat map of monthly differences
allCorrs$diff<-allCorrs$corr-allCorrsSPEI$corr
ggplot(allCorrs, aes(scale, month)) +
  geom_tile(aes(fill = diff), color="white") +
  scale_fill_gradient2(low = "blue",mid="white",high = "red",midpoint = 0.0, limits=c(-0.2,0.2), name="Pearson") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(1,12,1), labels = seq(1,12,1)) +
  labs(title = "Tucson - 30cm Monthly Avg Theta vs SPI/SPEI Differences - Pearson")


# NORMALITY DIAG plots ----
fullCorr$Normality<-ifelse(abs(fullCorr$medianSPI)>0.05 & fullCorr$swSPI<0.96 & fullCorr$swSPIp<0.1, "Non-normal","Normal")
allDiagsSPI$Normality<-ifelse(abs(allDiagsSPI$medianSPI)>0.05 & allDiagsSPI$swSPI<0.96 & allDiagsSPI$swSPIp<0.1, "Non-normal","Normal")
allDiagsSPEI$Normality<-ifelse(abs(allDiagsSPEI$medianSPI)>0.05 & allDiagsSPEI$swSPI<0.96 & allDiagsSPEI$swSPIp<0.1, "Non-normal","Normal")
DiagsTheta$Normality<-ifelse(abs(DiagsTheta$medianTheta)>0.05 & DiagsTheta$swTheta<0.96 & DiagsTheta$swThetaP<0.1, "Non-normal","Normal")
# all zThetas
AllThetaDiags<-as.data.frame(rbind(cbind(median(ztheta1$fitted, na.rm=TRUE), shapiro.test(ztheta1$fitted)[["statistic"]][["W"]], unlist(shapiro.test(ztheta1$fitted)[2])),
cbind(median(ztheta2$fitted, na.rm=TRUE), shapiro.test(ztheta2$fitted)[["statistic"]][["W"]], unlist(shapiro.test(ztheta2$fitted)[2])),
cbind(median(ztheta3$fitted, na.rm=TRUE), shapiro.test(ztheta3$fitted)[["statistic"]][["W"]], unlist(shapiro.test(ztheta3$fitted)[2]))),
row.names = c("theta1","theta2","theta3"), col.names=c("median","sw","swP"))
  colnames(AllThetaDiags)<-c("median","sw","swP")
AllThetaDiags$Normality<-ifelse(abs(AllThetaDiags$median)>0.05 & AllThetaDiags$sw<0.96 & AllThetaDiags$swP<0.1, "Non-normal","Normal")
  
# plot timescale vs correlation - KEY PLOT
plot(fullCorr$scale, fullCorr$medianSPI, type="l", col="blue", ylim=c(-0.1,0.1), ylab = "Pearson r", xlab = "scale (mos)")
lines(fullCorrSPEI$scale,fullCorrSPEI$medianSPI, type="l", col="red")
legend(38,1, c("SPI","SPEI"), lty=c(1,1), col=c("blue","red"))
title(main="Tucson - SPI/SPEI median values")
abline(h=0, col="black")

# ggplot heat map of diagnostics median/SW
diagVar<-allDiagsSPI
diagVar<-diagVar[which(diagVar$scale<=24),]
ggplot(diagVar, aes(scale, month)) +
  geom_tile(aes(fill = medianSPI), color="grey") +
  scale_fill_gradient2(low = "blue",mid="white",high = "red",midpoint = 0, limits=c(-0.1,0.1), name="Median") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(1,12,1), labels = seq(1,12,1)) +
  labs(title = "Tucson - median of SPI")+
  xlab("timescale (months)")+
  ylab("month")+
  theme_bw()

# ggplot heat map of normal/non normal
diagVar<-allDiagsSPI
diagVar<-diagVar[which(diagVar$scale<=24),]
ggplot(diagVar, aes(scale, month)) +
  geom_tile(aes(fill = Normality), color="grey") +
  #scale_fill_gradient2(low = "blue",mid="white",high = "red",midpoint = 0, limits=c(-0.1,0.1), name="Median") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks = seq(1,12,1), labels = seq(1,12,1)) +
  labs(title = "Tucson - Normality Test")+
  xlab("timescale (months)")+
  ylab("month")+
  theme_bw()


# ---- END of DIAG plots



# DIAGNOSTIC TIME SERIES PLOTS
# time series plots of highest correlating month/spi combos
spi<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),1,distribution = 'log-Logistic')
spiMonth<-ts(spi$fitted[cycle(spi$fitted) == 6]) # choose month
thetaMonth<-ts(ztheta1$fitted[cycle(ztheta1$fitted) == 6])

par(mfrow=c(2,1))
plot(spiMonth,type="l",col="red")
lines(thetaMonth,col="green")
#legend(0,4,c("SPI","Z-Theta"),lty=c(1,1),col=c("red","green"))
plot(as.numeric(spiMonth),as.numeric(thetaMonth), type="p")
cor(as.numeric(spiMonth),as.numeric(thetaMonth), use = "na.or.complete", method = "pearson")

# plot all months
par(mfrow=c(2,1))
plot(as.numeric(spi$fitted)-as.numeric(ztheta1$fitted), type="l")
abline(h=0)
hist(as.numeric(spi$fitted)-as.numeric(ztheta1$fitted))

# plot timescale vs correlation - KEY PLOT
plot(fullCorr$scale, fullCorr$corr, type="l", col="blue", ylim=c(0,1), ylab = "Pearson r", xlab = "scale (mos)")
lines(fullCorrSPEI$scale,fullCorrSPEI$corr, type="l", col="red")
legend(38,1, c("SPI","SPEI"), lty=c(1,1), col=c("blue","red"))
title(main="Tucson - SPI/SPEI Timescale Correlations with 10cm Theta")

# seasonal decomposition 
x1<-ts(moSoil$avgTheta,start=c(1950,1), freq=12)
x2<-ts(ztheta1$fitted[1:792],start=c(1950,1), freq=12)
plot(stl(x1, s.window = "periodic")) # monthly avg theta
# spectral periodogram
spec.pgram(x1, taper=0, log="no")
spec.pgram(x1, spans=c(9,9), taper = 0, log="no")


# plot zTheta/SPI/SPEI time series plots
tscale=9
yr1=2011.5
yr2=2013
spiPlot<-spi(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)),tscale,distribution = 'log-Logistic')
speiPlot<-spei(ts(moSoil$sumPrecip-moSoil$sumET,freq=12,start=c(1950,1)),tscale)
plot(spiPlot$fitted,type="l",col="blue", xlim=c(yr1,yr2),ylim=c(-3.5,3))
lines(speiPlot$fitted,col="red", xlim=c(yr1,yr2),ylim=c(-3.5,3))
lines(ztheta2$fitted,col="green", xlim=c(yr1,yr2),ylim=c(-3.5,3))
legend("topleft", c("SPI","SPEI","zTheta"), lty=c(1,1,1), col=c("blue","red","green"))
title(main="Tucson - 6 month SPI/SPEI/30cm zTheta")
# NOTE add sdii, rain days
par(new = T)
plot(ts(moSoil$sumPrecip,freq=12,start=c(1950,1)), xlim=c(yr1,yr2),ylim=c(0,250), axes=F, xlab=NA, ylab=NA, type='s',yaxs="i")
axis(side = 4)
mtext(side = 4, line = 12, 'SDII')
# better control over date axes
# add in RMSE, bias...



plot(spiPlot$fitted-ztheta1$fitted, type="l", col="blue",xlim=c(2000,2015),ylim=c(-3,3))
lines(speiPlot$fitted-ztheta1$fitted, type="l", col="red", xlim=c(2000,2015),ylim=c(-3,3))
legend("topleft", c("SPI-zTheta","SPEI-zTheta"), lty=c(1,1), col=c("blue","red"))
title(main="Tucson - 2 month SPI/SPEI - 10cm Theta")
abline(h=0)

par(mfrow=c(2,1))
hist(spiPlot$fitted-ztheta1$fitted)
hist(speiPlot$fitted-ztheta1$fitted)

plot(spiPlot$fitted-ztheta1$fitted, moSoil$sdii)
# lag precip metric
plot(spiPlot$fitted-ztheta1$fitted, c(tail(moSoil$sdii,-1),NA))


# NOTE scatter plot of spi/spei-zTheta vs PET, sdii...
par(mfrow=c(3,1))
plot(moSoil$avgTheta,moSoil$sdii)
abline(lsfit(moSoil$avgTheta,moSoil$sdii))
title(main="Albuquerque - monthly precip stats vs 10cm Theta")
plot(moSoil$avgTheta,moSoil$sumET)
abline(lsfit(moSoil$avgTheta,moSoil$sumET))
plot(moSoil$avgTheta,moSoil$sumPrecip)
abline(lsfit(moSoil$avgTheta,moSoil$sumPrecip))

# regress variables
fit <- lm(avgTheta ~ sumET + sumPrecip, data=moSoil)
# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
resids<-residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# All Subsets Regression
library(leaps)
leaps<-regsubsets(avgTheta ~ sumPrecip + rainDays + sdii + sumET, data=moSoil, nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# yearly analysis
pairs(yrSoil, panel = panel.smooth)
library(mgcv)
par(mfrow=c(2,2))
model<-gam(sumTheta~s(sumPrecip)+s(sumET)+s(rainDays), data=yrSoil)
plot(model)
par(mfrow=c(1,1))

library(tree)
model<-tree(avgTheta~(sumPrecip)+(sumET)+(rainDays), data=yrSoil)
plot(model)
text(model)

model1<-lm(sumTheta~sumPrecip+sumET,data=yrSoil)
summary(model1)

# regress SPI on zTheta...can SPI predict theta? Look at errors?        

# zTheta drought occurrence analysis
# id drought months        
drgtMo<-which(as.numeric(ztheta3$fitted)<=-1)
# find longest run of consecutive months
temp <- cumsum(c(1, diff(drgtMo) - 1))
temp2 <- rle(temp)
drgtRun<-drgtMo[which(temp == with(temp2, values[which.max(lengths)]))]
longDrgt<-moSoil3[drgtRun,]

# 
