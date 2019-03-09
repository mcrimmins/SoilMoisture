# write out ATMOSPH.IN file for HYDRUS-1D
# 10/23/15 MAC

#library(xlsx)
library(gdata)

# set working dir
setwd("C:/Users/Crimmins/Google Drive/MAC/r-stats/hydrus")

# get station metadata
data<-read.csv("winslow.csv")

# delete existing ATMOSPH file
unlink("ATMOSPH.IN") # CAUTION!! DELETES EXISTING FILE TO REFRESH FILE

# write out text blocks
h1<-'*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************\n'
cat(h1, file="ATMOSPH.IN",append=FALSE)
h2<-'   MaxAL                    (MaxAL = number of atmospheric data-records)\n'
cat(h2, file="ATMOSPH.IN",append=TRUE)
cat('  40419\n', file="ATMOSPH.IN",append=TRUE) # EDIT MAX NUMBER
h3<-' hCritS                 (max. allowed pressure head at the soil surface)\n'
cat(h3, file="ATMOSPH.IN",append=TRUE)
cat(' 1e+030\n', file="ATMOSPH.IN",append=TRUE) # EDIT PRESSURE HEAD
h4<-'       tAtm        Prec       rSoil       rRoot      hCritA          rB          hB          ht        tTop        tBot        Ampl        cTop        cBot\n'
cat(h4, file="ATMOSPH.IN",append=TRUE)

# write out data
write.fwf(data,file="ATMOSPH.IN",append = TRUE,colnames=FALSE,width = 11,scientific = TRUE)

footer<-"end*** END OF INPUT FILE 'ATMOSPH.IN' **********************************"
cat(footer, file="ATMOSPH.IN",append=TRUE)