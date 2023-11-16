#machine learning streamflow analysis- b2p: Denis Muthike

#B2P hy# install.packages("ncdf4")
library(ncdf4)
# install.packages("C:/Users/demu4180/Downloads/ncdf4.helpers_0.3-6.zip", repos = NULL, type = "win.binary") #needed to read nc file dates
# install.packages("C:/Users/demu4180/Downloads/PCICt_0.5-4.1.zip", repos = NULL, type = "win.binary") #needed to read nc file dates
library(ncdf4.helpers)
library(PCICt)
# install.packages(c("raster", "ggplot2"))
library(raster)
library(ggplot2)
# install.packages("lubridate")
library(lubridate)
# install.packages("rgdal")
library(rgdal)
# install.packages("vroom")
library(vroom)
# install.packages("fs")
library(fs)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("stats")
library(stats)
library(stats4)
# install.packages("randomForest")
library(randomForest)
# install.packages("Metrics")
library(Metrics)
# install.packages("hydroGOF")
library(hydroGOF)
library(hydroTSM)
library(zoo)
library(raster)
library(rasterVis)
library(ggmap)
#install.packages("moments")
library(moments)
library(sp)
# library(ie2misc)
library(reshape2)
# library(data.table)
library(rworldmap)
library(dplyr)
library(tseries)
library(timeSeries)
library(raster)
library(ncdf4) #hydrometeorological analysis- October 2022, Denis Muthike; denis.muthike@colorado.edu
library(mlbench)
library(caret)
library(caret)
library(e1071)
library(randomForest)
library(rsample)
# install.packages("h2o")
library(h2o)



#rainfall and streamflow analysis- machine learning
# setwd("C:/Users/demu4180/Documents/VIC_B2P")
dir1=setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia")
dir2=setwd("C:/Users/demu4180/Documents/VIC_B2P")



#Rainfall analysis
dates.imergfinal<-seq(as.Date("2000-06-01"), as.Date("2021-09-30"), by="days")
dates.imerglate<-seq(as.Date("2001/01/01"), as.Date("2022/10/04"), by="days")
dates.imerglatefill<-seq(as.Date("2000/10/01"), as.Date("2000/12/31"), by="days")
dates.gsmap<-seq(as.Date("2014/03/01"), as.Date("2022/10/31"), by="days")

imerglate<-paste0(dir2,"./IMERGL/imergl-20010101-20221020.nc") #dates on file end on 20221004
imerglate_stack<-raster::stack(imerglate)

# imerglate_fill<-paste0(dir1,"./B2P/ML/imerg/late/imergl-20001001-20001231.nc")
imerglate_fill<-"C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/imerg/late/imergl-20001001-20001231.nc"
imerglatefill_stack<-raster::stack(imerglate_fill)
# 
# imergfinal<-paste0(dir2,"./IMERGF/IMERG_final_corrected3.nc")
# imergfinal_stack<-stack(imergfinal)


imerglate.nc<-nc_open(paste0(dir2,"./IMERGL/imergl-20010101-20221020.nc"))
time.imergl<-nc.get.time.series(f = imerglate.nc, time.dim.name = "time") #reading dates using ncdf4 package (requires ncdf4.helpers and PCICt packages)

crs(imerglate_stack)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(imergfinal_stack)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

plot(imerglate_stack[[152]])


gsmap_mvk<-"C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/gsmap/v6/gsmap_daily_wogauge.nc"
gsmap_mvk_stk<-raster::stack(gsmap_mvk)
plot(gsmap_mvk_stk[[35]])

gsmap_gauge<-"C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/gsmap/v6/gsmap_daily_gauge.nc"
gsmap_gauge_stk<-raster::stack(gsmap_gauge)
plot(gsmap_gauge_stk[[35]])

TA.metadata<-read.csv(paste0("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/SERVIR/TAHMO_data/TA_metadata.csv"))
TA.stn.meta<-TA.metadata[!duplicated(TA.metadata$stationID),]
all.daily.refresh<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/SERVIR/TAHMO_data/Refresh_Feb2021/tahmo_satellite_daily_refresh.csv")
all.daily<-merge(all.daily.refresh,TA.stn.meta, by=c("stationID"))
all.daily$date<-as.Date(all.daily$date, origin="1970-01-01")
all.daily<-all.daily[all.daily$date>"2015-03-31" & all.daily$date<="2020-12-31",]
TA_rwanda<-all.daily[all.daily$country=="RW",]
TA_Rw_stn<-TA.stn.meta[TA.stn.meta$country=="RW",]
coordinates(TA_Rw_stn)=~longitude+latitude
proj4string(TA_Rw_stn)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(TA_Rw_stn)

#imerglate
imerg_Rw<-raster::extract(imerglate_stack, TA_Rw_stn, fun=mean, na.rm=T)
imerg_Rw_t<-t(imerg_Rw)
imerg_Rw_t<-as.data.frame(imerg_Rw_t)
rownames(imerg_Rw_t)<-seq(as.Date("2001/01/01"), as.Date("2022/10/04"), by="days")
imerg_Rw_t<-rownames_to_column(imerg_Rw_t, var="date")
colnames(imerg_Rw_t)[2:17]<-TA_Rw_stn@data$stationID

# #imergfinal
# imergfinal_Rw<-raster::extract(imergfinal_stack, TA_Rw_stn, fun=mean, na.rm=T)
# imergfinal_Rw_t<-t(imergfinal_Rw)
# imergfinal_Rw_t<-as.data.frame(imergfinal_Rw_t)
# rownames(imergfinal_Rw_t)<-dates.imergfinal
# imergfinal_Rw_t<-rownames_to_column(imergfinal_Rw_t, var="date")
# colnames(imergfinal_Rw_t)[2:17]<-TA_Rw_stn@data$stationID

# write.csv(imerg_Rw_t, paste0(dir2, "./imerglate_20010101-20221004.csv"), row.names = F)

rw_imergl_melt<-reshape2::melt(imerg_Rw_t, id.vars="date", variable.name="stationID",value.name="rf_imergl_mm")
rw_imergfinal_melt<-reshape2::melt(imergfinal_Rw_t, id.vars="date", variable.name="stationID",value.name="rf_imergf_mm")

rw_imergl_melt2<-na.omit(rw_imergl_melt)
rw_imergl_melt2$date<-as.Date(rw_imergl_melt2$date, origin="1970-01-01")
rw_imergl_melt2<-rw_imergl_melt2[rw_imergl_melt2$date>"2015-03-31" & rw_imergl_melt2$date<="2020-12-31",]
imergl_tahmo<-merge(rw_imergl_melt2, TA_rwanda, by=c("stationID","date"))
imergl_tahmo<-na.omit(imergl_tahmo)
cor(imergl_tahmo$rf_tahmo_mm,imergl_tahmo$rf_imergl_mm)
cor(imergl_tahmo$rf_tahmo_mm,imergl_tahmo$rf_chirps_mm)
cor(imergl_tahmo$rf_tahmo_mm,imergl_tahmo$rf_tamsat_mm)
cor(imergl_tahmo$rf_tahmo_mm,imergl_tahmo$rf_gsmap_mm)
cor(imergl_tahmo$rf_tahmo_mm,imergl_tahmo$rf_gsmap_mm_wgauge)


rw_imergfinal_melt2<-na.omit(rw_imergfinal_melt)
rw_imergfinal_melt2$date<-as.Date(rw_imergfinal_melt2$date, origin="1970-01-01")
rw_imergfinal_melt2<-rw_imergfinal_melt2[rw_imergfinal_melt2$date>"2015-03-31" & rw_imergfinal_melt2$date<="2020-12-31",]
imergfinal_tahmo<-merge(rw_imergfinal_melt2, TA_rwanda, by=c("stationID","date"))
imergfinal_tahmo<-na.omit(imergfinal_tahmo)
cor(imergfinal_tahmo$rf_tahmo_mm,imergfinal_tahmo$rf_imergf_mm)
cor(imergfinal_tahmo$rf_tahmo_mm,imergfinal_tahmo$rf_chirps_mm)
cor(imergfinal_tahmo$rf_tahmo_mm,imergfinal_tahmo$rf_tamsat_mm)
cor(imergfinal_tahmo$rf_tahmo_mm,imergfinal_tahmo$rf_gsmap_mm)
cor(imergfinal_tahmo$rf_tahmo_mm,imergfinal_tahmo$rf_gsmap_mm_wgauge)

all.merge.imerg<-merge(imergl_tahmo, rw_imergfinal_melt2, by=c("stationID","date"))
cor(all.merge.imerg$rf_imergl_mm,all.merge.imerg$rf_imergf_mm)

#gsmap
gsmap<-"C:\\Users\\demu4180\\MCGE Dropbox\\Denis Macharia\\gsmap\\v6\\gsmap_daily_gauge.nc"
gsmap.stk<-stack(gsmap)
gsmap_g<-raster::extract(gsmap.stk,TA_Rw_stn,fun=mean,na.rm=T)
gsmap.dates<-seq(as.Date("2000/04/01"), as.Date("2021/10/29"), by="days")
gsmap_g_t<-t(gsmap_g)
gsmap_g_t<-as.data.frame(gsmap_g_t)
rownames(gsmap_g_t)<-gsmap.dates
# library(tidyverse)

gsmap_g_t<-rownames_to_column(gsmap_g_t,var="date")
colnames(gsmap_g_t)[2:17]<-TA_Rw_stn@data$stationID
gsmap_g_tmelt<-reshape2::melt(gsmap_g_t,id.vars="date",variable.name="stationID",value.name = "rf_gsmapG_rnl_mm")
gsmap_g_tmelt<-na.omit(gsmap_g_tmelt)
gsmap_g_tmelt$date<-as.Date(gsmap_g_tmelt$date,origin="1970-01-01")
gsmap_g_tmelt<-gsmap_g_tmelt[gsmap_g_tmelt$date>"2015-03-31" & gsmap_g_tmelt$date<="2020-12-31",]
all.srp.merge<-merge(imergl_tahmo,gsmap_g_tmelt, by=c("stationID","date"))
all.srp.merge<-na.omit(all.srp.merge)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmapG_rnl_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm_wgauge)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm)

#gsmap2
#imerglate
gsmap_mvk_Rw<-raster::extract(gsmap_mvk_stk, TA_Rw_stn, fun=mean, na.rm=T)
gsmap_mvk_Rw_t<-t(gsmap_mvk_Rw)
gsmap_mvk_Rw_t<-as.data.frame(gsmap_mvk_Rw_t)
gsmap_mvk_Rw_t<-rownames_to_column(gsmap_mvk_Rw_t, var="date")
rownames(gsmap_mvk_Rw_t)<-dates.gsmap
gsmap_mvk_Rw_t<-rownames_to_column(gsmap_mvk_Rw_t, var="date")
colnames(gsmap_mvk_Rw_t)[2:17]<-TA_Rw_stn@data$stationID

##enacts
files.enacts<-list.files("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/climate/rainfall/srps/ENACTS/raw/forval",pattern = ".nc", all.files=FALSE, full.names = TRUE)
enacts.stk1<-stack(files.enacts[1:5])
enacts.stk2<-stack(files.enacts[6:7])
dates.stk1<-seq(as.Date("2014-01-01"),as.Date("2018-12-31"),by="days")
dates.stk2<-seq(as.Date("2019-01-01"),as.Date("2020-12-31"),by="days")

# enacts.stk<-stack(enacts)
enacts1<-raster::extract(enacts.stk1,TA_Rw_stn,fun=mean,na.rm=T)
enacts2<-raster::extract(enacts.stk2,TA_Rw_stn,fun=mean,na.rm=T)
# enacts1.dates<-seq(as.Date("2000/04/01"), as.Date("2021/10/29"), by="days")
enacts1_t<-t(enacts1)
enacts1_t<-as.data.frame(enacts1_t)
rownames(enacts1_t)<-dates.stk1

enacts2_t<-t(enacts2)
enacts2_t<-as.data.frame(enacts2_t)
rownames(enacts2_t)<-dates.stk2

#merge

# library(tidyverse)

enacts1_t<-rownames_to_column(enacts1_t,var="date")
enacts2_t<-rownames_to_column(enacts2_t,var="date")

colnames(enacts1_t)[2:17]<-TA_Rw_stn@data$stationID
colnames(enacts2_t)[2:17]<-TA_Rw_stn@data$stationID
enacts_t<-rbind(enacts1_t,enacts2_t)

enacts_tmelt<-reshape2::melt(enacts_t,id.vars="date",variable.name="stationID",value.name = "rf_enacts_mm")
enacts_tmelt<-na.omit(enacts_tmelt)
enacts_tmelt$date<-as.Date(enacts_tmelt$date,origin="1970-01-01")
enacts_tmelt<-enacts_tmelt[enacts_tmelt$date>"2015-03-31" & enacts_tmelt$date<="2020-12-31",]
all.srp.merge<-merge(imergl_tahmo,enacts_tmelt, by=c("stationID","date"))
all.srp.merge<-na.omit(all.srp.merge)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_enacts_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_imergl_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_chirps_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_tamsat_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm)
cor(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm_wgauge)

## CC
# CHIRPS-0.218216
# ENACTS-0.2247035
# TAMSAT-0.1644398
# GSMAP-0.2904541
# GSMAP_G-0.2648444 
# IMERGL-0.2330339

#mae
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_enacts_mm)
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_imergl_mm)
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_chirps_mm)
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_tamsat_mm)
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm)
mae(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm_wgauge)

rmse(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm_wgauge)
rmse(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_imergl_mm)
rmse(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_chirps_mm)
rmse(all.srp.merge$rf_tahmo_mm,all.srp.merge$rf_gsmap_mm)

#KGE
KGE(all.srp.merge$rf_enacts_mm,all.srp.merge$rf_tahmo_mm)
KGE(all.srp.merge$rf_imergl_mm,all.srp.merge$rf_tahmo_mm)
KGE(all.srp.merge$rf_chirps_mm,all.srp.merge$rf_tahmo_mm)
KGE(all.srp.merge$rf_tamsat_mm,all.srp.merge$rf_tahmo_mm)
KGE(all.srp.merge$rf_gsmap_mm,all.srp.merge$rf_tahmo_mm)
KGE(all.srp.merge$rf_gsmap_mm_wgauge,all.srp.merge$rf_tahmo_mm)

#ENACTS-0.143958
#IMERGL-0.1105254
#CHIRPS-0.1274522
#TAMSAT-0.1321451
#GSMAP-0.2067838
#GSMAP_Gauge-0.1389132

contable.all<-matrix(1:16, nrow = 8, ncol = 2, dimnames = list(c("CHIRPS_Rain", "CHIRPS_NoRain","IMERGL_Rain", "IMERGL_NoRain", "GSMaP_Rain", "GSMaP_NoRain","GSMaP_wGauge_Rain", "GSMaP_wGauge_NoRain"), c("TAHMO_Rain", "TAHMO_NoRain")))


contable.all[1,1]<-round((((length(which(all.srp.merge$rf_chirps_mm !=0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100)#when both have rain
contable.all[1,2]<-round((((length(which(all.srp.merge$rf_chirps_mm !=0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when chirps has rain but TAHMO doesn't
contable.all[2,1]<-round((((length(which(all.srp.merge$rf_chirps_mm ==0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when TAHMO has rain but chirps doesn't
contable.all[2,2]<-round((((length(which(all.srp.merge$rf_chirps_mm ==0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when both have no rain

contable.all[3,1]<-round((((length(which(all.srp.merge$rf_imergl_mm !=0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100)#when both have rain
contable.all[3,2]<-round((((length(which(all.srp.merge$rf_imergl_mm !=0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when imergl has rain but TAHMO doesn't
contable.all[4,1]<-round((((length(which(all.srp.merge$rf_imergl_mm ==0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when TAHMO has rain but imergl doesn't
contable.all[4,2]<-round((((length(which(all.srp.merge$rf_imergl_mm ==0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when both have no rain

contable.all[5,1]<-round((((length(which(all.srp.merge$rf_gsmap_mm !=0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100)#when both have rain
contable.all[5,2]<-round((((length(which(all.srp.merge$rf_gsmap_mm !=0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when GSMaP has rain but TAHMO doesn't
contable.all[6,1]<-round((((length(which(all.srp.merge$rf_gsmap_mm ==0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when TAHMO has rain but GSMaP doesn't
contable.all[6,2]<-round((((length(which(all.srp.merge$rf_gsmap_mm ==0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when both have no rain

contable.all[7,1]<-round((((length(which(all.srp.merge$rf_gsmap_mm_wgauge !=0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100)#when both have rain
contable.all[7,2]<-round((((length(which(all.srp.merge$rf_gsmap_mm_wgauge !=0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when GSMaP_wGauge has rain but TAHMO doesn't
contable.all[8,1]<-round((((length(which(all.srp.merge$rf_gsmap_mm_wgauge ==0 & all.srp.merge$rf_tahmo_mm !=0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when TAHMO has rain but GSMaP_wGauge doesn't
contable.all[8,2]<-round((((length(which(all.srp.merge$rf_gsmap_mm_wgauge ==0 & all.srp.merge$rf_tahmo_mm ==0))))/length(all.srp.merge$rf_tahmo_mm))*100) #when both have no rain


# as.table(contable.all)
POD.CHIRPS<-contable.all[1,1]/((contable.all[1,1])+(contable.all[2,1]))
POD.IMERGL<-contable.all[3,1]/((contable.all[3,1])+(contable.all[4,1]))
POD.GSMaP<-contable.all[5,1]/((contable.all[5,1])+(contable.all[6,1]))
POD.GSMaP_wGauge<-contable.all[7,1]/((contable.all[7,1])+(contable.all[8,1]))
# POD.ENACTS<-contable.all[1,1]/((contable.all[1,1])+(contable.all[2,1]))

POD.CHIRPS
POD.IMERGL
POD.GSMaP
POD.GSMaP_wGauge

## POD
# CHIRPS-0.421875
# ENACTS-0.3333333
# TAMSAT-
# GSMAP-0.78125
# GSMAP_G-0.84375
# IMERGL-0.7936508 



#Streamflow analysis- machine learning

#the following code uses these variables: rainfall (imergf-imerg and imerglate-imergl, and IRI/Rwanda Meteo- enacts), temperature data from GTS (tmin and tmax), soil moisture from EU CCI (sm) and Corpernicus (cds), and discharge data from Rwanda Water Borad (dicharge)


#1. Read rainfall data dates: 2000-06-01:2021-09-30 (imergf); 20100101:20201231 (imergl); 20000101:20211231 (enacts)
dates.imergf<-seq(as.Date("2000-06-01"), as.Date("2021-09-30"), by="days")
dates.imergl<-seq(as.Date("2001-01-01"), as.Date("2022-10-04"), by="days")
dates.enacts<-seq(as.Date("2010-01-01"), as.Date("2014-12-31"), by="days")
dates.imerglatefill<-seq(as.Date("2000/10/01"), as.Date("2000/12/31"), by="days")


imergf<-paste0(dir2,"./IMERGF/IMERG_final_corrected3.nc")
imergf.stk<-stack(imergf)

imergl<-paste0(dir2,"./IMERGL/imergl-20010101-20221020.nc")
imergl.stk<-stack(imergl)

imerglate_fill<-paste0(dir1,"./B2P/ML/imerg/late/imergl-20001001-20001231.nc")
imerglate_fill<-"C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/imerg/late/imergl-20001001-20001231.nc"

imerglatefill_stack<-stack(imerglate_fill)

# files.enacts<-list.files(path = "./ML/enacts",pattern = ".nc", all.files=FALSE, full.names = TRUE)
# enacts.stk<-stack(files.enacts)

#2. read temperature data dates:2000-01-01:2022-07-31
dates.temp<-seq(as.Date("2000-01-01"), as.Date("2022-10-31"), by="days")
tmin<-paste0(dir1,"./B2P/ML/temperature/gts/gts_tmin_20000101-20221031.nc")
tmax<-paste0(dir1,"./B2P/ML/temperature/gts/gts_tmax_20000101-20221031.nc")
tmin.stk<-stack(tmin)
tmax.stk<-stack(tmax)

# #3. read lai data dates: 2011-01-01:2013-12-31
# dates.lai<-seq(as.Date("2011-01-01"), as.Date("2013-12-31"), by="days")
# avhrr.lai<-list.files(path = "./ML/lai", pattern = ".nc", all.files=FALSE, full.names = TRUE)
# lai.stk<-stack(avhrr.lai)

#4. read soil moisture data dates:2000-01-01:2021-12-31
dates.cci.sm<-seq(as.Date("2000-01-01"), as.Date("2021-12-31"), by="days")
dates.cds.sm<-seq(as.Date("2022-01-01"), as.Date("2022-10-20"), by="days")

# files.nc<- fs::dir_ls(path = "./ML/soilmoisture/images/", recurse = TRUE, type = "file", glob = "*.nc") #copy files from different folders into a single folder
# target.dir<-"./ML/soilmoisture/data"
# sapply(files.nc, function(x) file.copy(from=x, to=target.dir, copy.mode = TRUE))

cci.sm<-list.files(path= "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/soilmoisture-ESACCI/data", pattern=".nc", all.files=FALSE,
                   full.names=TRUE)

soil.moisture<-stack(cci.sm)

#cds soil moisture
files.cds.sm<-list.files(path="C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/soilmoisture_EUCDR/2022", pattern=".nc", all.files=FALSE,
                           full.names=TRUE)
cds.soilmoisture<-stack(files.cds.sm)
africa<-subset(getMap(), continent=="Africa")
rwanda<-africa[africa@data$NAME=="Rwanda",]

cds.crop<-crop(cds.soilmoisture,rwanda)

for(i in 1:nlayers(cds.crop)){
  cds.crop[cds.crop<0] <-NA
} # apply NA mask to all the stacked layers


#5. read streamflow observations
ruliba_discharge<-read.csv(paste0(dir1,"./B2P/ML/discharge/observed/ruliba_new27Oct2022/ruliba_calibration_telemtry_MLflowcorrected.csv"))
# ruliba_discharge<-read.csv(paste0(dir1,"./B2P/ML/discharge/observed/ruliba_new27Oct2022/ruliba_discharge_combined_new.csv"))
# mwaka<-read.csv2("./ML/discharge/observed/mwaka_obs.csv")

#6. read catchment shapefiles
ruliba_catchment<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/catchments/Ruliba_catchment_prj.shp", layer="Ruliba_catchment_prj")
mwaka_catchment<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/catchments/Mwaka_catchment_prj.shp", layer="Mwaka_catchment_prj")
ngaru_catchment<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/catchments/Ngaru_catchment_prj.shp", layer="Ngaru_catchment_prj")


#Extract data
imerg_ruliba<-raster::extract(imergf.stk, ruliba_catchment, fun=mean,na.rm=TRUE)
imerg_mwaka<-raster::extract(imergf.stk, mwaka_catchment, fun=mean,na.rm=TRUE)
imerg_ngaru<-raster::extract(imergf.stk, ngaru_catchment, fun=mean,na.rm=TRUE)

imergl_ruliba<-raster::extract(imergl.stk, ruliba_catchment, fun=mean, na.rm=TRUE)
imergl_mwaka<-raster::extract(imergl.stk, mwaka_catchment, fun=mean, na.rm=TRUE)
imergl_ngaru<-raster::extract(imergl.stk, ngaru_catchment, fun=mean, na.rm=TRUE)

imerglfill_ruliba<-raster::extract(imerglatefill_stack, ruliba_catchment, fun=mean, na.rm=TRUE)
imerglfill_mwaka<-raster::extract(imerglatefill_stack, mwaka_catchment, fun=mean, na.rm=TRUE)
imerglfill_ngaru<-raster::extract(imerglatefill_stack, ngaru_catchment, fun=mean, na.rm=TRUE)

# enacts_ruliba<-raster::extract(enacts.stk, ruliba_catchment, fun=mean, na.rm=TRUE)
# enacts_mwaka<-raster::extract(enacts.stk, mwaka_catchment, fun=mean, na.rm=TRUE)
# enacts_ngaru<-raster::extract(enacts.stk, ngaru_catchment, fun=mean, na.rm=TRUE)


tmin_ruliba<-raster::extract(tmin.stk, ruliba_catchment, fun=mean,na.rm=TRUE)
tmin_mwaka<-raster::extract(tmin.stk, mwaka_catchment, fun=mean,na.rm=TRUE)
tmin_ngaru<-raster::extract(tmin.stk, ngaru_catchment, fun=mean,na.rm=TRUE)

tmax_ruliba<-raster::extract(tmax.stk, ruliba_catchment, fun=mean,na.rm=TRUE)
tmax_mwaka<-raster::extract(tmax.stk, mwaka_catchment, fun=mean,na.rm=TRUE)
tmax_ngaru<-raster::extract(tmax.stk, ngaru_catchment, fun=mean,na.rm=TRUE)

# lai_ruliba<-raster::extract(lai.stk, ruliba_catchment, fun=mean,na.rm=TRUE)
# lai_mwaka<-raster::extract(lai.stk, mwaka_catchment, fun=mean,na.rm=TRUE)
# lai_ngaru<-raster::extract(lai.stk, ngaru_catchment, fun=mean,na.rm=TRUE)

sm_ruliba<-raster::extract(soil.moisture, ruliba_catchment, fun=mean,na.rm=TRUE)
sm_mwaka<-raster::extract(soil.moisture, mwaka_catchment, fun=mean,na.rm=TRUE)
sm_ngaru<-raster::extract(soil.moisture, ngaru_catchment, fun=mean, na.rm=TRUE)

cdssm_ruliba<-raster::extract(cds.crop,ruliba_catchment,fun=mean,na.rm=T)
cdssm_mwaka<-raster::extract(cds.crop,mwaka_catchment,fun=mean,na.rm=T)
cdssm_ngaru<-raster::extract(cds.crop,ngaru_catchment,fun=mean,na.rm=T)


imerg_ruliba<-t(imerg_ruliba)
imerg_ruliba<-as.data.frame(imerg_ruliba)
rownames(imerg_ruliba)<-dates.imergf
imerg_ruliba<-rownames_to_column(imerg_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(imerg_ruliba)[2]<-"imergf_mm"
write.csv(imerg_ruliba, paste0(dir1,"./B2P/ML/csv/imerg_ruliba.csv", row.names = F))

imerg_mwaka<-t(imerg_mwaka)
imerg_mwaka<-as.data.frame(imerg_mwaka)
rownames(imerg_mwaka)<-dates.imergf
imerg_mwaka<-rownames_to_column(imerg_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(imerg_mwaka)[2]<-"imergf_mm"
write.csv(imerg_mwaka, paste0(dir1,"./B2P/ML/csv/imerg_mwaka.csv", row.names = F))

imerg_ngaru<-t(imerg_ngaru)
imerg_ngaru<-as.data.frame(imerg_ngaru)
rownames(imerg_ngaru)<-dates.imergf
imerg_ngaru<-rownames_to_column(imerg_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(imerg_ngaru)[2]<-"imergf_mm"
write.csv(imerg_ngaru, paste0(dir1,"./B2P/ML/csv/imerg_ngaru.csv", row.names = F))

imergl_ruliba<-t(imergl_ruliba)
imergl_ruliba<-as.data.frame(imergl_ruliba)
rownames(imergl_ruliba)<-dates.imergl
imergl_ruliba<-rownames_to_column(imergl_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(imergl_ruliba)[2]<-"imergl_mm"
write.csv(imergl_ruliba, paste0(dir1,"./B2P/ML/csv/imergl_ruliba.csv", row.names = F))

imergl_mwaka<-t(imergl_mwaka)
imergl_mwaka<-as.data.frame(imergl_mwaka)
rownames(imergl_mwaka)<-dates.imergl
imergl_mwaka<-rownames_to_column(imergl_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(imergl_mwaka)[2]<-"imergl_mm"
write.csv(imergl_mwaka, paste0(dir1,"./B2P/ML/csv/imergl_mwaka.csv", row.names = F))

imergl_ngaru<-t(imergl_ngaru)
imergl_ngaru<-as.data.frame(imergl_ngaru)
rownames(imergl_ngaru)<-dates.imergl
imergl_ngaru<-rownames_to_column(imergl_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(imergl_ngaru)[2]<-"imergl_mm"
write.csv(imergl_ngaru, paste0(dir1,"./B2P/ML/csv/imergl_ngaru.csv", row.names = F))

imerglfill_ruliba<-t(imerglfill_ruliba)
imerglfill_ruliba<-as.data.frame(imerglfill_ruliba)
rownames(imerglfill_ruliba)<-dates.imerglatefill
imerglfill_ruliba<-rownames_to_column(imerglfill_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(imerglfill_ruliba)[2]<-"imerglfill_mm"
write.csv(imerglfill_ruliba, paste0(dir1,"./B2P/ML/csv/imerglfill_ruliba.csv", row.names = F))

imerglfill_mwaka<-t(imerglfill_mwaka)
imerglfill_mwaka<-as.data.frame(imerglfill_mwaka)
rownames(imerglfill_mwaka)<-dates.imerglatefill
imerglfill_mwaka<-rownames_to_column(imerglfill_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(imerglfill_mwaka)[2]<-"imerglfill_mm"
write.csv(imerglfill_mwaka, paste0(dir1,"./B2P/ML/csv/imerglfill_mwaka.csv", row.names = F))

imerglfill_ngaru<-t(imerglfill_ngaru)
imerglfill_ngaru<-as.data.frame(imerglfill_ngaru)
rownames(imerglfill_ngaru)<-dates.imerglatefill
imerglfill_ngaru<-rownames_to_column(imerglfill_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(imerglfill_ngaru)[2]<-"imerglfill_mm"
write.csv(imerglfill_ngaru, paste0(dir1,"./B2P/ML/csv/imerglfill_ngaru.csv", row.names = F))


# enacts_ruliba<-t(enacts_ruliba)
# enacts_ruliba<-as.data.frame(enacts_ruliba)
# rownames(enacts_ruliba)<-dates.enacts
# enacts_ruliba<-rownames_to_column(enacts_ruliba, var="date") #name first blank column after extracting data from raster stack
# colnames(enacts_ruliba)[2]<-"enactsf_mm"
# write.csv(enacts_ruliba, "./ML/csv/enacts_ruliba.csv", row.names = F)
# 
# enacts_mwaka<-t(enacts_mwaka)
# enacts_mwaka<-as.data.frame(enacts_mwaka)
# rownames(enacts_mwaka)<-dates.enacts
# enacts_mwaka<-rownames_to_column(enacts_mwaka, var="date") #name first blank column after extracting data from raster stack
# colnames(enacts_mwaka)[2]<-"enactsf_mm"
# write.csv(enacts_mwaka, "./ML/csv/enacts_mwaka.csv", row.names = F)
# 
# enacts_ngaru<-t(enacts_ngaru)
# enacts_ngaru<-as.data.frame(enacts_ngaru)
# rownames(enacts_ngaru)<-dates.enacts
# enacts_ngaru<-rownames_to_column(enacts_ngaru, var="date") #name first blank column after extracting data from raster stack
# colnames(enacts_ngaru)[2]<-"enactsf_mm"
# write.csv(enacts_ngaru, "./ML/csv/enacts_ngaru.csv", row.names = F)
# 
# #temperature
tmin_ruliba<-t(tmin_ruliba)
tmin_ruliba<-as.data.frame(tmin_ruliba)
rownames(tmin_ruliba)<-dates.temp
tmin_ruliba<-rownames_to_column(tmin_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(tmin_ruliba)[2]<-"tmin"
write.csv(tmin_ruliba, paste0(dir1,"./B2P/ML/csv/tmin_ruliba.csv", row.names = F))

tmin_mwaka<-t(tmin_mwaka)
tmin_mwaka<-as.data.frame(tmin_mwaka)
rownames(tmin_mwaka)<-dates.temp
tmin_mwaka<-rownames_to_column(tmin_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(tmin_mwaka)[2]<-"tmin"
write.csv(tmin_mwaka, paste0(dir1,"./B2P/ML/csv/tmin_mwaka.csv", row.names = F))

tmin_ngaru<-t(tmin_ngaru)
tmin_ngaru<-as.data.frame(tmin_ngaru)
rownames(tmin_ngaru)<-dates.temp
tmin_ngaru<-rownames_to_column(tmin_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(tmin_ngaru)[2]<-"tmin"
write.csv(tmin_ngaru, paste0(dir1,"./B2P/ML/csv/tmin_ngaru.csv", row.names = F))

tmax_ruliba<-t(tmax_ruliba)
tmax_ruliba<-as.data.frame(tmax_ruliba)
rownames(tmax_ruliba)<-dates.temp
tmax_ruliba<-rownames_to_column(tmax_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(tmax_ruliba)[2]<-"tmax"
write.csv(tmax_ruliba, paste0(dir1,"./B2P/ML/csv/tmax_ruliba.csv", row.names = F))

tmax_mwaka<-t(tmax_mwaka)
tmax_mwaka<-as.data.frame(tmax_mwaka)
rownames(tmax_mwaka)<-dates.temp
tmax_mwaka<-rownames_to_column(tmax_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(tmax_mwaka)[2]<-"tmax"
write.csv(tmax_mwaka, paste0(dir1,"./B2P/ML/csv/tmax_mwaka.csv", row.names = F))

tmax_ngaru<-t(tmax_ngaru)
tmax_ngaru<-as.data.frame(tmax_ngaru)
rownames(tmax_ngaru)<-dates.temp
tmax_ngaru<-rownames_to_column(tmax_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(tmax_ngaru)[2]<-"tmax"
write.csv(tmax_ngaru, paste0(dir1,"./B2P/ML/csv/tmax_ngaru.csv", row.names = F))

# lai_ruliba<-t(lai_ruliba)
# lai_ruliba<-as.data.frame(lai_ruliba)
# lai_ruliba<-rownames_to_column(lai_ruliba, var="date") #name first blank column after extracting data from raster stack
# colnames(lai_ruliba)[2]<-"lai"
# write.csv(lai_ruliba, "./ML/csv/lai_ruliba.csv", row.names = F)
# 
# lai_mwaka<-t(lai_mwaka)
# lai_mwaka<-as.data.frame(lai_mwaka)
# lai_mwaka<-rownames_to_column(lai_mwaka, var="date") #name first blank column after extracting data from raster stack
# colnames(lai_mwaka)[2]<-"lai"
# write.csv(lai_mwaka, "./ML/csv/lai_mwaka.csv", row.names = F)
# 
# lai_ngaru<-t(lai_ngaru)
# lai_ngaru<-as.data.frame(lai_ngaru)
# lai_ngaru<-rownames_to_column(lai_ngaru, var="date") #name first blank column after extracting data from raster stack
# colnames(lai_ngaru)[2]<-"lai"
# write.csv(lai_ngaru, "./ML/csv/lai_ngaru.csv", row.names = F)

sm_ruliba<-t(sm_ruliba)
sm_ruliba<-as.data.frame(sm_ruliba)
rownames(sm_ruliba)<-dates.cci.sm
sm_ruliba<-rownames_to_column(sm_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(sm_ruliba)[2]<-"sm"
write.csv(sm_ruliba, paste0(dir1,"./B2P/ML/csv/sm_ruliba.csv", row.names = F))

sm_mwaka<-t(sm_mwaka)
sm_mwaka<-as.data.frame(sm_mwaka)
rownames(sm_mwaka)<-dates.cci.sm
sm_mwaka<-rownames_to_column(sm_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(sm_mwaka)[2]<-"sm"
write.csv(sm_mwaka, paste0(dir1,"./B2P/ML/csv/sm_mwaka.csv", row.names = F))

sm_ngaru<-t(sm_ngaru)
sm_ngaru<-as.data.frame(sm_ngaru)
rownames(sm_ngaru)<-dates.cci.sm
sm_ngaru<-rownames_to_column(sm_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(sm_ngaru)[2]<-"sm"
write.csv(sm_ngaru, paste0(dir1,"./B2P/ML/csv/sm_ngaru.csv", row.names = F))

cdssm_ruliba<-t(cdssm_ruliba)
cdssm_ruliba<-as.data.frame(cdssm_ruliba)
rownames(cdssm_ruliba)<-dates.cds.sm
cdssm_ruliba<-rownames_to_column(cdssm_ruliba, var="date") #name first blank column after extracting data from raster stack
colnames(cdssm_ruliba)[2]<-"sm"
write.csv(cdssm_ruliba, paste0(dir1,"./B2P/ML/csv/cdssm_ruliba.csv", row.names = F))

cdssm_mwaka<-t(cdssm_mwaka)
cdssm_mwaka<-as.data.frame(cdssm_mwaka)
rownames(cdssm_mwaka)<-dates.cds.sm
cdssm_mwaka<-rownames_to_column(cdssm_mwaka, var="date") #name first blank column after extracting data from raster stack
colnames(cdssm_mwaka)[2]<-"sm"
write.csv(cdssm_mwaka, paste0(dir1,"./B2P/ML/csv/cdssm_mwaka.csv", row.names = F))

cdssm_ngaru<-t(cdssm_ngaru)
cdssm_ngaru<-as.data.frame(cdssm_ngaru)
rownames(cdssm_ngaru)<-dates.cds.sm
cdssm_ngaru<-rownames_to_column(cdssm_ngaru, var="date") #name first blank column after extracting data from raster stack
colnames(cdssm_ngaru)[2]<-"sm"
write.csv(cdssm_ngaru, paste0(dir1,"./B2P/ML/csv/cdssm_ngaru.csv", row.names = F))

#combine imergf and imergl (imergf ends on 30/9/2021 whereas imergl ends on 04/10/22)
# ruliba_imergf<-imerg_ruliba
# ruliba_imergl<-imergl_ruliba
# 
# ruliba_imergf$rf_mm<-ruliba_imergf$imergf_mm
# ruliba_imergl$rf_mm<-ruliba_imergl$imergl_mm
# 
# ruliba_imergf<-ruliba_imergf[,c(1,3)]
# ruliba_imergl<-ruliba_imergl[,c(1,3)]
# 
# ruliba_imergl.concat<-ruliba_imergl[ruliba_imergl$date>="2021-10-01",]
# ruliba_imerg_ml<-rbind(ruliba_imergf,ruliba_imergl.concat)
# write.csv(ruliba_imerg_ml, paste0(dir1,"./B2P/ML/csv/ruliba_imerg_ml_final.csv", row.names = F))

#combine imerglfill and imergl
ruliba_imerglfill<-imerglfill_ruliba
names(ruliba_imerglfill)[2]<-"rf_mm"
head(ruliba_imergl)
head(ruliba_imerglfill)

ruliba_imerg_ml<-rbind(ruliba_imerglfill,ruliba_imergl)
head(ruliba_imerg_ml)

#combine cci-soil moisture and cds soil moisture (cci ends on 31/12/2021 whereas cds ends on 31/07/22)
ruliba_ccism<-sm_ruliba
ruliba_cdsm<-cdssm_ruliba

ruliba_soilmoisture<-rbind(ruliba_ccism,ruliba_cdsm)

write.csv(ruliba_soilmoisture, paste0(dir1,"./B2P/ML/csv/ruliba_soilmoisture_ml.csv", row.names = F))

#ML - RULIBA Catchment
# setwd("C:/Users/DENNIS/MCGE Dropbox/Denis Macharia/B2P")
#1. build model with Ruliba data
#read input data and subset dates to 2011-2013 (match with observed streamflow)
ruliba_rf<-ruliba_imerg_ml
ruliba_tmin<-tmin_ruliba
ruliba_tmax<-tmax_ruliba
ruliba_sm<-ruliba_soilmoisture
ruliba_discharge<-ruliba_discharge
# ruliba_rfl<-read.csv("./ML/csv/imergl_ruliba.csv")
# ruliba_rfen<-read.csv("./ML/csv/enacts_ruliba.csv")




#create lagged variables (1,2,3,4,5 days--likely concentration times)
ruliba_rf$rf.lag1d<-dplyr::lag(ruliba_rf$rf_mm, 1)
ruliba_rf$rf.lag2d<-dplyr::lag(ruliba_rf$rf_mm, 2)
ruliba_rf$rf.lag3d<-dplyr::lag(ruliba_rf$rf_mm, 3)
# ruliba_rf$rf.lag4d<-dplyr::lag(ruliba_rf$rf_mm, 4)
# ruliba_rf$rf.lag5d<-dplyr::lag(ruliba_rf$rf_mm, 5)

# 
# ruliba_rfl$rfl.lag1d<-Lag(ruliba_rfl$imerglf_mm, -1)
# ruliba_rfl$rfl.lag2d<-Lag(ruliba_rfl$imerglf_mm, -2)
# ruliba_rfl$rfl.lag3d<-Lag(ruliba_rfl$imerglf_mm, -3)
# ruliba_rfl$rfl.lag4d<-Lag(ruliba_rfl$imerglf_mm, -4)
# ruliba_rfl$rfl.lag5d<-Lag(ruliba_rfl$imerglf_mm, -5)
# 
# ruliba_rfen$rfen.lag1d<-Lag(ruliba_rfen$enactsf_mm, -1)
# ruliba_rfen$rfen.lag2d<-Lag(ruliba_rfen$enactsf_mm, -2)
# ruliba_rfen$rfen.lag3d<-Lag(ruliba_rfen$enactsf_mm, -3)
# ruliba_rfen$rfen.lag4d<-Lag(ruliba_rfen$enactsf_mm, -4)
# ruliba_rfen$rfen.lag5d<-Lag(ruliba_rfen$enactsf_mm, -5)

# write.csv(ruliba_rfen, "./ML/csv/ruliba_rfen.csv", row.names = F)
# ruliba_rfen<-read.csv("./ML/csv/ruliba_rfen.csv")

ruliba_tmin$tmin.lag1d<-dplyr::lag(ruliba_tmin$tmin, 1)
ruliba_tmin$tmin.lag2d<-dplyr::lag(ruliba_tmin$tmin, 2)
ruliba_tmin$tmin.lag3d<-dplyr::lag(ruliba_tmin$tmin, 3)


ruliba_tmax$tmax.lag1d<-dplyr::lag(ruliba_tmax$tmax, 1)
ruliba_tmax$tmax.lag2d<-dplyr::lag(ruliba_tmax$tmax, 2)
ruliba_tmax$tmax.lag3d<-dplyr::lag(ruliba_tmax$tmax, 3)


ruliba_sm$sm.lag1d<-dplyr::lag(ruliba_sm$sm, 1)
ruliba_sm$sm.lag2d<-dplyr::lag(ruliba_sm$sm, 2)
ruliba_sm$sm.lag3d<-dplyr::lag(ruliba_sm$sm, 3)

ruliba_timeseries_pred<-write.csv(ruliba_timeseries_pred, paste0(dir1, "./B2P/ML/discharge/predicted/ruliba_timeseries_pred.csv"),row.names=F)

ruliba_timeseries_pred<-read.csv(paste0(dir1, "./B2P/ML/discharge/predicted/ruliba_timeseries_predRF_vRFmod.csv"))

##NDVI- MODIS (GEE)
ruliba_ndvi<-read.csv(paste0(dir1, "./B2P/ML/emodis/ruliba_ndvi_timeseries.csv"))

#merge all variables into one dataframe
merge1<-merge(ruliba_rf,ruliba_tmin,by="date")
merge2<-merge(ruliba_tmax,ruliba_sm,by="date")
merge3<-merge(merge1,merge2,by="date")
merge4<-merge(merge3,ruliba_ndvi,by="date")
ruliba_timeseries_pred<-merge4


#subset dates
# ruliba_rf_ml<-ruliba_rf[ruliba_rf$date>="2001-01-01" & ruliba_rf$date<="2022-10-04",]
# ruliba_tmin_ml<-ruliba_tmin[ruliba_tmin$date>="2001-01-01" & ruliba_tmin$date<="2022-10-04",]
# ruliba_tmax_ml<-ruliba_tmax[ruliba_tmax$date>="2001-01-01" & ruliba_tmax$date<="2022-10-04",]
# ruliba_sm_ml<-ruliba_sm[ruliba_sm$date>="2001-01-01" & ruliba_sm$date<="2022-10-04",]
# ruliba_imgl_ml<-ruliba_rfl[ruliba_rfl$date>="2008-01-03" & ruliba_rfl$date<="2022-07-31",]
# ruliba_ena_ml<-ruliba_rfen[ruliba_rfen$date>="2008-01-03" & ruliba_rfen$date<="2022-07-31",]


#combine the data to a single dataframe
# ruliba_ml_data<-cbind(ruliba_rf_ml,ruliba_tmin_ml,ruliba_tmax_ml,ruliba_sm_ml)
# ruliba_ml_data<-ruliba_ml_data[,c(1:5,7:10,12:15,17:20)]
# ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)
# ruliba_discharge$date<-as.Date(ruliba_discharge$date)
# ruliba_discharge<-ruliba_discharge[,c(-4)] #remove ndvi lag
ruliba_discharge<-ruliba_discharge[,-3]

ruliba_ml_data<-merge(ruliba_timeseries_pred,ruliba_discharge, by="date")

ruliba_ml_data<-na.omit(ruliba_ml_data)
# ruliba_ml_data<-ruliba_ml_data[,c(1:17,19,18)]

write.csv(ruliba_ml_data, paste0(dir1,"./B2P/ML/csv/ml_final_data_v6.csv"), row.names = F)
ruliba_ml_data<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/csv/ml_final_data_v6.csv")

#lai
ruliba_lai<-read.csv(paste0(dir1,"./B2P/ML/emodis/lai/ruliba_lai_final.csv"))
ruliba_ml_data<-merge(ruliba_ml_data,ruliba_lai,by="date")
ruliba_ml_data<-ruliba_ml_data[,c(1:18,20,19)]

write.csv(ruliba_ml_data, "./B2P/ML/csv/ml_final_data_v7.csv", row.names = F)

#ggplot(ruliba_ml_data, aes(x=discharge))+geom_density(alpha = .2, fill= "#FF6666")

#RandomForest prediction of discharge

#with original data (all)
#rf.mod
index = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.75,0.25))
training = ruliba_ml_data[index==1,]
testing = ruliba_ml_data[index==2,]
rf.mod<-randomForest(discharge~.,data = training[,-1],ntree=1000)
disc_pred<-predict(rf.mod,testing[,-1])
testing$disc_pred<-disc_pred
cor(testing$discharge,testing$disc_pred)
NSE(sim=testing$disc_pred,obs = testing$discharge)

p1=ggof(sim=testing$disc_pred,obs=testing$discharge,ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "RF simulation vs Observations")
#Output rf.mod(75/25 split): NSE:0.65, r:0.81

#rmod6 with lai incorporated

index6<-sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.75,0.25))
training6 = ruliba_ml_data[index6==1,]
testing6 = ruliba_ml_data[index6==2,]
rf.mod6<-randomForest(discharge~.,data = training6[,-1],ntree=1000)
disc_pred6<-predict(rf.mod6,testing6[,-1])
testing6$disc_pred<-disc_pred6
cor(testing6$discharge,testing6$disc_pred)
NSE(sim=testing6$disc_pred,obs = testing6$discharge)
ggof(sim=testing6$disc_pred,obs=testing6$discharge,ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "RF simulation vs Observations")

#rfmod6 NSE 0.73, cor 0.87



# 
# #rf.mod1x
# index1 = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.8,0.2))
# training1 = ruliba_ml_data[index1==1,]
# testing1 = ruliba_ml_data[index1==2,]
# rf.mod1x<-randomForest(discharge~.,data = training1[,-1],ntree=1000)
# disc_pred1<-predict(rf.mod1x,testing1[,c(-1)])
# testing1$disc_pred1<-disc_pred1
# cor(testing1$discharge,testing1$disc_pred1)
# NSE(sim=testing1$disc_pred1,obs = testing1$discharge)
# ggof(sim=testing1$disc_pred1,obs=testing1$discharge,ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "RF simulation vs Observations")
# #Output rf.mod1x: NSE:0.68, r:0.83 80/20 split

#predict all data
ruliba_timeseries_pred$discRFmod1X<-predict(rf.mod1x,na.roughfix(ruliba_timeseries_pred[,c(-1)]))
write.csv(ruliba_timeseries_pred, paste0(dir1,"./B2P/ML/discharge/predicted/ruliba_timeseries_predRF_vRFmod1X.csv"), row.names = F)

ruliba_timeseries_pred<-merge(ruliba_timeseries_pred,ruliba_lai,by="date")
ruliba_timeseries_pred<-ruliba_timeseries_pred[,c(1:19)]

write.csv(ruliba_timeseries_pred,"../csv/ruliba_timeseries_pred_finalDec01.csv",row.names = F)

ruliba_timeseries_pred$dischargeRF<-predict(rf.mod6,na.roughfix(ruliba_timeseries_pred[,c(-1)]))

ruliba_timeseries_pred<-ruliba_timeseries_pred[ruliba_timeseries_pred$date>"2002-12-31",]
write.csv(ruliba_timeseries_pred,"../discharge/predicted/ruliba_timeseries_pred_finalDec01_v1.csv",row.names = F)

# write.csv(ruliba_timeseries_pred, paste0(dir1,"./B2P/ML/discharge/predicted/ruliba_timeseries_predRF_vRFmod.csv"), row.names = F)


# #predict with scaled data (scale by month min/max) rf.mod3
index3 = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.8,0.2))
training3 = ruliba_ml_data[index3==1,]
testing3 = ruliba_ml_data[index3==2,]

library(lubridate)

# training2$mon<-month(as.Date(training2$date))
# training2$month<-month.abb[training2$mon]
#
# testing2$mon<-month(as.Date(testing2$date))
# testing2$month<-month.abb[testing2$mon]

training.scaled2<-training3 %>% mutate_if(is.numeric,scale)
testing.scaled2<-testing3 %>% mutate_if(is.numeric,scale)
rf.mod3<-randomForest(discharge~.,data = training.scaled2[,-1])
disc_pred3<-predict(rf.mod3,testing.scaled2[,-1])
testing.scaled2$disc_pred3<-disc_pred3

testing.scaled2<-na.omit(testing.scaled2)

cor(testing.scaled2$discharge,testing.scaled2$disc_pred3)
NSE.data.frame(sim=testing.scaled2$disc_pred3,obs=testing.scaled2$discharge)
mae.data.frame(sim=testing.scaled2$disc_pred3,obs=testing.scaled2$discharge)

#Output rf.mod3: NSE:0.65, r:0.81
# 



##SVM mod4

library(quantmod)
library(e1071)
# install.packages("quantmod","e1071")

index7 = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.75,0.25))
training7 = ruliba_ml_data[index7==1,]
testing7 = ruliba_ml_data[index7==2,]
# training= training[c(-19)]
# testing= testing[c(-19)]

# training.xts<-xts(training[,1:19],order.by = as.Date(training[,1]))
# testing.xts<-xts(testing[,1:19],order.by = as.Date(testing[,1]))

svmt<-svm(discharge~.,data=training7[,-1],kernel='linear',type="eps", cost=1.0,epsilon=0.1)
testing7$svmpred<-predict(svmt,testing7[,-1])
cor(testing7$svmpred,testing7$discharge)
NSE(testing7$svmpred,testing7$discharge)

#Output SVM mod4: NSE:0.45, r:0.71


#GLM
ruliba_ml_data_resc<-ruliba_ml_data
ruliba_ml_data_resc$mon<-month(as.Date(ruliba_ml_data_resc$date))
ruliba_ml_data_resc<-ruliba_ml_data_resc %>% group_by(mon) %>% mutate_if(is.numeric, scale)

index8 = sample(2,nrow(ruliba_ml_data_resc), replace = TRUE, prob = c(0.75,0.25))
training8 = ruliba_ml_data_resc[index8==1,]
testing8 = ruliba_ml_data_resc[index8==2,]

glm.train<-glm(discharge ~., data=training8[,-1], family = gaussian())
testing8$glm_disc<-predict(glm.train,testing8[,-1])
cor(testing8$glm_disc,testing8$discharge)
NSE(testing8$glm_disc,testing8$discharge)

#Output GLM mod4: NSE:0.51, r:0.71


glm.rul<-glm(discharge ~ rf.lag2d+rf.lag3d+tmin.lag2d+tmax+tmax.lag1d+sm+sm.lag1d+sm.lag2d+sm.lag3d, family = gaussian(), data = training5)
with(summary(glm.rul), 1 - deviance/null.deviance) #McFadden R2=0.52 (v good)

glm.pred<-predict(glm.rul,testing5)
testing5$glm_disc2<-glm.pred
cor(testing5$glm_disc2,testing5$discharge)
NSE(testing5$glm_disc2,testing5$discharge)

#Output GLM2 mod4: NSE:0.49, r:0.7 -- with only significant variables


#timeseries plots- single panel
plot.data<-ruliba_ml_data[,c("rf_mm","rf.lag2d","rf.lag3d","discharge")]
colnames(plot.data)<-c("R[mm/d]","R2d_lag","R3d_lag","Q[m3/s]")
discharges.ts=ts(plot.data,frequency=365.25,start=c(2018,3,1))
timeSeries::plot(discharges.ts,main="Flow and rainfall time series",xlab="")



plot.data2<-ruliba_ml_data[,c("sm","sm.lag1d","sm.lag2d","discharge")]
colnames(plot.data2)<-c("SM[m3/m3]","SM1d_lag","SM2d_lag","Q[m3/s]")
discharges.ts2=ts(plot.data2,frequency=365.25,start=c(2018,3,1))
timeSeries::plot(discharges.ts2,main="Flow and soil moisture time series",xlab="")


plot.data3<-ruliba_timeseries_pred[,c("sm","discRFmod1X")]
colnames(plot.data3)<-c("SM[m3/m3]","Q[m3/s]")
discharges.ts3=ts(plot.data3,frequency=365.25,start=c(2001,1,1))
timeSeries::plot(discharges.ts3,main="Flow and soil moisture time series",xlab="")

cor(ruliba_timeseries_pred$discRFmod1X,ruliba_timeseries_pred$rf.lag3d, use = "complete.obs")
cor(ruliba_timeseries_pred$discRFmod1X,ruliba_timeseries_pred$sm,use = "complete.obs")

cor(ruliba_ml_data$discharge,ruliba_ml_data$rf.lag3d)

# install.packages("corrplot")
# library(corrplot)
# install.packages("ggcorrplot")
# library(ggcorrplot)
# 
# cor.ml<-ruliba_ml_data[,-1]
# ggcorrplot(cor.ml)


#################################################
#END

###################################


#plotting with hydroGOF and hydroTSM
library(hydroGOF)
vic_compare<-read.csv("./ML/csv/vic_compare_ruliba.csv")
head(vic_compare)
library(zoo)
library(hydroTSM)

NSE(vic_compare$VIC_Q,vic_compare$Obs_Q)
NSE(vic_compare$RF_Q,vic_compare$Obs_Q)
NSE(vic_compare$HBV_Q,vic_compare$Obs_Q)

gof(sim=vic_compare$VIC_Q,obs=vic_compare$Obs_Q)
gof(sim=vic_compare$RF_Q,obs=vic_compare$Obs_Q)
gof(sim=vic_compare$HBV_Q,obs=vic_compare$Obs_Q)

ggof(sim=vic_compare$RF_Q,obs=vic_compare$Obs_Q,dates=vic_compare$date, date.fmt = "%Y-%m-%d", ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "RF simulation vs Observations")
ggof(sim=vic_compare$VIC_Q,obs=vic_compare$Obs_Q,dates=vic_compare$date, date.fmt = "%Y-%m-%d", ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "VIC simulation vs Observations")
ggof(sim=vic_compare$HBV_Q,obs=vic_compare$Obs_Q,dates=vic_compare$date, date.fmt = "%Y-%m-%d", ftype="dm", gofs = c("NSE", "rNSE", "ME", "r", "KGE","RMSE", "PBIAS"), FUN=mean, main = "HBV simulation vs Observations")


#xgboost

install.packages("drat", repos="https://cran.rstudio.com")


####NEW ANALYSIS WITH UPDATED DATA
# Working directory : ("C:/Users/DENNIS/MCGE Dropbox/Denis Macharia/B2P")
# #IMERGL (daily)- 20010101:20221020- SERVIR climateServ
# ## GTS min and max (daily)- 20010101:20221020
# ##CCI Soil moisture (daily)- 20000101-20211231
# ##CDS soil moisture (daily)- 20100101-20220731**-check to confirm- Copernicus
# ##E-MODIS NDVI (10-day)- 20020101:20221020 - SERVIR climateServ
# #Observed streamflow- Ruliba with telemetry-- inside observed discharge folder in ./ML/csv/discharge..
##LSTM model- https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

##Flood frequency analysis- https://rpubs.com/cassiorampinelli/528388
##Combine year,mon,day into a date with Lubridate: make_date(year = df$yearnum, month = df$monthnum, day = df$daynum)


#LSTM

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
# install.packages("timerk","tidyquant","tibbletime")
install.packages("C:/Users/demu4180/Downloads/tidyquant_1.0.5.zip", repos = NULL, type = "win.binary")
install.packages("C:/Users/demu4180/Downloads/tibbletime_0.1.6.zip", repos = NULL, type = "win.binary")
install.packages("C:/Users/demu4180/Downloads/timetk_2.8.1.zip", repos = NULL, type = "win.binary")
install.packages("C:/Users/demu4180/Downloads/PerformanceAnalytics_2.0.4.zip", repos = NULL, type = "win.binary")
install.packages("C:/Users/demu4180/Downloads/quadprog_1.5-8.zip", repos = NULL, type = "win.binary")
install.packages("C:/Users/demu4180/Downloads/Quandl_2.11.0.zip", repos = NULL, type = "win.binary")

library(timetk)
library(tidyquant)
library(tibbletime)
library(quadprog)
library(PerformanceAnalytics)
library(Quandl)

# Visualization
install.packages("cowplot")
library(cowplot)

# Preprocessing
install.packages("recipes")
library(recipes)

# Sampling / Accuracy
install.packages("rsample","yardstick")
library(rsample)
install.packages("C:/Users/demu4180/Downloads/yardstick_1.1.0.zip", repos = NULL, type = "win.binary")

library(yardstick) 

# Modeling
library(keras)
sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)

sun_spots

p1 <- sun_spots %>%
  ggplot(aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "From 1749 to 2013 (Full Data Set)"
  )

p2 <- sun_spots %>%
  filter_time("start" ~ "1800") %>%
  ggplot(aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1749 to 1800 (Zoomed In To Show Cycle)",
    caption = "datasets::sunspot.month"
  )

p_title <- ggdraw() + 
  draw_label("Sunspots", size = 18, fontface = "bold", colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))


#ACF
tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}

max_lag <- 12 * 50
sun_spots %>%
  tidy_acf(value, lags = 0:max_lag)



###Plot discharge for DIV report
dir3<-setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/discharge/predicted/vic/enacts/20112013/formatted")
Nyarusange<-read.csv(paste0(dir3,"./Nya_vic_daily.csv"))
Gasasa<-read.csv(paste0(dir3,"./Gas_vic_daily.csv"))
Kwiterambere<-read.csv(paste0(dir3,"./Kwi_vic_daily.csv"))
Ntaruka<-read.csv(paste0(dir3,"./Nta_vic_daily.csv"))
Rugeshi<-read.csv(paste0(dir3,"./Rug_vic_daily.csv"))
Uwumugeti<-read.csv(paste0(dir3,"./Uwu_vic_daily.csv"))
Muhembe<-read.csv(paste0(dir3,"./Muh_vic_daily.csv"))
Mutiwingoma<-read.csv(paste0(dir3,"./Mut_vic_daily.csv"))
Ruliba<-read.csv(paste0(dir3,"./Rul_vic_dailyv.csv"))

Nyarusange<-Nyarusange[Nyarusange$discharge<=500,]
Nyarusange$date2<-as.Date(Nyarusange$date)
Gasasa$date2<-as.Date(Gasasa$date)
Kwiterambere$date2<-as.Date(Kwiterambere$date)
Kwiterambere<-Kwiterambere[Kwiterambere$discharge<=25,]
Ntaruka$date2<-as.Date(Ntaruka$date)
Ntaruka<-Ntaruka[Ntaruka$discharge<=25,]
Rugeshi$date2<-as.Date(Rugeshi$date)
Uwumugeti$date2<-as.Date(Uwumugeti$date)
Muhembe$date2<-as.Date(Muhembe$date)
Mutiwingoma$date2<-as.Date(Mutiwingoma$date)
Ruliba$date2<-as.Date(Ruliba$date)
Ruliba<-Ruliba[Ruliba$discharge<=1000,]


p1<-ggplot(Nyarusange, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("date")+ylab("Discharge,[m3/s]")+ggtitle("Nyarusange")
p2<-ggplot(Gasasa, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Gasasa")
p3<-ggplot(Kwiterambere, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Kwiterambere")
p4<-ggplot(Ntaruka, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Ntaruka")
p5<-ggplot(Rugeshi, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Rugeshi")
p6<-ggplot(Uwumugeti, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Uwumugeti")
p7<-ggplot(Muhembe, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Muhembe")
p8<-ggplot(Mutiwingoma, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Mutiwingoma")
p9<-ggplot(Ruliba, aes(x=date2,y=discharge,group=1))+geom_line()+xlab("")+ylab("")+ggtitle("Ruliba-calib-station")

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol=3)

ruliba.telemetry<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/discharge/observed/ruliba_new27Oct2022/ruliba_calibration_telemetry.csv")
ruliba.telemetry$date2<-as.Date(ruliba.telemetry$date)
ruliba.manual<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/discharge/observed/ruliba_calibration.csv")
ruliba.manual$date2<-as.Date(ruliba.manual$date)
p1<-ggplot(ruliba.manual, aes(x=date2,y=Q,group=1))+geom_line()+xlab("")+ylab("Q,[m3/s]")+ggtitle("Manual station data")

p2<-ggplot(ruliba.telemetry, aes(x=date2,y=Q,group=1))+geom_line()+xlab("")+ylab("Q,[m3/s]")+ggtitle("New telemetry data")

grid.arrange(p1,p2,ncol=1)


###Flood frequency analysis
Q_fdc<-read.csv(paste0(dir1, "./B2P/ML/discharge/predicted/RF_predictedQ_forFDC.csv"))
library(timeSeries)
Qfdc.ts<-ts(Q_fdc)
timeSeries::plot(Qfdc.ts)
summary(Qfdc.ts)

flow<-Q_fdc$dischargeRF
flow<-sort(flow,decreasing=T)
df<-data.frame(x=100/length(flow)*1:length(flow),y=flow)
plot(x = df$x, y = df$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Less Than (%)",main="Flow Duration Curve")
grid()

x=df$x
y=df$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

duration.dataframe


#optimizing RF model

#https://afit-r.github.io/random_forests

library(mlbench)
library(caret)
library(caret)
library(e1071)
library(randomForest)
library(rsample)
# install.packages("h2o")
library(h2o)
library(tidyverse)

set.seed(123)
index7 = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.75,0.25))
trainingRF = ruliba_ml_data[index7==1,]
testingRF = ruliba_ml_data[index7==2,]

m1 <- randomForest(
  formula = discharge ~ .,
  data    = trainingRF
)
m1


which.min(m1$mse)
sqrt(m1$mse[which.min(m1$mse)])
#m1 % Var explained: 75.7, rmse 18.99


# set.seed(123)
# valid_split <- initial_split(trainingRF, .8)
# ames_train_v2 <- analysis(valid_split)
# ames_valid <- assessment(valid_split)
# 
# x_test <- ames_valid[setdiff(names(ames_valid), "discharge")]
# y_test <- ames_valid$discharge
# rf_oob_comp <- randomForest(
#   formula = discharge ~ .,
#   data    = ames_train_v2,
#   xtest   = x_test,
#   ytest   = y_test
# )
# 
# oob <- sqrt(rf_oob_comp$mse)
# validation <- sqrt(rf_oob_comp$test$mse)
# 
# tibble::tibble(
#   `Out of Bag Error` = oob,
#   `Test error` = validation,
#   ntrees = 1:rf_oob_comp$ntree
# ) %>%
#   gather(Metric, RMSE, -ntrees) %>%
#   ggplot(aes(ntrees, RMSE, color = Metric)) +
#   geom_line() +
#   scale_y_continuous(labels = scales::dollar) +
#   xlab("Number of trees")
# 
# features <- setdiff(names(trainingRF), "discharge")
# set.seed(123)
# m2 <- tuneRF(
#   x          = trainingRF[features],
#   y          = trainingRF$discharge,
#   ntreeTry   = 500,
#   mtryStart  = 5,
#   stepFactor = 1.5,
#   improve    = 0.01,
#   trace      = FALSE      # to not show real-time progress 
# )

#Full grid search with H2O
h2o.no_progress()
h2o.init(max_mem_size = "5g")
ames_train<-na.omit(trainingRF[,-1])

y <- "discharge"
x <- setdiff(names(ames_train), y)

train.h2o <- as.h2o(trainingRF[,-1])

# hyperparameter grid

hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 150),
  mtries      = seq(15, 35, by = 10),
  max_depth   = seq(20, 40, by = 5),
  min_rows    = seq(1, 5, by = 2),
  nbins       = seq(10, 30, by = 5),
  sample_rate = c(.55, .632, .75)
)

search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

grid_perf <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

ames_test<-na.omit(testingRF[,-1])
ames_test.h2o <- as.h2o(ames_test)
best_model_perf <- h2o.performance(model = best_model, newdata = ames_test.h2o)
h2o.mse(best_model_perf) %>% sqrt()
#rsme 21.18643


# h2o
pred_h2o <- predict(best_model, ames_test.h2o)
head(pred_h2o)
h2p<-as.data.frame(pred_h2o)
rf.h2o<-cbind(ames_test,h2p)
cor(rf.h2o$predict,rf.h2o$discharge)
NSE(rf.h2o$predict,rf.h2o$discharge)
rmse(rf.h2o$predict,rf.h2o$discharge)
mae(rf.h2o$predict,rf.h2o$discharge)

#rf.modh2o tuned-testing: cor 0.85, nse 0.72, mae 14.6, rmse 21.19

#predict training data
train_h2o <- predict(best_model, train.h2o)
train_pred<-as.data.frame(train_h2o)
train_pred_df<-cbind(ames_train,train_pred)
NSE(train_pred_df$predict,train_pred_df$discharge)
cor(train_pred_df$predict,train_pred_df$discharge)
mae(train_pred_df$predict,train_pred_df$discharge)
rmse(train_pred_df$predict,train_pred_df$discharge)
#rf.modh2o tuned-training: cor 0.99, nse 0.98, mae 3.55, rmse 5.11


#tune with mtry
bestmtry <- tuneRF(trainingRF[,-1],trainingRF$discharge,stepFactor = 1.2, improve = 0.01, trace=T, plot= T)

bestmtry2 <- tuneRF(trainingRF[,-1],trainingRF$discharge,stepFactor = 1.2, improve = 0.01, trace=T, plot= T)


hyper_tune<-list(
  ntrees=500,
  mtries=18
)

model.tuned<-randomForest(discharge~.,trainingRF[,-1],ntree=500,mtry=18)
testingRF$test_disc<-predict(model.tuned,testingRF[,-1])
cor(testingRF$test_disc,testingRF$discharge)
NSE(testingRF$test_disc,testingRF$discharge)
#model.tuned r = 0.94, NSE = 0.87

# model.tuned2<-randomForest(discharge~.,trainingRF[,-1],ntree=500,mtry=18)
# testingRF$test_disc2<-predict(model.tuned2,testingRF[,-1])
# cor(testingRF$test_disc2,testingRF$discharge)
# NSE(testingRF$test_disc2,testingRF$discharge)

##MWAKA TEST
mwaka_ndvi<-read.csv(paste0(dir1,"./B2P/ML/emodis/mwaka_ndvi.csv"))
mwaka_discharge<-read.csv(paste0(dir1,"./B2P/ML/discharge/observed/mwaka_final/mwaka_discharge_ml.csv"))
mwaka_testcm1<-merge(mwaka_discharge,mwaka_ndvi,by="date")
cor(mwaka_testcm1$discharge,mwaka_testcm1$ndvi)

mwaka_rf<-imergl_mwaka
mwaka_tmin<-tmin_mwaka
mwaka_tmax<-tmax_mwaka
mwaka_sm<-sm_mwaka
mwaka_discharge<-mwaka_discharge
mwaka_ndvi<-mwaka_ndvi

mwaka_rf$rf.lag1d<-dplyr::lag(mwaka_rf$imergl_mm, 1)
mwaka_rf$rf.lag2d<-dplyr::lag(mwaka_rf$imergl_mm, 2)
mwaka_rf$rf.lag3d<-dplyr::lag(mwaka_rf$imergl_mm, 3)

mwaka_tmin$tmin.lag1d<-dplyr::lag(mwaka_tmin$tmin, 1)
mwaka_tmin$tmin.lag2d<-dplyr::lag(mwaka_tmin$tmin, 2)
mwaka_tmin$tmin.lag3d<-dplyr::lag(mwaka_tmin$tmin, 3)


mwaka_tmax$tmax.lag1d<-dplyr::lag(mwaka_tmax$tmax, 1)
mwaka_tmax$tmax.lag2d<-dplyr::lag(mwaka_tmax$tmax, 2)
mwaka_tmax$tmax.lag3d<-dplyr::lag(mwaka_tmax$tmax, 3)


mwaka_sm$sm.lag1d<-dplyr::lag(mwaka_sm$sm, 1)
mwaka_sm$sm.lag2d<-dplyr::lag(mwaka_sm$sm, 2)
mwaka_sm$sm.lag3d<-dplyr::lag(mwaka_sm$sm, 3)


#merge all variables into one dataframe
merge1<-merge(mwaka_rf,mwaka_tmin,by="date")
merge2<-merge(mwaka_tmax,mwaka_sm,by="date")
merge3<-merge(merge1,merge2,by="date")
merge4<-merge(merge3,mwaka_ndvi,by="date")
merge5<-merge(merge4,mwaka_discharge,by="date")
mwaka_timeseries_pred<-merge5
names(mwaka_timeseries_pred)[2]<-"rf_mm"

write.csv(mwaka_timeseries_pred, paste0(dir1, "./B2P/ML/csv/mwaka_timeseries_pred.csv",row.names=F))

mwaka_timeseries_pred<-read.csv(paste0(dir1, "./B2P/ML/csv/mwaka_timeseries_pred.csv"))

mwaka_discRF<-predict(rf.mod1xwA,na.roughfix(mwaka_timeseries_pred[,-1]))
mwaka_timeseries_pred$disc_pred<-mwaka_discRF
cor(mwaka_timeseries_pred$disc_pred,mwaka_timeseries_pred$discharge)
NSE(mwaka_timeseries_pred$disc_pred,mwaka_timeseries_pred$discharge)


##revised
index5 = sample(2,nrow(ruliba_ml_data), replace = TRUE, prob = c(0.8,0.2))
training5 = ruliba_ml_data[index5==1,]
testing5 = ruliba_ml_data[index5==2,]

training5$rf_mm1<-training5$rf_mm
training5$rf.lag1d1<-training5$rf.lag1d
training5$rf.lag2d1<-training5$rf.lag2d
training5$rf.lag3d1<-training5$rf.lag3d
training5$sm<-training5$sm
training5$sm.lag1d1<-training5$sm.lag1d
training5$sm.lag2d1<-training5$sm.lag2d
training5$sm.lag3d1<-training5$sm.lag3d
training5<-training5[,c(1,20:23,6:14,24:26,18:19)]


testing5$rf_mm1<-testing5$rf_mm
testing5$rf.lag1d1<-testing5$rf.lag1d
testing5$rf.lag2d1<-testing5$rf.lag2d
testing5$rf.lag3d1<-testing5$rf.lag3d
testing5$sm<-testing5$sm
testing5$sm.lag1d1<-testing5$sm.lag1d
testing5$sm.lag2d1<-testing5$sm.lag2d
testing5$sm.lag3d1<-testing5$sm.lag3d
testing5<-testing5[,c(1,20:23,6:14,24:26,18:19)]

rf.modrev<-randomForest(discharge~.,data=training5[,-1],ntree=1000)
testing5$modrev_disc<-predict(rf.modrev,testing5[,-1])

##mwaka predict
mwaka_pred<-mwaka_timeseries_pred[,c(1:19)]

mwaka_pred$rf_mm1<-mwaka_pred$rf_mm
mwaka_pred$rf.lag1d1<-mwaka_pred$rf.lag1d
mwaka_pred$rf.lag2d1<-mwaka_pred$rf.lag2d
mwaka_pred$rf.lag3d1<-mwaka_pred$rf.lag3d
mwaka_pred$sm<-mwaka_pred$sm
mwaka_pred$sm.lag1d1<-mwaka_pred$sm.lag1d
mwaka_pred$sm.lag2d1<-mwaka_pred$sm.lag2d
mwaka_pred$sm.lag3d1<-mwaka_pred$sm.lag3d
mwaka_pred<-mwaka_pred[,c(1,20:23,6:14,24:26,18:19)]
mwaka_pred$discharge<-mwaka_pred$discharge


mwaka_pred$modrev_disc<-predict(rf.modrev,mwaka_pred[,-1])
mwaka_pred$modrev_disc<-mwaka_pred$modrev_disc/(7175/1795)
MAE(mwaka_pred$modrev_disc,mwaka_pred$discharge)
write.csv(mwaka_pred,"C:/vic/mwaka_pred.csv",row.names = F)
ggof(mwaka_pred$modrev_disc,mwaka_pred$discharge,dates=mwaka_pred$date,gofs = c("r","KGE","NSE"))


#plotting precip and streamflow on the same graph
# https://rpubs.com/cxiao/hydrograph-ggplot2-plot
#https://cran.r-project.org/web/packages/CSHShydRology/vignettes/hydrograph_plot.html
# install.packages("CSHShydRology")
library(CSHShydRology)





#bridge watershed areas
library(rgdal)

dir4<-setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/")

nyarusange_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/nyarusange_catchment_dis.shp", layer = "nyarusange_catchment_dis")
ntaruka_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/ntaruka_catchment_dis.shp", layer = "ntaruka_catchment_dis")
kwiterambere_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/kwiterambere_catchment_dis.shp", layer = "kwiterambere_catchment_dis")
muhembe_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/muhembe_catchment_dis.shp", layer = "muhembe_catchment_dis")
rugeshi_gasasa_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/rugeshi-gasasa_catchment_dis.shp", layer = "rugeshi-gasasa_catchment_dis")
mutiwingoma_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/mutiwingoma_catchment_dis1.shp", layer = "mutiwingoma_catchment_dis1")
uwumugeti_basin<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/uwumugeti-kigusa_catchment_dis.shp", layer = "uwumugeti-kigusa_catchment_dis")

all_basins<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/all_basins_1.shp", layer = "all_basins_1")
all_rivers<-readOGR(dsn = "C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/data/hydrology/catchments/for_bridges/rivers_camera_basinsprj1.shp", layer = "rivers_camera_basinsprj1")

plot(ruliba_catchment)
plot(mutiwingoma_basin,col="blue",add=T)
plot(ntaruka_basin,col="blue",add=T)
plot(muhembe_basin,col="blue",add=T)
plot(uwumugeti_basin,col="blue",add=T)
plot(rugeshi_gasasa_basin,col="blue",add=T)
plot(nyarusange_basin,col="blue",add=T)
plot(kwiterambere_basin,col="blue",add=T)
plot(bridges,col="black",add=T)
plot(bridges,col="black",pch=16,add=T)
axis(1)
axis(2)
box(col = 'black')

#calculate area of each watershed/basin in KM^2

crs(all_basins)
library(sf)

all_basins_area<-st_as_sf(all_basins)
st_area(all_basins_area)
all_basins_area$area<-st_area(all_basins_area)/1000000

#extract meteorological variables for each bridge watershed

imergl_all_basins<-raster::extract(imergl.stk, all_basins, fun=mean, na.rm=TRUE) #rainfall
tmin_all_basins<-raster::extract(tmin.stk, all_basins, fun=mean,na.rm=TRUE) #tmin
tmax_all_basins<-raster::extract(tmax.stk, all_basins, fun=mean,na.rm=TRUE) #tmax
sm_all_basins<-raster::extract(soil.moisture, all_basins, fun=mean,na.rm=TRUE) #soil moisture a
cdssm_all_basins<-raster::extract(cds.crop,all_basins,fun=mean,na.rm=T) #soil moisture b

imergl_all_basins_t<-t(imergl_all_basins)
imergl_all_basins_t<-as.data.frame(imergl_all_basins_t)
rownames(imergl_all_basins_t)<-dates.imergl
imergl_all_basins_t<-rownames_to_column(imergl_all_basins_t,var="date")
colnames(imergl_all_basins_t)[2:8]<-all_basins$name

tmin_all_basins_t<-t(tmin_all_basins)
tmin_all_basins_t<-as.data.frame(tmin_all_basins_t)
rownames(tmin_all_basins_t)<-dates.temp
tmin_all_basins_t<-rownames_to_column(tmin_all_basins_t,var="date")
colnames(tmin_all_basins_t)[2:8]<-all_basins$name

tmax_all_basins_t<-t(tmax_all_basins)
tmax_all_basins_t<-as.data.frame(tmax_all_basins_t)
rownames(tmax_all_basins_t)<-dates.temp
tmax_all_basins_t<-rownames_to_column(tmax_all_basins_t,var="date")
colnames(tmax_all_basins_t)[2:8]<-all_basins$name

cci_sm_all_basins_t<-t(sm_all_basins)
cci_sm_all_basins_t<-as.data.frame(cci_sm_all_basins_t)
rownames(cci_sm_all_basins_t)<-dates.cci.sm
cci_sm_all_basins_t<-rownames_to_column(cci_sm_all_basins_t,var="date")
colnames(cci_sm_all_basins_t)[2:8]<-all_basins$name

cds_sm_all_basins_t<-t(cdssm_all_basins)
cds_sm_all_basins_t<-as.data.frame(cds_sm_all_basins_t)
rownames(cds_sm_all_basins_t)<-dates.cds.sm
cds_sm_all_basins_t<-rownames_to_column(cds_sm_all_basins_t,var="date")
colnames(cds_sm_all_basins_t)[2:8]<-all_basins$name

#combine cci-soil moisture and cds soil moisture (cci ends on 31/12/2021 whereas cds ends on 20/10/22)
all_basins_sm<-rbind(cci_sm_all_basins_t,cds_sm_all_basins_t)
colnames(all_basins_sm)[6]<-"Rugeshi_Gasasa"

sapply(all_basins_sm, function(x) sum(is.na(x))) #find length of missing data
all_basins_sm<-all_basins_sm %>%
          fill(2:8, .direction = 'down')

#lai and ndvi
setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/emodis")

mut_lai<-read.csv("./lai/mutiwingoma_lai_final.csv")
nta_lai<-read.csv("./lai/ntaruka_lai_final.csv")
kwi_lai<-read.csv("./lai/kwiterambere_lai_final.csv")
muh_lai<-read.csv("./lai/muhembe_lai_final.csv")
rug_lai<-read.csv("./lai/rugeshi_lai_final.csv")
uwu_lai<-read.csv("./lai/uwumugeti_lai_final.csv")
nya_lai<-read.csv("./lai/nyarusange_lai_final.csv")

mut_ndvi<-read.csv("./ndvi/mutiwingoma_ndvi_final.csv")
nta_ndvi<-read.csv("./ndvi/ntaruka_ndvi_final.csv")
kwi_ndvi<-read.csv("./ndvi/kwiterambere_ndvi_final.csv")
muh_ndvi<-read.csv("./ndvi/muhembe_ndvi_final.csv")
rug_ndvi<-read.csv("./ndvi/rugeshi_ndvi_final.csv")
uwu_ndvi<-read.csv("./ndvi/uwumugeti_ndvi_final.csv")
nya_ndvi<-read.csv("./ndvi/nyarusange_ndvi_final.csv")


#streamflow modeling for each bridge watershed
#1. mutiwingoma
mut_imergl<-imergl_all_basins_t[,c("date","Mutiwingoma")]
colnames(mut_imergl)[2]<-"rf_mm"
mut_tmin<-tmin_all_basins_t[,c("date","Mutiwingoma")]
colnames(mut_tmin)[2]<-"tmin"
mut_tmax<-tmax_all_basins_t[,c("date","Mutiwingoma")]
colnames(mut_tmax)[2]<-"tmax"
mut_sm<-all_basins_sm[,c("date","Mutiwingoma")]
colnames(mut_sm)[2]<-"sm"

mutiwingoma_ml_data<-list(mut_imergl,mut_tmin,mut_tmax,mut_sm,mut_ndvi,mut_lai) %>% reduce(inner_join,by="date")

mutiwingoma_ml_data$rf.lag1d<-dplyr::lag(mutiwingoma_ml_data$rf_mm, 1)
mutiwingoma_ml_data$rf.lag2d<-dplyr::lag(mutiwingoma_ml_data$rf_mm, 2)
mutiwingoma_ml_data$rf.lag3d<-dplyr::lag(mutiwingoma_ml_data$rf_mm, 3)

mutiwingoma_ml_data$tmin.lag1d<-dplyr::lag(mutiwingoma_ml_data$tmin, 1)
mutiwingoma_ml_data$tmin.lag2d<-dplyr::lag(mutiwingoma_ml_data$tmin, 2)
mutiwingoma_ml_data$tmin.lag3d<-dplyr::lag(mutiwingoma_ml_data$tmin, 3)


mutiwingoma_ml_data$tmax.lag1d<-dplyr::lag(mutiwingoma_ml_data$tmax, 1)
mutiwingoma_ml_data$tmax.lag2d<-dplyr::lag(mutiwingoma_ml_data$tmax, 2)
mutiwingoma_ml_data$tmax.lag3d<-dplyr::lag(mutiwingoma_ml_data$tmax, 3)


mutiwingoma_ml_data$sm.lag1d<-dplyr::lag(mutiwingoma_ml_data$sm, 1)
mutiwingoma_ml_data$sm.lag2d<-dplyr::lag(mutiwingoma_ml_data$sm, 2)
mutiwingoma_ml_data$sm.lag3d<-dplyr::lag(mutiwingoma_ml_data$sm, 3)

mutiwingoma_ml_data<-mutiwingoma_ml_data[mutiwingoma_ml_data$date>"2002-12-31",]

sapply(mutiwingoma_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

mutiwingoma_ml_pred<-as.h2o(mutiwingoma_ml_data[,-1])

# mutiwingoma_ml_pred$disc_pred<-predict(best_model,mutiwingoma_ml_pred)

# mutiwingoma_ml_pred$discharge<-predict(model.tuned,mutiwingoma_ml_pred)
mutiwingoma_ml_data$disc_pred<-predict(rf.mod6,mutiwingoma_ml_data[,-1])

mut_basin<-st_as_sf(mutiwingoma_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
mut_area<-st_area(mut_basin)/1000000 
q_ratio<-rul_area/mut_area
mutiwingoma_ml_data$disc_pred2<-mutiwingoma_ml_data$disc_pred*(mut_area/rul_area)
write.csv(mutiwingoma_ml_data,"../discharge/predicted/mutiwingoma_discharge_predictedRF.csv",row.names = F)


#2. Ntaruka

nta_imergl<-imergl_all_basins_t[,c("date","Ntaruka")]
colnames(nta_imergl)[2]<-"rf_mm"
nta_tmin<-tmin_all_basins_t[,c("date","Ntaruka")]
colnames(nta_tmin)[2]<-"tmin"
nta_tmax<-tmax_all_basins_t[,c("date","Ntaruka")]
colnames(nta_tmax)[2]<-"tmax"
nta_sm<-all_basins_sm[,c("date","Ntaruka")]
colnames(nta_sm)[2]<-"sm"

ntaruka_ml_data<-list(nta_imergl,nta_tmin,nta_tmax,nta_sm,nta_ndvi,nta_lai) %>% reduce(inner_join,by="date")

ntaruka_ml_data$rf.lag1d<-dplyr::lag(ntaruka_ml_data$rf_mm, 1)
ntaruka_ml_data$rf.lag2d<-dplyr::lag(ntaruka_ml_data$rf_mm, 2)
ntaruka_ml_data$rf.lag3d<-dplyr::lag(ntaruka_ml_data$rf_mm, 3)

ntaruka_ml_data$tmin.lag1d<-dplyr::lag(ntaruka_ml_data$tmin, 1)
ntaruka_ml_data$tmin.lag2d<-dplyr::lag(ntaruka_ml_data$tmin, 2)
ntaruka_ml_data$tmin.lag3d<-dplyr::lag(ntaruka_ml_data$tmin, 3)


ntaruka_ml_data$tmax.lag1d<-dplyr::lag(ntaruka_ml_data$tmax, 1)
ntaruka_ml_data$tmax.lag2d<-dplyr::lag(ntaruka_ml_data$tmax, 2)
ntaruka_ml_data$tmax.lag3d<-dplyr::lag(ntaruka_ml_data$tmax, 3)


ntaruka_ml_data$sm.lag1d<-dplyr::lag(ntaruka_ml_data$sm, 1)
ntaruka_ml_data$sm.lag2d<-dplyr::lag(ntaruka_ml_data$sm, 2)
ntaruka_ml_data$sm.lag3d<-dplyr::lag(ntaruka_ml_data$sm, 3)

ntaruka_ml_data<-ntaruka_ml_data[ntaruka_ml_data$date>"2002-12-31",]

sapply(ntaruka_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

ntaruka_ml_pred<-as.h2o(ntaruka_ml_data[,-1])

# ntaruka_ml_pred$disc_pred<-predict(best_model,ntaruka_ml_pred)

# ntaruka_ml_pred$discharge<-predict(model.tuned,ntaruka_ml_pred)
ntaruka_ml_data$disc_pred<-predict(rf.mod6,ntaruka_ml_data[,-1])

nta_basin<-st_as_sf(ntaruka_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
nta_area<-st_area(nta_basin)/1000000 
q_ratio_ntaruka<-rul_area/nta_area
ntaruka_ml_data$disc_pred2<-ntaruka_ml_data$disc_pred*(nta_area/rul_area)
write.csv(ntaruka_ml_data,"../discharge/predicted/ntaruka_discharge_predictedRF.csv",row.names = F)


#3.Kwiterambere
kwi_imergl<-imergl_all_basins_t[,c("date","Kwiterambere")]
colnames(kwi_imergl)[2]<-"rf_mm"
kwi_tmin<-tmin_all_basins_t[,c("date","Kwiterambere")]
colnames(kwi_tmin)[2]<-"tmin"
kwi_tmax<-tmax_all_basins_t[,c("date","Kwiterambere")]
colnames(kwi_tmax)[2]<-"tmax"
kwi_sm<-all_basins_sm[,c("date","Kwiterambere")]
colnames(kwi_sm)[2]<-"sm"

kwiterambere_ml_data<-list(kwi_imergl,kwi_tmin,kwi_tmax,kwi_sm,kwi_ndvi,kwi_lai) %>% reduce(inner_join,by="date")

kwiterambere_ml_data$rf.lag1d<-dplyr::lag(kwiterambere_ml_data$rf_mm, 1)
kwiterambere_ml_data$rf.lag2d<-dplyr::lag(kwiterambere_ml_data$rf_mm, 2)
kwiterambere_ml_data$rf.lag3d<-dplyr::lag(kwiterambere_ml_data$rf_mm, 3)

kwiterambere_ml_data$tmin.lag1d<-dplyr::lag(kwiterambere_ml_data$tmin, 1)
kwiterambere_ml_data$tmin.lag2d<-dplyr::lag(kwiterambere_ml_data$tmin, 2)
kwiterambere_ml_data$tmin.lag3d<-dplyr::lag(kwiterambere_ml_data$tmin, 3)


kwiterambere_ml_data$tmax.lag1d<-dplyr::lag(kwiterambere_ml_data$tmax, 1)
kwiterambere_ml_data$tmax.lag2d<-dplyr::lag(kwiterambere_ml_data$tmax, 2)
kwiterambere_ml_data$tmax.lag3d<-dplyr::lag(kwiterambere_ml_data$tmax, 3)


kwiterambere_ml_data$sm.lag1d<-dplyr::lag(kwiterambere_ml_data$sm, 1)
kwiterambere_ml_data$sm.lag2d<-dplyr::lag(kwiterambere_ml_data$sm, 2)
kwiterambere_ml_data$sm.lag3d<-dplyr::lag(kwiterambere_ml_data$sm, 3)

kwiterambere_ml_data<-kwiterambere_ml_data[kwiterambere_ml_data$date>"2002-12-31",]

sapply(kwiterambere_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

kwiterambere_ml_pred<-as.h2o(kwiterambere_ml_data[,-1])

# kwiterambere_ml_pred$disc_pred<-predict(best_model,kwiterambere_ml_pred)

# kwiterambere_ml_pred$discharge<-predict(model.tuned,kwiterambere_ml_pred)
kwiterambere_ml_data$disc_pred<-predict(rf.mod6,kwiterambere_ml_data[,-1])

kwi_basin<-st_as_sf(kwiterambere_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
kwi_area<-st_area(kwi_basin)/1000000 
q_ratio_kwiterambere<-rul_area/kwi_area
kwiterambere_ml_data$disc_pred2<-kwiterambere_ml_data$disc_pred*(kwi_area/rul_area)
write.csv(kwiterambere_ml_data,"../discharge/predicted/kwiterambere_discharge_predictedRF.csv",row.names = F)

#4. Nyarusange

nya_imergl<-imergl_all_basins_t[,c("date","Nyarusange")]
colnames(nya_imergl)[2]<-"rf_mm"
nya_tmin<-tmin_all_basins_t[,c("date","Nyarusange")]
colnames(nya_tmin)[2]<-"tmin"
nya_tmax<-tmax_all_basins_t[,c("date","Nyarusange")]
colnames(nya_tmax)[2]<-"tmax"
nya_sm<-all_basins_sm[,c("date","Nyarusange")]
colnames(nya_sm)[2]<-"sm"

nyarusange_ml_data<-list(nya_imergl,nya_tmin,nya_tmax,nya_sm,nya_ndvi,nya_lai) %>% reduce(inner_join,by="date")

nyarusange_ml_data$rf.lag1d<-dplyr::lag(nyarusange_ml_data$rf_mm, 1)
nyarusange_ml_data$rf.lag2d<-dplyr::lag(nyarusange_ml_data$rf_mm, 2)
nyarusange_ml_data$rf.lag3d<-dplyr::lag(nyarusange_ml_data$rf_mm, 3)

nyarusange_ml_data$tmin.lag1d<-dplyr::lag(nyarusange_ml_data$tmin, 1)
nyarusange_ml_data$tmin.lag2d<-dplyr::lag(nyarusange_ml_data$tmin, 2)
nyarusange_ml_data$tmin.lag3d<-dplyr::lag(nyarusange_ml_data$tmin, 3)


nyarusange_ml_data$tmax.lag1d<-dplyr::lag(nyarusange_ml_data$tmax, 1)
nyarusange_ml_data$tmax.lag2d<-dplyr::lag(nyarusange_ml_data$tmax, 2)
nyarusange_ml_data$tmax.lag3d<-dplyr::lag(nyarusange_ml_data$tmax, 3)


nyarusange_ml_data$sm.lag1d<-dplyr::lag(nyarusange_ml_data$sm, 1)
nyarusange_ml_data$sm.lag2d<-dplyr::lag(nyarusange_ml_data$sm, 2)
nyarusange_ml_data$sm.lag3d<-dplyr::lag(nyarusange_ml_data$sm, 3)

nyarusange_ml_data<-nyarusange_ml_data[nyarusange_ml_data$date>"2002-12-31",]

sapply(nyarusange_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

nyarusange_ml_pred<-as.h2o(nyarusange_ml_data[,-1])

# nyarusange_ml_pred$disc_pred<-predict(best_model,nyarusange_ml_pred)

# nyarusange_ml_pred$discharge<-predict(model.tuned,nyarusange_ml_pred)
nyarusange_ml_data$disc_pred<-predict(rf.mod6,nyarusange_ml_data[,-1])

nya_basin<-st_as_sf(nyarusange_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
nya_area<-st_area(nya_basin)/1000000 
q_ratio_nyarusange<-rul_area/nya_area
nyarusange_ml_data$disc_pred2<-nyarusange_ml_data$disc_pred*(nya_area/rul_area)
write.csv(nyarusange_ml_data,"../discharge/predicted/nyarusange_discharge_predictedRF.csv",row.names = F)

#5. Rugeshi
rug_imergl<-imergl_all_basins_t[,c("date","Rugeshi-Gasasa")]
colnames(rug_imergl)[2]<-"rf_mm"
rug_tmin<-tmin_all_basins_t[,c("date","Rugeshi-Gasasa")]
colnames(rug_tmin)[2]<-"tmin"
rug_tmax<-tmax_all_basins_t[,c("date","Rugeshi-Gasasa")]
colnames(rug_tmax)[2]<-"tmax"
rug_sm<-all_basins_sm[,c("date","Rugeshi_Gasasa")]
colnames(rug_sm)[2]<-"sm"

rugeshi_ml_data<-list(rug_imergl,rug_tmin,rug_tmax,rug_sm,rug_ndvi,rug_lai) %>% reduce(inner_join,by="date")

rugeshi_ml_data$rf.lag1d<-dplyr::lag(rugeshi_ml_data$rf_mm, 1)
rugeshi_ml_data$rf.lag2d<-dplyr::lag(rugeshi_ml_data$rf_mm, 2)
rugeshi_ml_data$rf.lag3d<-dplyr::lag(rugeshi_ml_data$rf_mm, 3)

rugeshi_ml_data$tmin.lag1d<-dplyr::lag(rugeshi_ml_data$tmin, 1)
rugeshi_ml_data$tmin.lag2d<-dplyr::lag(rugeshi_ml_data$tmin, 2)
rugeshi_ml_data$tmin.lag3d<-dplyr::lag(rugeshi_ml_data$tmin, 3)


rugeshi_ml_data$tmax.lag1d<-dplyr::lag(rugeshi_ml_data$tmax, 1)
rugeshi_ml_data$tmax.lag2d<-dplyr::lag(rugeshi_ml_data$tmax, 2)
rugeshi_ml_data$tmax.lag3d<-dplyr::lag(rugeshi_ml_data$tmax, 3)


rugeshi_ml_data$sm.lag1d<-dplyr::lag(rugeshi_ml_data$sm, 1)
rugeshi_ml_data$sm.lag2d<-dplyr::lag(rugeshi_ml_data$sm, 2)
rugeshi_ml_data$sm.lag3d<-dplyr::lag(rugeshi_ml_data$sm, 3)

rugeshi_ml_data<-rugeshi_ml_data[rugeshi_ml_data$date>"2002-12-31",]

sapply(rugeshi_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

rugeshi_ml_pred<-as.h2o(rugeshi_ml_data[,-1])

# rugeshi_ml_pred$disc_pred<-predict(best_model,rugeshi_ml_pred)

# rugeshi_ml_pred$discharge<-predict(model.tuned,rugeshi_ml_pred)
rugeshi_ml_data$disc_pred<-predict(rf.mod6,rugeshi_ml_data[,-1])

rug_basin<-st_as_sf(rugeshi_gasasa_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
rug_area<-st_area(rug_basin)/1000000 
q_ratio_rugeshi<-rul_area/rug_area
rugeshi_ml_data$disc_pred2<-rugeshi_ml_data$disc_pred*(rug_area/rul_area)
write.csv(rugeshi_ml_data,"../discharge/predicted/rugeshi_discharge_predictedRF.csv",row.names = F)

#6. Uwumugeti
uwu_imergl<-imergl_all_basins_t[,c("date","Uwumugeti")]
colnames(uwu_imergl)[2]<-"rf_mm"
uwu_tmin<-tmin_all_basins_t[,c("date","Uwumugeti")]
colnames(uwu_tmin)[2]<-"tmin"
uwu_tmax<-tmax_all_basins_t[,c("date","Uwumugeti")]
colnames(uwu_tmax)[2]<-"tmax"
uwu_sm<-all_basins_sm[,c("date","Uwumugeti")]
colnames(uwu_sm)[2]<-"sm"

uwumugeti_ml_data<-list(uwu_imergl,uwu_tmin,uwu_tmax,uwu_sm,uwu_ndvi,uwu_lai) %>% reduce(inner_join,by="date")

uwumugeti_ml_data$rf.lag1d<-dplyr::lag(uwumugeti_ml_data$rf_mm, 1)
uwumugeti_ml_data$rf.lag2d<-dplyr::lag(uwumugeti_ml_data$rf_mm, 2)
uwumugeti_ml_data$rf.lag3d<-dplyr::lag(uwumugeti_ml_data$rf_mm, 3)

uwumugeti_ml_data$tmin.lag1d<-dplyr::lag(uwumugeti_ml_data$tmin, 1)
uwumugeti_ml_data$tmin.lag2d<-dplyr::lag(uwumugeti_ml_data$tmin, 2)
uwumugeti_ml_data$tmin.lag3d<-dplyr::lag(uwumugeti_ml_data$tmin, 3)


uwumugeti_ml_data$tmax.lag1d<-dplyr::lag(uwumugeti_ml_data$tmax, 1)
uwumugeti_ml_data$tmax.lag2d<-dplyr::lag(uwumugeti_ml_data$tmax, 2)
uwumugeti_ml_data$tmax.lag3d<-dplyr::lag(uwumugeti_ml_data$tmax, 3)


uwumugeti_ml_data$sm.lag1d<-dplyr::lag(uwumugeti_ml_data$sm, 1)
uwumugeti_ml_data$sm.lag2d<-dplyr::lag(uwumugeti_ml_data$sm, 2)
uwumugeti_ml_data$sm.lag3d<-dplyr::lag(uwumugeti_ml_data$sm, 3)

uwumugeti_ml_data<-uwumugeti_ml_data[uwumugeti_ml_data$date>"2002-12-31",]

sapply(uwumugeti_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

uwumugeti_ml_pred<-as.h2o(uwumugeti_ml_data[,-1])

# uwumugeti_ml_pred$disc_pred<-predict(best_model,uwumugeti_ml_pred)

# uwumugeti_ml_pred$discharge<-predict(model.tuned,uwumugeti_ml_pred)
uwumugeti_ml_data$disc_pred<-predict(rf.mod6,uwumugeti_ml_data[,-1])

uwu_basin<-st_as_sf(uwumugeti_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 #8520.543 KM^2
uwu_area<-st_area(uwu_basin)/1000000 #492.1259 KM^2
q_ratio_uwumugeti<-rul_area/uwu_area
uwumugeti_ml_data$disc_pred2<-uwumugeti_ml_data$disc_pred*(uwu_area/rul_area)
write.csv(uwumugeti_ml_data,"../discharge/predicted/uwumugeti_discharge_predictedRF.csv",row.names = F)

#7. Muhembe

muh_imergl<-imergl_all_basins_t[,c("date","Muhembe")]
colnames(muh_imergl)[2]<-"rf_mm"
muh_tmin<-tmin_all_basins_t[,c("date","Muhembe")]
colnames(muh_tmin)[2]<-"tmin"
muh_tmax<-tmax_all_basins_t[,c("date","Muhembe")]
colnames(muh_tmax)[2]<-"tmax"
muh_sm<-all_basins_sm[,c("date","Muhembe")]
colnames(muh_sm)[2]<-"sm"

muhembe_ml_data<-list(muh_imergl,muh_tmin,muh_tmax,muh_sm,muh_ndvi,muh_lai) %>% reduce(inner_join,by="date")

muhembe_ml_data$rf.lag1d<-dplyr::lag(muhembe_ml_data$rf_mm, 1)
muhembe_ml_data$rf.lag2d<-dplyr::lag(muhembe_ml_data$rf_mm, 2)
muhembe_ml_data$rf.lag3d<-dplyr::lag(muhembe_ml_data$rf_mm, 3)

muhembe_ml_data$tmin.lag1d<-dplyr::lag(muhembe_ml_data$tmin, 1)
muhembe_ml_data$tmin.lag2d<-dplyr::lag(muhembe_ml_data$tmin, 2)
muhembe_ml_data$tmin.lag3d<-dplyr::lag(muhembe_ml_data$tmin, 3)


muhembe_ml_data$tmax.lag1d<-dplyr::lag(muhembe_ml_data$tmax, 1)
muhembe_ml_data$tmax.lag2d<-dplyr::lag(muhembe_ml_data$tmax, 2)
muhembe_ml_data$tmax.lag3d<-dplyr::lag(muhembe_ml_data$tmax, 3)


muhembe_ml_data$sm.lag1d<-dplyr::lag(muhembe_ml_data$sm, 1)
muhembe_ml_data$sm.lag2d<-dplyr::lag(muhembe_ml_data$sm, 2)
muhembe_ml_data$sm.lag3d<-dplyr::lag(muhembe_ml_data$sm, 3)

muhembe_ml_data<-muhembe_ml_data[muhembe_ml_data$date>"2002-12-31",]

sapply(muhembe_ml_data, function(x) sum(is.na(x)))

#predict discharge with h2o model

#initialize h2o
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")

muhembe_ml_pred<-as.h2o(muhembe_ml_data[,-1])

# muhembe_ml_pred$disc_pred<-predict(best_model,muhembe_ml_pred)

# muhembe_ml_pred$discharge<-predict(model.tuned,muhembe_ml_pred)
muhembe_ml_data$disc_pred<-predict(rf.mod6,muhembe_ml_data[,-1])

muh_basin<-st_as_sf(muhembe_basin)
rul_basin<-st_as_sf(ruliba_catchment)

rul_area<-st_area(rul_basin)/1000000 
muh_area<-st_area(muh_basin)/1000000 
q_ratio_muhembe<-rul_area/muh_area
muhembe_ml_data$disc_pred2<-muhembe_ml_data$disc_pred*(muh_area/rul_area)
write.csv(muhembe_ml_data,"../discharge/predicted/muhembe_discharge_predictedRF.csv",row.names = F)




#####BRIDGE CAMERA ANALYSIS
setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/cameras/raw")
# install.packages(c("data.table","dplyr"))
library(data.table)
library(dplyr)
library(tidyquant)
library(lubridate)

R1data_gasasa_1<-read.csv("./2021/Round 1/csv/Ngororero-Gasasa/100_BTCF_Ngororero_Gasasa_report.csv")
R1data_gasasa_2<-read.csv("./2021/Round 1/csv/Ngororero-Gasasa/101_BTCF_Ngororero_Gasasa_report.csv")
R2data_gasasa_1<-read.csv("./2021/Round 2/csv/Ngororero-Gasasa/103_BTCF_Ngororero_Gasasa_report.csv")
R2data_gasasa_2<-read.csv("./2021/Round 2/csv/Ngororero-Gasasa/104_BTCF_Ngororero_Gasasa_report.csv")
R2data_gasasa_3<-read.csv("./2021/Round 2/csv/Ngororero-Gasasa/105_BTCF_Ngororero_Gasasa_report.csv")

R3data_gasasa<-read.csv("./2022/final_reports/May_2022/Gasasa/report_combined.csv")
R4data_gasasa<-read.csv("./2022/final_reports/May-July_2022/Gasasa/100_BTCF/report.csv")
R5data_gasasa<-read.csv("./2022/final_reports/July-August_2022/Gasasa/100_BTCF/report.csv")

# 
# R2data_gasasa_2<-R2data_gasasa_2[,1:7]
# R2data_gasasa_3<-R2data_gasasa_3[,1:7]

data_gasasa_merge<-rbind(R1data_gasasa_1,R1data_gasasa_2,R2data_gasasa_1,R2data_gasasa_2,R2data_gasasa_3,R3data_gasasa,R4data_gasasa,R5data_gasasa)

data_gasasa_merge<-data_gasasa_merge[,c(3,4,7)]

data_gasasa_merge$date<-as.Date(data_gasasa_merge$date)
data_gasasa_merge<-data_gasasa_merge[data_gasasa_merge$date>"2021-08-28" & data_gasasa_merge$date<"2022-07-30",]

data_gasasa_merge[is.na(strptime(data_gasasa_merge$date, format = "%Y-%m-%d")),]


# data_gasasa_merge[data_gasasa_merge$date=="19/05/2021",]<-"9/5/2021"
# data_gasasa_merge[data_gasasa_merge$date=="19/06/2021",]<-"9/6/2021"
# data_gasasa_merge[data_gasasa_merge$date=="19/13/2021",]<-"9/13/2021"
#can also use replace package

write.csv(data_gasasa_merge, "../processed/data_gasasa_merge.csv", row.names = F)

#NGORORERO-GASASA
#aggregate by day

daily_gasasa_crossings<-data_gasasa_merge
daily_gasasa_crossings<-daily_gasasa_crossings[,c(1,3)]
daily_gasasa_crossings<-aggregate(.~date, data=daily_gasasa_crossings, sum)
ggplot(daily_gasasa_crossings, aes(x=date, y=count))+geom_point()
summary(daily_gasasa_crossings$count)

write.csv(daily_gasasa_crossings, "../processed/daily_gasasa_crossings.csv", row.names = F)

#aggregate by week of the year
formatdate <- function(date)
  paste0("Week ", (as.numeric(format(date, "%d")) - 1) + 1,
         ", ", format(date, "%b %Y"))
weekly.traffic.gasasa2021<-aggregate(data_gasasa_merge$count, by = list(formatdate(data_gasasa_merge$date)), sum)
colnames(weekly.traffic.gasasa2021)[1]<-"Week_in_month"
colnames(weekly.traffic.gasasa2021)[2]<-"Total"

data_gasasa_merge$week<-strftime(data_gasasa_merge$date, format = "%V")
data_gasasa_weekly<-data_gasasa_merge[,c(3,4)]
data_gasasa_weekly<-aggregate(.~week,data = data_gasasa_weekly, sum)

ttt<-data_gasasa_merge %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NGORORERO-RUGESHI

R1data_rugeshi_1<-read.csv("./2021/Round 1/csv/Ngororero-Rugeshi/100_BTCF_Ngororero_Rugeshi_report.csv")
R1data_rugeshi_2<-read.csv("./2021/Round 1/csv/Ngororero-Rugeshi/101_BTCF_Ngororero_Rugeshi_report.csv")
R2data_rugeshi_1<-read.csv("./2021/Round 2/csv/Ngororero-Rugeshi/100_BTCF_Ngororero_Rugeshi_report.csv")

R3data_rugeshi<-read.csv("./2022/final_reports/April_2022/Rugeshi/100_BTCF/report_ocr_time.csv")
R4data_rugeshi<-read.csv("./2022/final_reports/May_2022/Rugeshi/report_combined.csv")
R5data_rugeshi<-read.csv("./2022/final_reports/May-July_2022/Rugeshi/100_BTCF/report.csv")
R6data_rugeshi<-read.csv("./2022/final_reports/July-August_2022/Rugeshi/100_BTCF/report.csv")



data_rugeshi_merge<-rbind(R1data_rugeshi_1,R1data_rugeshi_2,R2data_rugeshi_1,R3data_rugeshi,R4data_rugeshi,R5data_rugeshi,R6data_rugeshi)
data_rugeshi_merge<-data_rugeshi_merge[,c(3,4,7)]
data_rugeshi_merge$date<-as.Date(data_rugeshi_merge$date)
data_rugeshi_merge<-data_rugeshi_merge[data_rugeshi_merge$date>"2021-08-27" & 
                                         data_rugeshi_merge$date<"2022-08-15",]

#aggregate by day
daily_rugeshi_crossings<-data_rugeshi_merge
daily_rugeshi_crossings<-daily_rugeshi_crossings[,c(1,3)]
daily_rugeshi_crossings<-aggregate(.~date, data=daily_rugeshi_crossings, sum)
ggplot(daily_rugeshi_crossings, aes(x=date, y=count))+geom_point()
summary(daily_rugeshi_crossings$count)

write.csv(daily_rugeshi_crossings, "../processed/daily_rugeshi_crossings.csv", row.names = F)

#aggregate by week
# rugeshi_weekly_crossings<-aggregate(count~week(date), data=data_rugeshi_merge, sum)

rugeshi_weekly_crossings<-daily_rugeshi_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)


#NYARUGENGE-NYARUSANGE

R1data_nyarusange_1<-read.csv("./2021/Round 1/csv/Nyarugenge-Nyarusange/100_BTCF_Nyarugenge_Nyarusange_report - Aug15.csv")
R1data_nyarusange_2<-read.csv("./2021/Round 1/csv/Nyarugenge-Nyarusange/101_BTCF_Nyarugenge_Nyarusange_report.csv")
R2data_nyarusange_1<-read.csv("./2021/Round 2/csv/Nyarugenge-Nyarusange/100_BTCF_Nyarugenge_Nyarusange_report.csv")
R3data_nyarusange<-read.csv("./2022/final_reports/April_2022/Nyarusange/report_combined.csv")
R4data_nyarusange<-read.csv("./2022/final_reports/May-July_2022/Nyarusange/report_combined.csv")
R5data_nyarusange<-read.csv("./2022/final_reports/July-August_2022/Nyarusange/100_BTCF/report.csv")



data_nyarusange_merge<-rbind(R1data_nyarusange_1,R1data_nyarusange_2,R2data_nyarusange_1,R3data_nyarusange,R4data_nyarusange,R5data_nyarusange)
data_nyarusange_merge<-data_nyarusange_merge[,c(3,4,7)]
data_nyarusange_merge$date<-as.Date(data_nyarusange_merge$date)
data_nyarusange_merge<-data_nyarusange_merge[data_nyarusange_merge$date>"2021-08-24" & 
                                               data_nyarusange_merge$date<"2022-08-02",]

#aggregate by day
daily_nyarusange_crossings<-data_nyarusange_merge
daily_nyarusange_crossings<-daily_nyarusange_crossings[,c(1,3)]
daily_nyarusange_crossings<-aggregate(.~date, data=daily_nyarusange_crossings, sum)
ggplot(daily_nyarusange_crossings, aes(x=date, y=count))+geom_point()
summary(daily_nyarusange_crossings$count)

write.csv(daily_nyarusange_crossings, "../processed/daily_nyarusange_crossings.csv", row.names = F)

#aggregate by week
# nyarusange_weekly_crossings<-aggregate(count~week(date), data=data_nyarusange_merge, sum)
nyarusange_weekly_crossings<-daily_nyarusange_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)


#GAKENKE-KWITERAMBERE

R2data_kwiterambere_1<-read.csv("./2021/Round 2/csv/Gakenke-kwiterambere/100_BTCF_Gakenke_kwiterambere_report.csv")
R2data_kwiterambere_2<-read.csv("./2021/Round 2/csv/Gakenke-kwiterambere/101_BTCF_Gakenke_kwiterambere_report.csv")
R3data_kwiterambere<-read.csv("./2022/final_reports/May_2022/Kwiterambere/100_BTCF/report.csv")
R4data_kwiterambere<-read.csv("./2022/final_reports/May-July_2022/Kwiterambere/report_combined.csv")



data_kwiterambere_merge<-rbind(R2data_kwiterambere_1,R2data_kwiterambere_2,R3data_kwiterambere,R4data_kwiterambere)
data_kwiterambere_merge<-data_kwiterambere_merge[,c(3,4,7)]
data_kwiterambere_merge$date<-as.Date(data_kwiterambere_merge$date)
data_kwiterambere_merge<-data_kwiterambere_merge[data_kwiterambere_merge$date>"2021-11-01" & 
                                                   data_kwiterambere_merge$date<"2022-07-13",]

#aggregate by day
daily_kwiterambere_crossings<-data_kwiterambere_merge
daily_kwiterambere_crossings<-daily_kwiterambere_crossings[,c(1,3)]
daily_kwiterambere_crossings<-aggregate(.~date, data=daily_kwiterambere_crossings, sum)
ggplot(daily_kwiterambere_crossings, aes(x=date, y=count))+geom_point()
summary(daily_kwiterambere_crossings$count)

write.csv(daily_kwiterambere_crossings, "../processed/daily_kwiterambere_crossings.csv", row.names = F)

#aggregate by week
# kwiterambere_weekly_crossings<-aggregate(count~week(date), data=data_kwiterambere_merge, sum)

kwiterambere_weekly_crossings<-daily_kwiterambere_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#GAKENKE-NTARUKA

R2data_ntaruka_1<-read.csv("./2021/Round 2/csv/Gakenke-Ntaruka/100_BTCF_Gakenke_Ntaruka_report.csv")
R2data_ntaruka_2<-read.csv("./2021/Round 2/csv/Gakenke-Ntaruka/101_BTCF_Gakenke_Ntaruka_report.csv")
R3data_ntaruka<-read.csv("./2022/final_reports/April_2022/Ntaruka/100_BTCF/report_ocr_time.csv")
R4data_ntaruka<-read.csv("./2022/final_reports/May_2022/Ntaruka/100_BTCF/report.csv")
R5data_ntaruka<-read.csv("./2022/final_reports/May-July_2022/Ntaruka/report_combined.csv")
R6data_ntaruka<-read.csv("./2022/final_reports/July-August_2022/Ntaruka/100_BTCF/report.csv")



data_ntaruka_merge<-rbind(R2data_ntaruka_1,R2data_ntaruka_2,R3data_ntaruka,R4data_ntaruka,R5data_ntaruka,R6data_ntaruka)
data_ntaruka_merge<-data_ntaruka_merge[,c(3,4,7)]
data_ntaruka_merge$date<-as.Date(data_ntaruka_merge$date)
data_ntaruka_merge<-data_ntaruka_merge[data_ntaruka_merge$date>"2021-11-02" & 
                                         data_ntaruka_merge$date<"2022-08-29",]

#aggregate by day
daily_ntaruka_crossings<-data_ntaruka_merge
daily_ntaruka_crossings<-daily_ntaruka_crossings[,c(1,3)]
daily_ntaruka_crossings<-aggregate(.~date, data=daily_ntaruka_crossings, sum)
ggplot(daily_ntaruka_crossings, aes(x=date, y=count))+geom_point()
summary(daily_ntaruka_crossings$count)

write.csv(daily_ntaruka_crossings, "../processed/daily_ntaruka_crossings.csv", row.names = F)

#aggregate by week
# ntaruka_weekly_crossings<-aggregate(count~week(date), data=data_ntaruka_merge, sum)


ntaruka_weekly_crossings<-daily_ntaruka_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#KARONGI-GASHARU

R2data_gasharu_1<-read.csv("./2021/Round 2/csv/Karongi-Gasharu/100_BTCF_Karongi_Gasharu_report.csv")
R2data_gasharu_2<-read.csv("./2021/Round 2/csv/Karongi-Gasharu/101_BTCF_Karongi_Gasharu_report.csv")
R3data_gasharu<-read.csv("./2022/final_reports/May-July_2022/Gasharu/report_combined.csv")

data_gasharu_merge<-rbind(R2data_gasharu_1,R2data_gasharu_2,R3data_gasharu)
data_gasharu_merge<-data_gasharu_merge[,c(3,4,7)]
data_gasharu_merge$date<-as.Date(data_gasharu_merge$date, origin="1970-01-01")
data_gasharu_merge<-data_gasharu_merge[data_gasharu_merge$date>"2021-11-30" & 
                                         data_gasharu_merge$date<"2022-07-08",]

#aggregate by day
daily_gasharu_crossings<-data_gasharu_merge
daily_gasharu_crossings<-daily_gasharu_crossings[,c(1,3)]
daily_gasharu_crossings<-aggregate(.~date, data=daily_gasharu_crossings, sum)
ggplot(daily_gasharu_crossings, aes(x=date, y=count))+geom_point()
summary(daily_gasharu_crossings$count)

write.csv(daily_gasharu_crossings,"../processed/daily_gasharu_crossings.csv",row.names = F)

#aggregate by week
# gasharu_weekly_crossings<-aggregate(count~week(date), data=data_gasharu_merge, sum)

gasharu_weekly_crossings<-daily_gasharu_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NGORORERO-MUHEMBE

R2data_muhembe_1<-read.csv("./2021/Round 2/csv/Ngororero-Muhembe/100_BTCF_Ngororero_Muhembe_report.csv")
R3data_muhembe<-read.csv("./2022/final_reports/April_2022/Muhembe/100_BTCF/report_ocr_time.csv")
R4data_muhembe<-read.csv("./2022/final_reports/May_2022/Muhembe/100_BTCF/report.csv")
R5data_muhembe<-read.csv("./2022/final_reports/May-July_2022/Muhembe/100_BTCF/report.csv")
R6data_muhembe<-read.csv("./2022/final_reports/July-August_2022/Muhembe/100_BTCF/report.csv")


data_muhembe_merge<-rbind(R2data_muhembe_1,R3data_muhembe,R4data_muhembe,R5data_muhembe,R6data_muhembe)
data_muhembe_merge<-data_muhembe_merge[,c(3,4,7)]
data_muhembe_merge$date<-as.Date(data_muhembe_merge$date, origin="1970-01-01")
data_muhembe_merge<-data_muhembe_merge[data_muhembe_merge$date>"2021-11-05" & 
                                         data_muhembe_merge$date<"2022-08-21",]

#aggregate by day
daily_muhembe_crossings<-data_muhembe_merge
daily_muhembe_crossings<-daily_muhembe_crossings[,c(1,3)]
daily_muhembe_crossings<-aggregate(.~date, data=daily_muhembe_crossings, sum)
ggplot(daily_muhembe_crossings, aes(x=date, y=count))+geom_point()
summary(daily_muhembe_crossings$count)

write.csv(daily_muhembe_crossings,"../processed/daily_muhembe_crossings.csv",row.names = F)

#aggregate by week
muhembe_weekly_crossings<-aggregate(count~week(date), data=data_muhembe_merge, sum)

muhembe_weekly_crossings<-daily_muhembe_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NYAMAGABE-MUTIWINGOMA

R2data_mutiwingoma_1<-read.csv("./2021/Round 2/csv/Nyamagabe-Mutiwingoma/100_BTCF_Nyamagabe_Mutiwingoma_report.csv")
R3data_mutiwingoma<-read.csv("./2022/final_reports/April_2022/Mutiwingoma/100_BTCF/report_ocr_time.csv")
R4data_mutiwingoma<-read.csv("./2022/final_reports/May_2022/Mutiwingoma/100_BTCF/report_ocr_time.csv")
R5data_mutiwingoma<-read.csv("./2022/final_reports/May-July_2022/Mutiwingoma/100_BTCF/report.csv")
R6data_mutiwingoma<-read.csv("./2022/final_reports/July-August_2022/Mutiwingoma/100_BTCF/report.csv")


data_muhembe_merge<-rbind(R2data_muhembe_1,R3data_mutiwingoma,R4data_mutiwingoma,R5data_mutiwingoma,R6data_mutiwingoma)
data_mutiwingoma_merge<-data_muhembe_merge[,c(3,4,7)]
data_mutiwingoma_merge$date<-as.Date(data_mutiwingoma_merge$date)
data_mutiwingoma_merge<-data_mutiwingoma_merge[data_mutiwingoma_merge$date>"2021-11-08" & 
                                                 data_mutiwingoma_merge$date<"2022-08-16",]

#aggregate by day
daily_mutiwingoma_crossings<-data_mutiwingoma_merge
daily_mutiwingoma_crossings<-daily_mutiwingoma_crossings[,c(1,3)]
daily_mutiwingoma_crossings<-aggregate(.~date, data=daily_mutiwingoma_crossings, sum)
ggplot(daily_mutiwingoma_crossings, aes(x=date, y=count))+geom_point()
summary(daily_mutiwingoma_crossings$count)

write.csv(daily_mutiwingoma_crossings,"../processed/daily_mutiwingoma_crossings.csv", row.names = F)

#aggregate by week
mutiwingoma_weekly_crossings<-aggregate(count~week(date), data=data_mutiwingoma_merge, sum)

mutiwingoma_weekly_crossings<-daily_mutiwingoma_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NYAMAGABE-UWUMUGETI-KIGUSA

R2data_kigusa_1<-read.csv("./2021/Round 2/csv/Nyamagabe-Uwumugeti-Kigusa/100_BTCF_Nyamagabe_Uwumugeti-Kigusa_report.csv")
R3data_kigusa<-read.csv("./2022/final_reports/April_2022/Uwumugeti-Kigusa/100_BTCF/report_ocr_time.csv")
R4data_kigusa<-read.csv("./2022/final_reports/May_2022/Uwumugeti-Kigusa/100_BTCF/report.csv")
R5data_kigusa<-read.csv("./2022/final_reports/May-July_2022/Uwumugeti/100_BTCF/report.csv")
R6data_kigusa<-read.csv("./2022/final_reports/July-AUgust_2022/Uwumugeti/100_BTCF/report.csv")


data_kigusa_merge<-rbind(R2data_kigusa_1,R3data_kigusa,R4data_kigusa,R5data_kigusa,R6data_kigusa)
data_kigusa_merge<-data_kigusa_merge[,c(3,4,7)]
data_kigusa_merge$date<-as.Date(data_kigusa_merge$date)
data_kigusa_merge<-data_kigusa_merge[data_kigusa_merge$date>"2021-11-07" & 
                                       data_kigusa_merge$date<"2022-08-23",]

#aggregate by day
daily_kigusa_crossings<-data_kigusa_merge
daily_kigusa_crossings<-daily_kigusa_crossings[,c(1,3)]
daily_kigusa_crossings<-aggregate(.~date, data=daily_kigusa_crossings, sum)
ggplot(daily_kigusa_crossings, aes(x=date, y=count))+geom_point()
summary(daily_kigusa_crossings$count)

#aggregate by week
kigusa_weekly_crossings<-aggregate(count~week(date), data=data_kigusa_merge, sum)

kigusa_weekly_crossings<-daily_kigusa_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NYAMASHEKE-KIGANDI

R2data_kigandi_1<-read.csv("./2021/Round 2/csv/Nyamasheke-Kigandi/100_BTCF_Nyamasheke_Kigandi_report.csv")
R2data_kigandi_2<-read.csv("./2021/Round 2/csv/Nyamasheke-Kigandi/101_BTCF_Nyamasheke_Kigandi_report.csv")

R3data_kigandi<-read.csv("./2022/final_reports/April_2022/Kigandi/100_BTCF/report_ocr_time.csv")
R4data_kigandi<-read.csv("./2022/final_reports/May_2022/Kigandi/report_combined.csv")
R5data_kigandi<-read.csv("./2022/final_reports/May-July_2022/Kigandi/100_BTCF/report.csv")


data_kigandi_merge<-rbind(R2data_kigandi_1,R2data_kigandi_2,R3data_kigandi,R4data_kigandi,R5data_kigandi)
data_kigandi_merge<-data_kigandi_merge[,c(3,4,7)]
data_kigandi_merge$date<-as.Date(data_kigandi_merge$date)
data_kigandi_merge<-data_kigandi_merge[data_kigandi_merge$date>"2021-11-05" & 
                                         data_kigandi_merge$date<"2022-06-10",]

#aggregate by day
daily_kigandi_crossings<-data_kigandi_merge
daily_kigandi_crossings<-daily_kigandi_crossings[,c(1,3)]
daily_kigandi_crossings<-aggregate(.~date, data=daily_kigandi_crossings, sum)
ggplot(daily_kigandi_crossings, aes(x=date, y=count))+geom_point()
summary(daily_kigandi_crossings$count)

#aggregate by week
kigandi_weekly_crossings<-aggregate(count~week(date), data=data_kigandi_merge, sum)

daily_kigandi_crossings$date<-as.Date(daily_kigandi_crossings$date)

kigandi_weekly_crossings<-daily_kigandi_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)

#NYAMASHEKE-NYAGISASA

R2data_nyagisasa_1<-read.csv("./2021/Round 2/csv/Nyamasheke-Nyagisasa/100_BTCF_Nyamasheke_Nyagisasa_report.csv")
R2data_nyagisasa_2<-read.csv("./2021/Round 2/csv/Nyamasheke-Nyagisasa/101_BTCF_Nyamasheke_Nyagisasa_report.csv")
R3data_nyagisasa<-read.csv("./2022/final_reports/April_2022/Nyagisasa/100_BTCF/report_ocr_time.csv")
R4data_nyagisasa<-read.csv("./2022/final_reports/May_2022/Nyagisasa/report_combined.csv")
R5data_nyagisasa<-read.csv("./2022/final_reports/May-July_2022/Nyagisasa/100_BTCF/report.csv")
R6data_nyagisasa<-read.csv("./2022/final_reports/July-August_2022/Nyagisasa/100_BTCF/report.csv")


data_nyagisasa_merge<-rbind(R2data_nyagisasa_1,R2data_nyagisasa_2,R3data_nyagisasa,R4data_nyagisasa,R5data_nyagisasa,R6data_nyagisasa)
data_nyagisasa_merge<-data_nyagisasa_merge[,c(3,4,7)]
data_nyagisasa_merge$date<-as.Date(data_nyagisasa_merge$date)
data_nyagisasa_merge<-data_nyagisasa_merge[data_nyagisasa_merge$date>"2021-11-04" & 
                                             data_nyagisasa_merge$date<"2022-07-28",]

#aggregate by day
daily_nyagisasa_crossings<-data_nyagisasa_merge
daily_nyagisasa_crossings<-daily_nyagisasa_crossings[,c(1,3)]
daily_nyagisasa_crossings<-aggregate(.~date, data=daily_nyagisasa_crossings, sum)
ggplot(daily_nyagisasa_crossings, aes(x=date, y=count))+geom_point()
summary(daily_nyagisasa_crossings$count)

#aggregate by week
nyagisasa_weekly_crossings<-aggregate(count~week(date), data=data_nyagisasa_merge, sum)

nyagisasa_weekly_crossings<-daily_nyagisasa_crossings %>%
  tq_transmute(select     = count,
               mutate_fun = apply.weekly,
               FUN        = sum)


#####PLOTS#####
par(mfrow=c(6,2))
#Round 1&2
daily_gasasa_crossings
daily_rugeshi_crossings
daily_nyarusange_crossings
#Round 2
daily_kuiterambere_crossings
daily_ntaruka_crossings
daily_gasharu_crossings
daily_muhembe_crossings
daily_mutiwingoma_crossings
daily_kigusa_crossings
daily_kigandi_crossings
daily_nyagisasa_crossings

par(mfrow=c(6,2))
# install.packages("patchwork")
library(patchwork)

p1=ggplot(daily_gasasa_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("Crossings per day")+ggtitle("Gasasa")
p2=ggplot(daily_rugeshi_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Rugeshi")
p3=ggplot(daily_nyarusange_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Nyarusange")
p4=ggplot(daily_kwiterambere_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Kwiterambere")
p5=ggplot(daily_ntaruka_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("Crossings per day")+ggtitle("Ntaruka")
p6=ggplot(daily_gasharu_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Gasharu")
p7=ggplot(daily_muhembe_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Muhembe")
p8=ggplot(daily_mutiwingoma_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Mutiwingoma")
p9=ggplot(daily_kigusa_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("Crossings per day")+ggtitle("Kigusa")
p10=ggplot(daily_kigandi_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Kigandi")
p11=ggplot(daily_nyagisasa_crossings, aes(x=date, y=count))+geom_point()+xlab("")+ylab("")+ggtitle("Nyagisasa")

#arrange plots by date of first record
p1+p2+p3+p4+p5+p11+p7+p8+p9+p10+p6


#correlations between bridge use and hydrometeorological data
dir.create("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/cameras/analysis", showWarnings = TRUE, recursive = FALSE, mode = "0777")

# nyarusange_rainfall<-raster::extract(imergl.stk,nyarusange_basin,fun=mean,na.rm=T)
all_basins_rainfall<-raster::extract(imergl.stk,all_basins,fun=mean,na.rm=T)
all_basins_rainfall_t<-t(all_basins_rainfall)
all_basins_rainfall_t<-as.data.frame(all_basins_rainfall_t)
rownames(all_basins_rainfall_t)<-dates.imergl
all_basins_rainfall_t<-rownames_to_column(all_basins_rainfall_t,var="date")
colnames(all_basins_rainfall_t)[2:8]<-all_basins@data$name
write.csv(all_basins_rainfall_t,"C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/cameras/analysis/all_basins_rainfall_t.csv",row.names = F)

ntaruka_rainfall<-all_basins_rainfall_t[,c("date","Ntaruka")]
colnames(ntaruka_rainfall)[2]<-"rf"
ntaruka_rainfall$rflag1d<-dplyr::lag(ntaruka_rainfall$rf,1)
ntaruka_rainfall$rflag2d<-dplyr::lag(ntaruka_rainfall$rf,2)
ntaruka_rainfall$rflag3d<-dplyr::lag(ntaruka_rainfall$rf,3)
ntaruka_rainfall$rflag4d<-dplyr::lag(ntaruka_rainfall$rf,4)
ntaruka_rainfall$rflag5d<-dplyr::lag(ntaruka_rainfall$rf,5)

ntaruka_rainfall$rf.anom<-((ntaruka_rainfall$rf-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100
ntaruka_rainfall$rf.anomlg1<-((ntaruka_rainfall$rflag1d-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100
ntaruka_rainfall$rf.anomlg2<-((ntaruka_rainfall$rflag2d-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100
ntaruka_rainfall$rf.anomlg3<-((ntaruka_rainfall$rflag3d-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100
ntaruka_rainfall$rf.anomlg4<-((ntaruka_rainfall$rflag4d-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100
ntaruka_rainfall$rf.anomlg5<-((ntaruka_rainfall$rflag5d-mean(ntaruka_rainfall$rf))/mean(ntaruka_rainfall$rf))*100

#merge bridge crossings and rainfall
ntaruka_rainfall$date<-as.Date(ntaruka_rainfall$date)
daily_ntaruka_crossings$date<-as.Date(daily_ntaruka_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

ntaruka_rainbridge_merge<-merge(ntaruka_rainfall,daily_ntaruka_crossings,by="date")
cor(ntaruka_rainbridge_merge$rflag4d,ntaruka_rainbridge_merge$count)
cor(ntaruka_rainbridge_merge$rf.anom,ntaruka_rainbridge_merge$count)

ntaruka_sm_merge<-merge(ruliba_ml_data,daily_ntaruka_crossings,by="date")
cor(ntaruka_sm_merge$discharge,ntaruka_sm_merge$count)

ntaruka_rainbridge<-ntaruka_rainbridge_merge[,c(1:7,14)]
ntaruka_rainbridge$mon<-month(ntaruka_rainbridge$date)

ntaruka_rainbridge_agg<-aggregate(.~mon,data=ntaruka_rainbridge,sum)
ntaruka_rainbridge_agg$Month<-month.abb[ntaruka_rainbridge_agg$mon]
cor(ntaruka_rainbridge_agg$rf,ntaruka_rainbridge_agg$count) #cor: 0.34

length(which(ntaruka_rainbridge$mon==11)) #28 days
length(which(ntaruka_rainbridge$mon==12)) #31
length(which(ntaruka_rainbridge$mon==1)) #11
length(which(ntaruka_rainbridge$mon==2))#5
length(which(ntaruka_rainbridge$mon==3))#31
length(which(ntaruka_rainbridge$mon==4))#30
length(which(ntaruka_rainbridge$mon==5))#31
length(which(ntaruka_rainbridge$mon==6))#30
length(which(ntaruka_rainbridge$mon==7))#31
length(which(ntaruka_rainbridge$mon==8))#2
length(which(ntaruka_rainbridge$mon==9))#0
length(which(ntaruka_rainbridge$mon==10))#0

#exclude months with inadequate days
ntaruka_rainbridge_agg2<-subset(ntaruka_rainbridge_agg,Month %in% c("Nov","Dec","Mar","Apr","May","Jun","Jul"))

cor(ntaruka_rainbridge_agg2$rf,ntaruka_rainbridge_agg2$count) #cor: 0.47 p=0.2853

# plot2(ntaruka_rainbridge_merge$rf,ntaruka_rainbridge_merge$count,plot.type = "multiple",xlab = "day",ylab = c("Rainfall","No. of People"),main = "Daily Rainfall vs Bridge Crossings")
plot2(ntaruka_rainbridge_sel_agg$rf,ntaruka_rainbridge_sel_agg$count,plot.type = "multiple",xlab = "Mon",ylab = c("Rainfall","No. of People"),main = "Monthly Rainfall vs Bridge Crossings")

# ntaruka_disc<-ntaruka_sm_merge[,c("date","discharge")]
# ntaruka_disc$mon<-month(ntaruka_disc$date)
# ntaruka_disc_agg<-aggregate(.~mon,data=ntaruka_disc,mean)
# 
# ntaruka_merge2<-merge(ntaruka_disc_agg,ntaruka_rainbridge_agg,by="mon")
# 
# cor(ntaruka_merge2$rf,ntaruka_merge2$count)
# cor(ntaruka_merge2$discharge,ntaruka_merge2$count)


#muhembe

muhembe_rainfall<-all_basins_rainfall_t[,c("date","Muhembe")]
colnames(muhembe_rainfall)[2]<-"rf"
muhembe_rainfall$rflag1d<-dplyr::lag(muhembe_rainfall$rf,1)
muhembe_rainfall$rflag2d<-dplyr::lag(muhembe_rainfall$rf,2)
muhembe_rainfall$rflag3d<-dplyr::lag(muhembe_rainfall$rf,3)
muhembe_rainfall$rflag4d<-dplyr::lag(muhembe_rainfall$rf,4)
muhembe_rainfall$rflag5d<-dplyr::lag(muhembe_rainfall$rf,5)

muhembe_rainfall$rf.anom<-((muhembe_rainfall$rf-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100
muhembe_rainfall$rf.anomlg1<-((muhembe_rainfall$rflag1d-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100
muhembe_rainfall$rf.anomlg2<-((muhembe_rainfall$rflag2d-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100
muhembe_rainfall$rf.anomlg3<-((muhembe_rainfall$rflag3d-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100
muhembe_rainfall$rf.anomlg4<-((muhembe_rainfall$rflag4d-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100
muhembe_rainfall$rf.anomlg5<-((muhembe_rainfall$rflag5d-mean(muhembe_rainfall$rf))/mean(muhembe_rainfall$rf))*100

#merge bridge crossings and rainfall
muhembe_rainfall$date<-as.Date(muhembe_rainfall$date)
daily_muhembe_crossings$date<-as.Date(daily_muhembe_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

muhembe_rainbridge_merge<-merge(muhembe_rainfall,daily_muhembe_crossings,by="date")
cor(muhembe_rainbridge_merge$rf,muhembe_rainbridge_merge$count)
cor(muhembe_rainbridge_merge$rflag3d,muhembe_rainbridge_merge$count)

muhembe_sm_merge<-merge(ruliba_ml_data,daily_muhembe_crossings,by="date")
cor(muhembe_sm_merge$discharge,muhembe_sm_merge$count)

muhembe_rainbridge<-muhembe_rainbridge_merge[,c(1:7,14)]
muhembe_rainbridge$mon<-month(muhembe_rainbridge$date)
muhembe_rainbridge_agg<-aggregate(.~mon,data=muhembe_rainbridge,sum)
muhembe_rainbridge_agg$Month<-month.abb[muhembe_rainbridge_agg$mon]
cor(muhembe_rainbridge_agg$rf,muhembe_rainbridge_agg$count) #cor: 0.17

length(which(muhembe_rainbridge$mon==11)) #25 days
length(which(muhembe_rainbridge$mon==12)) #31
length(which(muhembe_rainbridge$mon==1)) #13
length(which(muhembe_rainbridge$mon==2))#1
length(which(muhembe_rainbridge$mon==3))#17
length(which(muhembe_rainbridge$mon==4))#22
length(which(muhembe_rainbridge$mon==5))#31
length(which(muhembe_rainbridge$mon==6))#28
length(which(muhembe_rainbridge$mon==7))#12
length(which(muhembe_rainbridge$mon==8))#18
length(which(muhembe_rainbridge$mon==9))#0
length(which(muhembe_rainbridge$mon==10))#0

muhembe_rainbridge_agg2<-subset(muhembe_rainbridge_agg,Month %in% c("Nov","Dec","Apr","May","Jun"))

cor(muhembe_rainbridge_agg2$rf,muhembe_rainbridge_agg2$count) # cor 0.25 p =0.6886


#nyarusange

nyarusange_rainfall<-all_basins_rainfall_t[,c("date","Nyarusange")]
colnames(nyarusange_rainfall)[2]<-"rf"
nyarusange_rainfall$rflag1d<-dplyr::lag(nyarusange_rainfall$rf,1)
nyarusange_rainfall$rflag2d<-dplyr::lag(nyarusange_rainfall$rf,2)
nyarusange_rainfall$rflag3d<-dplyr::lag(nyarusange_rainfall$rf,3)
nyarusange_rainfall$rflag4d<-dplyr::lag(nyarusange_rainfall$rf,4)
nyarusange_rainfall$rflag5d<-dplyr::lag(nyarusange_rainfall$rf,5)

nyarusange_rainfall$rf.anom<-((nyarusange_rainfall$rf-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100
nyarusange_rainfall$rf.anomlg1<-((nyarusange_rainfall$rflag1d-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100
nyarusange_rainfall$rf.anomlg2<-((nyarusange_rainfall$rflag2d-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100
nyarusange_rainfall$rf.anomlg3<-((nyarusange_rainfall$rflag3d-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100
nyarusange_rainfall$rf.anomlg4<-((nyarusange_rainfall$rflag4d-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100
nyarusange_rainfall$rf.anomlg5<-((nyarusange_rainfall$rflag5d-mean(nyarusange_rainfall$rf))/mean(nyarusange_rainfall$rf))*100

#merge bridge crossings and rainfall
nyarusange_rainfall$date<-as.Date(nyarusange_rainfall$date)
daily_nyarusange_crossings$date<-as.Date(daily_nyarusange_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

nyarusange_rainbridge_merge<-merge(nyarusange_rainfall,daily_nyarusange_crossings,by="date")
cor(nyarusange_rainbridge_merge$rf,nyarusange_rainbridge_merge$count)
cor(nyarusange_rainbridge_merge$rflag3d,nyarusange_rainbridge_merge$count)

# nyarusange_sm_merge<-merge(ruliba_ml_data,daily_nyarusange_crossings,by="date")
# cor(nyarusange_sm_merge$discharge,nyarusange_sm_merge$count)
# cor(nyarusange_sm_merge$sm,nyarusange_sm_merge$count)

nyarusange_rainbridge<-nyarusange_rainbridge_merge[,c(1:7,14)]
nyarusange_rainbridge$mon<-month(nyarusange_rainbridge$date)

nyarusange_rainbridge_agg<-aggregate(.~mon,data=nyarusange_rainbridge,sum)
nyarusange_rainbridge_agg$Month<-month.abb[nyarusange_rainbridge_agg$mon]

cor(nyarusange_rainbridge_agg$rf,nyarusange_rainbridge_agg$count) #cor: 0.37

length(which(nyarusange_rainbridge$mon==11)) #20 days
length(which(nyarusange_rainbridge$mon==12)) #3
length(which(nyarusange_rainbridge$mon==1)) #0
length(which(nyarusange_rainbridge$mon==2))#0
length(which(nyarusange_rainbridge$mon==3))#7
length(which(nyarusange_rainbridge$mon==4))#0
length(which(nyarusange_rainbridge$mon==5))#8
length(which(nyarusange_rainbridge$mon==6))#30
length(which(nyarusange_rainbridge$mon==7))#31
length(which(nyarusange_rainbridge$mon==8))#8
length(which(nyarusange_rainbridge$mon==9))#29
length(which(nyarusange_rainbridge$mon==10))#0

nyarusange_rainbridge_agg2<-subset(nyarusange_rainbridge_agg,Month %in% c("Nov","Jun","Jul","Sep"))

cor(nyarusange_rainbridge_agg2$rf,nyarusange_rainbridge_agg2$count) #0.75 p=0.246

#Mutiwingoma

mutiwingoma_rainfall<-all_basins_rainfall_t[,c("date","Mutiwingoma")]
colnames(mutiwingoma_rainfall)[2]<-"rf"
mutiwingoma_rainfall$rflag1d<-dplyr::lag(mutiwingoma_rainfall$rf,1)
mutiwingoma_rainfall$rflag2d<-dplyr::lag(mutiwingoma_rainfall$rf,2)
mutiwingoma_rainfall$rflag3d<-dplyr::lag(mutiwingoma_rainfall$rf,3)
mutiwingoma_rainfall$rflag4d<-dplyr::lag(mutiwingoma_rainfall$rf,4)
mutiwingoma_rainfall$rflag5d<-dplyr::lag(mutiwingoma_rainfall$rf,5)

mutiwingoma_rainfall$rf.anom<-((mutiwingoma_rainfall$rf-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100
mutiwingoma_rainfall$rf.anomlg1<-((mutiwingoma_rainfall$rflag1d-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100
mutiwingoma_rainfall$rf.anomlg2<-((mutiwingoma_rainfall$rflag2d-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100
mutiwingoma_rainfall$rf.anomlg3<-((mutiwingoma_rainfall$rflag3d-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100
mutiwingoma_rainfall$rf.anomlg4<-((mutiwingoma_rainfall$rflag4d-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100
mutiwingoma_rainfall$rf.anomlg5<-((mutiwingoma_rainfall$rflag5d-mean(mutiwingoma_rainfall$rf))/mean(mutiwingoma_rainfall$rf))*100

#merge bridge crossings and rainfall
mutiwingoma_rainfall$date<-as.Date(mutiwingoma_rainfall$date)
daily_mutiwingoma_crossings$date<-as.Date(daily_mutiwingoma_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

mutiwingoma_rainbridge_merge<-merge(mutiwingoma_rainfall,daily_mutiwingoma_crossings,by="date")
cor(mutiwingoma_rainbridge_merge$rf,mutiwingoma_rainbridge_merge$count)
cor(mutiwingoma_rainbridge_merge$rflag3d,mutiwingoma_rainbridge_merge$count)

# mutiwingoma_sm_merge<-merge(ruliba_ml_data,daily_mutiwingoma_crossings,by="date")
# cor(mutiwingoma_sm_merge$discharge,mutiwingoma_sm_merge$count)
# cor(mutiwingoma_sm_merge$sm,mutiwingoma_sm_merge$count)

mutiwingoma_rainbridge<-mutiwingoma_rainbridge_merge[,c(1:7,14)]
mutiwingoma_rainbridge$mon<-month(mutiwingoma_rainbridge$date)
mutiwingoma_rainbridge_agg<-aggregate(.~mon,data=mutiwingoma_rainbridge,sum)
cor(mutiwingoma_rainbridge_agg$rf,mutiwingoma_rainbridge_agg$count) #cor: 0.6

length(which(mutiwingoma_rainbridge$mon==11)) #22 days
length(which(mutiwingoma_rainbridge$mon==12)) #31
length(which(mutiwingoma_rainbridge$mon==1)) #13
length(which(mutiwingoma_rainbridge$mon==2))#2
length(which(mutiwingoma_rainbridge$mon==3))#31
length(which(mutiwingoma_rainbridge$mon==4))#30
length(which(mutiwingoma_rainbridge$mon==5))#22
length(which(mutiwingoma_rainbridge$mon==6))#10
length(which(mutiwingoma_rainbridge$mon==7))#17
length(which(mutiwingoma_rainbridge$mon==8))#15
length(which(mutiwingoma_rainbridge$mon==9))#0
length(which(mutiwingoma_rainbridge$mon==10))#0


mutiwingoma_rainbridge_agg$Month<-month.abb[mutiwingoma_rainbridge_agg$mon]
mutiwingoma_rainbridge_agg<-subset(mutiwingoma_rainbridge_agg,Month %in% c("Nov","Dec","Mar","Apr","May","Jul","Aug"))
mutiwingoma_rainbridge_agg2<-subset(mutiwingoma_rainbridge_agg,Month %in% c("Nov","Dec","Mar","Apr","May"))


# cor(mutiwingoma_rainbridge_agg$rf,mutiwingoma_rainbridge_agg$count) #cor: 0.35
cor(mutiwingoma_rainbridge_agg2$rf,mutiwingoma_rainbridge_agg2$count) #cor: 0.76 p=0.1378

barplot(mutiwingoma_rainbridge_agg$count,names.arg=mutiwingoma_rainbridge_agg$Month)


#Kwiterambere

kwiterambere_rainfall<-all_basins_rainfall_t[,c("date","Kwiterambere")]
colnames(kwiterambere_rainfall)[2]<-"rf"
kwiterambere_rainfall$rflag1d<-dplyr::lag(kwiterambere_rainfall$rf,1)
kwiterambere_rainfall$rflag2d<-dplyr::lag(kwiterambere_rainfall$rf,2)
kwiterambere_rainfall$rflag3d<-dplyr::lag(kwiterambere_rainfall$rf,3)
kwiterambere_rainfall$rflag4d<-dplyr::lag(kwiterambere_rainfall$rf,4)
kwiterambere_rainfall$rflag5d<-dplyr::lag(kwiterambere_rainfall$rf,5)

kwiterambere_rainfall$rf.anom<-((kwiterambere_rainfall$rf-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100
kwiterambere_rainfall$rf.anomlg1<-((kwiterambere_rainfall$rflag1d-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100
kwiterambere_rainfall$rf.anomlg2<-((kwiterambere_rainfall$rflag2d-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100
kwiterambere_rainfall$rf.anomlg3<-((kwiterambere_rainfall$rflag3d-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100
kwiterambere_rainfall$rf.anomlg4<-((kwiterambere_rainfall$rflag4d-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100
kwiterambere_rainfall$rf.anomlg5<-((kwiterambere_rainfall$rflag5d-mean(kwiterambere_rainfall$rf))/mean(kwiterambere_rainfall$rf))*100

#merge bridge crossings and rainfall
kwiterambere_rainfall$date<-as.Date(kwiterambere_rainfall$date)
daily_kwiterambere_crossings$date<-as.Date(daily_kwiterambere_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

kwiterambere_rainbridge_merge<-merge(kwiterambere_rainfall,daily_kwiterambere_crossings,by="date")
cor(kwiterambere_rainbridge_merge$rf,kwiterambere_rainbridge_merge$count)
cor(kwiterambere_rainbridge_merge$rflag3d,kwiterambere_rainbridge_merge$count)

# kwiterambere_sm_merge<-merge(ruliba_ml_data,daily_kwiterambere_crossings,by="date")
# cor(kwiterambere_sm_merge$discharge,kwiterambere_sm_merge$count)
# cor(kwiterambere_sm_merge$sm,kwiterambere_sm_merge$count)

kwiterambere_rainbridge<-kwiterambere_rainbridge_merge[,c(1:7,14)]
kwiterambere_rainbridge$mon<-month(kwiterambere_rainbridge$date)
kwiterambere_rainbridge_agg<-aggregate(.~mon,data=kwiterambere_rainbridge,sum)
kwiterambere_rainbridge_agg$Month<-month.abb[kwiterambere_rainbridge_agg$mon]
cor(kwiterambere_rainbridge_agg$rf,kwiterambere_rainbridge_agg$count) #cor: 0.5

length(which(kwiterambere_rainbridge$mon==11)) #29 days
length(which(kwiterambere_rainbridge$mon==12)) #18
length(which(kwiterambere_rainbridge$mon==1)) #0
length(which(kwiterambere_rainbridge$mon==2))#0
length(which(kwiterambere_rainbridge$mon==3))#0
length(which(kwiterambere_rainbridge$mon==4))#26
length(which(kwiterambere_rainbridge$mon==5))#27
length(which(kwiterambere_rainbridge$mon==6))#30
length(which(kwiterambere_rainbridge$mon==7))#12
length(which(kwiterambere_rainbridge$mon==8))#0
length(which(kwiterambere_rainbridge$mon==9))#0
length(which(kwiterambere_rainbridge$mon==10))#0


kwiterambere_rainbridge_agg2<-subset(kwiterambere_rainbridge_agg,Month %in% c("Nov","Apr","May","Jun"))

cor(kwiterambere_rainbridge_agg2$rf,kwiterambere_rainbridge_agg2$count) #cor: 0.15 p=0.8477

#Rugeshi

rugeshi_rainfall<-all_basins_rainfall_t[,c("date","Rugeshi-Gasasa")]
colnames(rugeshi_rainfall)[2]<-"rf"
rugeshi_rainfall$rflag1d<-dplyr::lag(rugeshi_rainfall$rf,1)
rugeshi_rainfall$rflag2d<-dplyr::lag(rugeshi_rainfall$rf,2)
rugeshi_rainfall$rflag3d<-dplyr::lag(rugeshi_rainfall$rf,3)
rugeshi_rainfall$rflag4d<-dplyr::lag(rugeshi_rainfall$rf,4)
rugeshi_rainfall$rflag5d<-dplyr::lag(rugeshi_rainfall$rf,5)

rugeshi_rainfall$rf.anom<-((rugeshi_rainfall$rf-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100
rugeshi_rainfall$rf.anomlg1<-((rugeshi_rainfall$rflag1d-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100
rugeshi_rainfall$rf.anomlg2<-((rugeshi_rainfall$rflag2d-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100
rugeshi_rainfall$rf.anomlg3<-((rugeshi_rainfall$rflag3d-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100
rugeshi_rainfall$rf.anomlg4<-((rugeshi_rainfall$rflag4d-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100
rugeshi_rainfall$rf.anomlg5<-((rugeshi_rainfall$rflag5d-mean(rugeshi_rainfall$rf))/mean(rugeshi_rainfall$rf))*100

#merge bridge crossings and rainfall
rugeshi_rainfall$date<-as.Date(rugeshi_rainfall$date)
daily_rugeshi_crossings$date<-as.Date(daily_rugeshi_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

rugeshi_rainbridge_merge<-merge(rugeshi_rainfall,daily_rugeshi_crossings,by="date")
cor(rugeshi_rainbridge_merge$rf,rugeshi_rainbridge_merge$count)
cor(rugeshi_rainbridge_merge$rflag3d,rugeshi_rainbridge_merge$count)

# rugeshi_sm_merge<-merge(ruliba_ml_data,daily_rugeshi_crossings,by="date")
# cor(rugeshi_sm_merge$discharge,rugeshi_sm_merge$count)
# cor(rugeshi_sm_merge$sm,rugeshi_sm_merge$count)

rugeshi_rainbridge<-rugeshi_rainbridge_merge[,c(1:7,14)]
rugeshi_rainbridge$mon<-month(rugeshi_rainbridge$date)
rugeshi_rainbridge_agg<-aggregate(.~mon,data=rugeshi_rainbridge,sum)
rugeshi_rainbridge_agg$Month<-month.abb[rugeshi_rainbridge_agg$mon]
cor(rugeshi_rainbridge_agg$rf,rugeshi_rainbridge_agg$count) #cor: 0.81

length(which(rugeshi_rainbridge$mon==11)) #18 days
length(which(rugeshi_rainbridge$mon==12)) #0
length(which(rugeshi_rainbridge$mon==1)) #0
length(which(rugeshi_rainbridge$mon==2))#1
length(which(rugeshi_rainbridge$mon==3))#31
length(which(rugeshi_rainbridge$mon==4))#22
length(which(rugeshi_rainbridge$mon==5))#31
length(which(rugeshi_rainbridge$mon==6))#2
length(which(rugeshi_rainbridge$mon==7))#13
length(which(rugeshi_rainbridge$mon==8))#18
length(which(rugeshi_rainbridge$mon==9))#30
length(which(rugeshi_rainbridge$mon==10))#18


rugeshi_rainbridge_agg2<-subset(rugeshi_rainbridge_agg,Month %in% c("Mar","Apr","May","Sep"))

cor(rugeshi_rainbridge_agg2$rf,rugeshi_rainbridge_agg2$count) #cor: 0.58 p=0.4155

#Uwumugeti

uwumugeti_rainfall<-all_basins_rainfall_t[,c("date","Uwumugeti")]
colnames(uwumugeti_rainfall)[2]<-"rf"
uwumugeti_rainfall$rflag1d<-dplyr::lag(uwumugeti_rainfall$rf,1)
uwumugeti_rainfall$rflag2d<-dplyr::lag(uwumugeti_rainfall$rf,2)
uwumugeti_rainfall$rflag3d<-dplyr::lag(uwumugeti_rainfall$rf,3)
uwumugeti_rainfall$rflag4d<-dplyr::lag(uwumugeti_rainfall$rf,4)
uwumugeti_rainfall$rflag5d<-dplyr::lag(uwumugeti_rainfall$rf,5)

uwumugeti_rainfall$rf.anom<-((uwumugeti_rainfall$rf-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100
uwumugeti_rainfall$rf.anomlg1<-((uwumugeti_rainfall$rflag1d-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100
uwumugeti_rainfall$rf.anomlg2<-((uwumugeti_rainfall$rflag2d-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100
uwumugeti_rainfall$rf.anomlg3<-((uwumugeti_rainfall$rflag3d-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100
uwumugeti_rainfall$rf.anomlg4<-((uwumugeti_rainfall$rflag4d-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100
uwumugeti_rainfall$rf.anomlg5<-((uwumugeti_rainfall$rflag5d-mean(uwumugeti_rainfall$rf))/mean(uwumugeti_rainfall$rf))*100

#merge bridge crossings and rainfall
uwumugeti_rainfall$date<-as.Date(uwumugeti_rainfall$date)
daily_uwumugeti_crossings$date<-as.Date(daily_uwumugeti_crossings$date)
ruliba_ml_data$date<-as.Date(ruliba_ml_data$date)

uwumugeti_rainbridge_merge<-merge(uwumugeti_rainfall,daily_kigusa_crossings,by="date")
cor(uwumugeti_rainbridge_merge$rf,uwumugeti_rainbridge_merge$count)
cor(uwumugeti_rainbridge_merge$rflag3d,uwumugeti_rainbridge_merge$count)

# uwumugeti_sm_merge<-merge(ruliba_ml_data,daily_uwumugeti_crossings,by="date")
# cor(uwumugeti_sm_merge$discharge,uwumugeti_sm_merge$count)
# cor(uwumugeti_sm_merge$sm,uwumugeti_sm_merge$count)

uwumugeti_rainbridge<-uwumugeti_rainbridge_merge[,c(1:7,14)]
uwumugeti_rainbridge$mon<-month(uwumugeti_rainbridge$date)
uwumugeti_rainbridge_agg<-aggregate(.~mon,data=uwumugeti_rainbridge,sum)
uwumugeti_rainbridge_agg$Month<-month.abb[uwumugeti_rainbridge_agg$mon]
cor(uwumugeti_rainbridge_agg$rf,uwumugeti_rainbridge_agg$count) #cor: 0.37

length(which(uwumugeti_rainbridge$mon==11)) #23 days
length(which(uwumugeti_rainbridge$mon==12)) #21
length(which(uwumugeti_rainbridge$mon==1)) #0
length(which(uwumugeti_rainbridge$mon==2))#2
length(which(uwumugeti_rainbridge$mon==3))#31
length(which(uwumugeti_rainbridge$mon==4))#30
length(which(uwumugeti_rainbridge$mon==5))#31
length(which(uwumugeti_rainbridge$mon==6))#30
length(which(uwumugeti_rainbridge$mon==7))#31
length(which(uwumugeti_rainbridge$mon==8))#22
length(which(uwumugeti_rainbridge$mon==9))#0
length(which(uwumugeti_rainbridge$mon==10))#0


uwumugeti_rainbridge_agg2<-subset(uwumugeti_rainbridge_agg,Month %in% c("Nov","Dec","Mar","Apr","May","Jun","Jul","Aug"))

cor(uwumugeti_rainbridge_agg2$rf,uwumugeti_rainbridge_agg2$count) #cor: 0.06 p=0.8873


#import discharge data and compare with crossings
setwd("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/discharge/predicted/final")
#Mutiwingoma 
mut_disc<-read.csv("./mutiwingoma_discharge_predictedRF.csv")
mut_disc$date<-as.Date(mut_disc$date)
nta_disc<-read.csv("./ntaruka_discharge_predictedRF.csv")
nta_disc$date<-as.Date(nta_disc$date)
kwi_disc<-read.csv("./kwiterambere_discharge_predictedRF.csv")
kwi_disc$date<-as.Date(kwi_disc$date)
muh_disc<-read.csv("./muhembe_discharge_predictedRF.csv")
muh_disc$date<-as.Date(muh_disc$date)
rug_disc<-read.csv("./rugeshi_discharge_predictedRF.csv")
rug_disc$date<-as.Date(rug_disc$date)
gas_disc<-read.csv("./gasasa_discharge_predictedRF.csv")
gas_disc$date<-as.Date(gas_disc$date)
nya_disc<-read.csv("./nyarusange_discharge_predictedRF.csv")
nya_disc$date<-as.Date(nya_disc$date)
uwu_disc<-read.csv("./uwumugeti_discharge_predictedRF.csv")
uwu_disc$date<-as.Date(uwu_disc$date)

#merge with camera data
mut_merge<-merge(mut_disc,daily_mutiwingoma_crossings,by="date")
nta_merge<-merge(nta_disc,daily_ntaruka_crossings,by="date")
kwi_merge<-merge(kwi_disc,daily_kwiterambere_crossings,by="date")
muh_merge<-merge(muh_disc,daily_muhembe_crossings,by="date")
rug_merge<-merge(rug_disc,daily_rugeshi_crossings,by="date")
gas_merge<-merge(gas_disc,daily_gasasa_crossings,by="date")
nya_merge<-merge(nya_disc,daily_nyarusange_crossings,by="date")
uwu_merge<-merge(uwu_disc,daily_kigusa_crossings,by="date")

#correlations
cor(mut_merge$dischargeRF,mut_merge$count)
cor(nta_merge$dischargeRF,nta_merge$count)
cor(kwi_merge$dischargeRF,kwi_merge$count)
cor(muh_merge$dischargeRF,muh_merge$count)
cor(rug_merge$dischargeRF,rug_merge$count)
cor(gas_merge$dischargeRF,gas_merge$count)
cor(nya_merge$dischargeRF,nya_merge$count)
cor(uwu_merge$dischargeRF,uwu_merge$count)

#compare crossings between high and low discharge
mut_higQ<-mut_merge[mut_merge$dischargeRF>=6.06,]#Q40
mut_lowQ<-mut_merge[mut_merge$dischargeRF<=5.16,]#Q70
t.test(mut_higQ$count,mut_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

kwi_higQ<-kwi_merge[kwi_merge$dischargeRF>=4.01,]
kwi_lowQ<-kwi_merge[kwi_merge$dischargeRF<=3.41,]
t.test(kwi_higQ$count,kwi_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

nta_higQ<-nta_merge[nta_merge$dischargeRF>=0.76,]
nta_lowQ<-nta_merge[nta_merge$dischargeRF<=0.65,]
t.test(nta_higQ$count,nta_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

muh_higQ<-muh_merge[muh_merge$dischargeRF>=2.1,]
muh_lowQ<-muh_merge[muh_merge$dischargeRF<=1.79,]
t.test(muh_higQ$count,muh_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

nya_higQ<-nya_merge[nya_merge$dischargeRF>=1.24,]
nya_lowQ<-nya_merge[nya_merge$dischargeRF<=1.05,]
t.test(nya_higQ$count,nya_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

rug_higQ<-rug_merge[rug_merge$dischargeRF>=1.47,]
rug_lowQ<-rug_merge[rug_merge$dischargeRF<=1.25,]
t.test(rug_higQ$count,rug_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test


gas_higQ<-gas_merge[gas_merge$dischargeRF>=1.47,]
gas_lowQ<-gas_merge[gas_merge$dischargeRF<=1.25,]
t.test(gas_higQ$count,gas_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test


uwu_higQ<-uwu_merge[uwu_merge$dischargeRF>=0.37,]
uwu_lowQ<-uwu_merge[uwu_merge$dischargeRF<=0.31,]
t.test(uwu_higQ$count,uwu_lowQ$count,var.equal = FALSE) #Welch Two Sample t-test

#monthly comparisons
library(lubridate)
library(plyr)

mut_merge$mon<-month(mut_merge$date)
mut_mon<-ddply(mut_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(mut_mon$avgQ,mut_mon$totCros) #cor -0.17
mut_mon2<-subset(mut_mon,mon %in% c("11","12","3","4","5"))
cor(mut_mon2$avgQ,mut_mon2$totCros) #cor 0.59

kwi_merge$mon<-month(kwi_merge$date)
kwi_mon<-ddply(kwi_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(kwi_mon$avgQ,kwi_mon$totCros) #cor 0.42
kwi_mon2<-subset(kwi_mon,mon %in% c("11","4","5","6"))
cor(kwi_mon2$avgQ,kwi_mon2$totCros) #cor -0.66

nta_merge$mon<-month(nta_merge$date)
nta_mon<-ddply(nta_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(nta_mon$avgQ,nta_mon$totCros) #cor 0.14
nta_mon2<-subset(nta_mon,mon %in% c("11","12","3","4","5","6","7"))
cor(nta_mon2$avgQ,nta_mon2$totCros) #cor -0.31

nya_merge$mon<-month(nya_merge$date)
nya_mon<-ddply(nya_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(nya_mon$avgQ,nya_mon$totCros) #cor -0.49
nya_mon2<-subset(nya_mon,mon %in% c("11","6","7","9"))
cor(nya_mon2$avgQ,nya_mon2$totCros) #cor -0.29

rug_merge$mon<-month(rug_merge$date)
rug_mon<-ddply(rug_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(rug_mon$avgQ,rug_mon$totCros) #cor -0.14
rug_mon2<-subset(rug_mon,mon %in% c("3","4","5","9"))
cor(rug_mon2$avgQ,rug_mon2$totCros) #cor -0.13


gas_merge$mon<-month(gas_merge$date)
gas_mon<-ddply(gas_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(gas_mon$avgQ,gas_mon$totCros) #cor -0.35

muh_merge$mon<-month(muh_merge$date)
muh_mon<-ddply(muh_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(muh_mon$avgQ,muh_mon$totCros) #cor 0.13
muh_mon2<-subset(muh_mon,mon %in% c("11","12","4","5","6"))
cor(muh_mon2$avgQ,muh_mon2$totCros) #cor 0.43

uwu_merge$mon<-month(uwu_merge$date)
uwu_mon<-ddply(uwu_merge,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count))
cor(uwu_mon$avgQ,uwu_mon$totCros) #cor -0.09
uwu_mon2<-subset(uwu_mon,mon %in% c("11","12","3","4","5","6","7","8"))
cor(uwu_mon2$avgQ,uwu_mon2$totCros) #cor 0.3


#processing data for determinants model
#http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#spatial-error-and-lag-models

#
library(plyr)

bridge_buffers<-readOGR(dsn="C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/catchments/bridge_5km_buffer.shp", layer = "bridge_5km_buffer")
road_dens<-raster("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/roads/road_density.tif")
pop_dens<-raster("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/population/rwa_pd_2020_1km.tif")
elevation<-raster("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/data/dem/Rwanda_SRTM30meters.tif")

bridge_roads<-raster::extract(road_dens,bridge_buffers,fun=mean)
bridge_pop<-raster::extract(pop_dens,bridge_buffers,fun=mean)
bridge_elev<-raster::extract(elevation,bridge_buffers,fun=mean)

rownames(bridge_roads)<-bridge_buffers$Site_Name
bridge_roads<-as.data.frame(bridge_roads)
bridge_roads<-rownames_to_column(bridge_roads,var="site")
colnames(bridge_roads)[2]<-"density"

rownames(bridge_pop)<-bridge_buffers$Site_Name
bridge_pop<-as.data.frame(bridge_pop)
bridge_pop<-rownames_to_column(bridge_pop,var="site")
colnames(bridge_pop)[2]<-"density"

rownames(bridge_elev)<-bridge_buffers$Site_Name
bridge_elev<-as.data.frame(bridge_elev)
bridge_elev<-rownames_to_column(bridge_elev,var="site")
colnames(bridge_elev)[2]<-"elev"

#mutiwingoma
mut_merge2<-mut_merge2 %>% mutate(flood=ifelse(mut_merge2$dischargeRF>=6.06,1,0))
mut_rain<-mutiwingoma_rainbridge[,c("date","rf","mon")]
mut_merge3<-merge(mut_rain,mut_merge2,by=c("date","mon"))
mut_pop<-bridge_pop[bridge_pop$site=="Mutiwingoma",]
mut_road<-bridge_roads[bridge_roads$site=="Mutiwingoma",]
mut_elev<-bridge_elev[bridge_elev$site=="Mutiwingoma",]

mut_merge4<-ddply(mut_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
mut_merge4$pop<-mut_pop$density
mut_merge4$roads<-mut_road$density
mut_merge4$elev<-mut_elev$elev
mut_sel<-subset(mut_merge4,mon %in% c("11","12","3","4","5"))
cor(mut_sel$totCros,mut_sel$totFL) #0.84


#kwiterambere
kwi_merge2<-kwi_merge %>% mutate(flood=ifelse(kwi_merge$dischargeRF>=4.01,1,0))
kwi_rain<-kwiterambere_rainbridge[,c("date","rf","mon")]
kwi_merge3<-merge(kwi_rain,kwi_merge2,by=c("date","mon"))
kwi_pop<-bridge_pop[bridge_pop$site=="Kwiterambere",]
kwi_road<-bridge_roads[bridge_roads$site=="Kwiterambere",]
kwi_elev<-bridge_elev[bridge_elev$site=="Kwiterambere",]

kwi_merge4<-ddply(kwi_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
kwi_merge4$pop<-kwi_pop$density
kwi_merge4$roads<-kwi_road$density
kwi_merge4$elev<-kwi_elev$elev
kwi_sel<-subset(kwi_merge4,mon %in% c("11","4","5","6"))
cor(kwi_sel$totCros,kwi_sel$totFL) #-0.53


#Ntaruka
nta_merge2<-nta_merge %>% mutate(flood=ifelse(nta_merge$dischargeRF>=0.76,1,0))
nta_rain<-ntaruka_rainbridge[,c("date","rf","mon")]
nta_merge3<-merge(nta_rain,nta_merge2,by=c("date","mon"))
nta_pop<-bridge_pop[bridge_pop$site=="Ntaruka",]
nta_road<-bridge_roads[bridge_roads$site=="Ntaruka",]
nta_elev<-bridge_elev[bridge_elev$site=="Ntaruka",]

nta_merge4<-ddply(nta_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
nta_merge4$pop<-nta_pop$density
nta_merge4$roads<-nta_road$density
nta_merge4$elev<-nta_elev$elev
nta_sel<-subset(nta_merge4,mon %in% c("11","12","3","4","5","6","7"))
cor(nta_sel$totCros,nta_sel$totFL) #-0.33

#Muhembe
muh_merge2<-muh_merge %>% mutate(flood=ifelse(muh_merge$dischargeRF>=2.1,1,0))
muh_rain<-muhembe_rainbridge[,c("date","rf","mon")]
muh_merge3<-merge(muh_rain,muh_merge2,by=c("date","mon"))
muh_pop<-bridge_pop[bridge_pop$site=="Muhembe",]
muh_road<-bridge_roads[bridge_roads$site=="Muhembe",]
muh_elev<-bridge_elev[bridge_elev$site=="Muhembe",]

muh_merge4<-ddply(muh_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
muh_merge4$pop<-muh_pop$density
muh_merge4$roads<-muh_road$density
muh_merge4$elev<-muh_elev$elev
muh_sel<-subset(muh_merge4,mon %in% c("11","12","4","5","6"))
cor(muh_sel$totCros,muh_sel$totFL) #0.84

#Nyarusange
nya_merge2<-nya_merge %>% mutate(flood=ifelse(nya_merge$dischargeRF>=1.24,1,0))
nya_rain<-nyarusange_rainbridge[,c("date","rf","mon")]
nya_merge3<-merge(nya_rain,nya_merge2,by=c("date","mon"))
nya_pop<-bridge_pop[bridge_pop$site=="Nyarusange",]
nya_road<-bridge_roads[bridge_roads$site=="Nyarusange",]
nya_elev<-bridge_elev[bridge_elev$site=="Nyarusange",]

nya_merge4<-ddply(nya_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
nya_merge4$pop<-nya_pop$density
nya_merge4$roads<-nya_road$density
nya_merge4$elev<-nya_elev$elev
nya_sel<-subset(nya_merge4,mon %in% c("11","6","7","9"))
cor(nya_sel$totCros,nya_sel$totFL) #0.05

#Rugeshi
rug_merge2<-rug_merge %>% mutate(flood=ifelse(rug_merge$dischargeRF>=1.47,1,0))
rug_rain<-rugeshi_rainbridge[,c("date","rf","mon")]
rug_merge3<-merge(rug_rain,rug_merge2,by=c("date","mon"))
rug_pop<-bridge_pop[bridge_pop$site=="Rugeshi",]
rug_road<-bridge_roads[bridge_roads$site=="Rugeshi",]
rug_elev<-bridge_elev[bridge_elev$site=="Rugeshi",]

rug_merge4<-ddply(rug_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
rug_merge4$pop<-rug_pop$density
rug_merge4$roads<-rug_road$density
rug_merge4$elev<-rug_elev$elev
rug_sel<-subset(rug_merge4,mon %in% c("3","4","5","9"))
cor(rug_sel$totCros,rug_sel$totFL) #0.55

#Gasasa
# gas_cross<-daily_gasasa_crossings
# gas_cross$mon<-month(gas_cross$date)
length(which(gas_merge3$mon==1))#0
length(which(gas_merge3$mon==2))#0
length(which(gas_merge3$mon==3))#0
length(which(gas_merge3$mon==4))#22
length(which(gas_merge3$mon==5))#31
length(which(gas_merge3$mon==6))#2
length(which(gas_merge3$mon==7))#11
length(which(gas_merge3$mon==8))#3
length(which(gas_merge3$mon==9))#30
length(which(gas_merge3$mon==10))#1
length(which(gas_merge3$mon==11))#18
length(which(gas_merge3$mon==12))#0


gas_merge2<-gas_merge %>% mutate(flood=ifelse(gas_merge$dischargeRF>=1.47,1,0))
gas_rain<-rugeshi_rainbridge[,c("date","rf","mon")]
gas_merge3<-merge(gas_rain,gas_merge2,by=c("date","mon"))
gas_pop<-bridge_pop[bridge_pop$site=="Rugeshi",]
gas_road<-bridge_roads[bridge_roads$site=="Rugeshi",]
gas_elev<-bridge_elev[bridge_elev$site=="Rugeshi",]

gas_merge4<-ddply(gas_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
gas_merge4$pop<-gas_pop$density
gas_merge4$roads<-gas_road$density
gas_merge4$elev<-gas_elev$elev
gas_sel<-subset(gas_merge4,mon %in% c("4","5","9"))
round(cor(gas_sel$totCros,gas_sel$totFL),2) #-0.5

#Uwumugeti
uwu_merge2<-uwu_merge %>% mutate(flood=ifelse(uwu_merge$dischargeRF>=0.37,1,0))
uwu_rain<-uwumugeti_rainbridge[,c("date","rf","mon")]
uwu_merge3<-merge(uwu_rain,uwu_merge2,by=c("date","mon"))
uwu_pop<-bridge_pop[bridge_pop$site=="Uwumugeti-Kigusa",]
uwu_road<-bridge_roads[bridge_roads$site=="Uwumugeti-Kigusa",]
uwu_elev<-bridge_elev[bridge_elev$site=="Uwumugeti-Kigusa",]

uwu_merge4<-ddply(uwu_merge3,.(mon),summarise,avgQ=mean(dischargeRF),totCros=sum(count),totRf=sum(rf),totFL=sum(flood))
uwu_merge4$pop<-uwu_pop$density
uwu_merge4$roads<-uwu_road$density
uwu_merge4$elev<-uwu_elev$elev
uwu_sel<-subset(uwu_merge4,mon %in% c("11","12","3","4","5","6","7","8"))
round(cor(uwu_sel$totCros,uwu_sel$totFL),2) #0.36


# bridge_model<-rbind(mut_sel,nta_sel,kwi_sel,nya_sel,rug_sel,gas_sel,uwu_sel)
# bridge.lm<-lm(totCros~.,data = bridge_model[,-1],method = "qr")


###RESULTS FOR MONTHLY ASSOCIATIONS
# correlations with monthly rainfall
# mut 0.76
# nta 0.47
# kwi 0.15
# nya 0.75
# rug 0.58
# muh 0.25
# gas 0.99#??
# uwu 0.06
# 
# correlations with streamflow
# mut 0.59
# nta -0.31
# kwi -0.66
# nya -0.29
# rug -0.13
# muh 0.43
# gas -0.35
# uwu 0.3
# correlations with flood events
# mut 0.84
# nta -0.33
# kwi -0.53
# nya 0.05
# rug 0.55
# muh 0.84
# gas -0.5
# uwu 0.36

###correlations between crossings and geographical covariates
cor_stats<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Macharia/B2P/ML/floods/correlations_crossings.csv")

cr1=ggplot(cor_stats, aes(x=Average_crossings,y=Pop_density ))+
  geom_point(alpha=0.5)+
  labs(x="Crossings",y="Population density (people/km2)")+
  geom_smooth(method=lm)+theme_bw()

cr2=ggplot(cor_stats, aes(x=Average_crossings,y=Road_density ))+
  geom_point(alpha=0.5)+
  labs(x="Crossings",y="Road density (length (m)/km2)")+
  geom_smooth(method=lm)+theme_bw()

grid.arrange(cr1,cr2,
             ncol=1,nrow=2)
cor.test(cor_stats$Road_density,cor_stats$Average_crossings)
t.test(cor_stats$Road_density,cor_stats$Average_crossings)

cor.test(cor_stats$Pop_density,cor_stats$Average_crossings)
t.test(cor_stats$Pop_density,cor_stats$Average_crossings)


##plot for presentation
library(dplyr)
rug_pt<-select(rug_sel,mon,totCros)
# gas_pt<-select(gas_sel,mon,totCros)
muh_pt<-select(muh_sel,mon,totCros)
kwi_pt<-select(kwi_sel,mon,totCros)
nya_pt<-select(nya_sel,mon,totCros)
nta_pt<-select(nta_sel,mon,totCros)
mut_pt<-select(mut_sel,mon,totCros)
uwu_pt<-select(uwu_sel,mon,totCros)

rug_pt$name<-"Rugeshi"
muh_pt$name<-"Muhembe"
kwi_pt$name<-"Kwiterambere"
nya_pt$name<-"Nyarusange"
nta_pt$name<-"Ntaruka"
mut_pt$name<-"Mutiwingoma"
uwu_pt$name<-"Uwumugeti"

pt_comb<-rbind(rug_pt,muh_pt,kwi_pt,nya_pt,nta_pt,mut_pt,uwu_pt)
# library(lubridate)
pt_comb$month<-month.abb[pt_comb$mon]

ggplot(pt_comb, aes(x=month,y=totCros))+
  geom_point()+
  xlab("Month")+
  ylab("Crossings")+
  facet_wrap(~name,ncol = 2)+
  scale_x_discrete(limits = month.abb)
