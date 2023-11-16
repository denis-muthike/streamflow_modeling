
###Flood frequency analysis

#some resources: ##https://rpubs.com/tbiggs/GEOG576_EX5_FCDs: #https://rpubs.com/cassiorampinelli/528388
library(timeSeries)
library(hydroTSM)

setwd("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/final")
Q_fdc<-read.csv("./RF_predictedQ_forFDC.csv")

Qfdc.ts<-ts(Q_fdc)
timeSeries::plot(Qfdc.ts)
summary(Qfdc.ts)

par(mfrow=c(3,2))

flow<-Q_fdc$dischargeRF
flow<-sort(flow,decreasing=T)
df<-data.frame(x=100/length(flow)*1:length(flow),y=flow)
plot(x = df$x, y = df$y, type = "l", log = "y",ylab="Q [m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded",main="Flow Duration Curve")
# grid()

x=df$x
y=df$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

duration.dataframe

#         %  	Q,[m3/s]
# [1,]	  5 	142.85623
# [2,] 	  10 	130.49590
# [3,]	  20 	118.02997
# [4,] 	  30 	110.43484
# [5,] 	  40 	104.16072
# [6,] 	  50  98.33406
# [7,]	  60  92.89583
# [8,] 	  70  88.45124
# [9,] 	  80  84.70622
# [10,]	  90  80.08482
# [11,] 	95  76.56352
# [12,] 	99  70.63333


#Annual Duration Series
library(dplyr)
library(lubridate)
library(lmom)

drf<-as.data.frame(Qfdc.ts)
#Creating a daily index for all data range
data_rul<-data_frame(date=seq(as.Date("2003-01-01"),as.Date("2022-10-04"),by=1),amount=drf$dischargeRF)

#Grouping maximum average daily discharge for each year
max.by.year<-data_rul %>% group_by(year=floor_date(date, "year")) %>% summarize(amount=max(amount))
plot(max.by.year,type="l",ylab="Q [m3/s]",xlab="Year",main="Maximum Daily Discharge")

#Recording the maximum discharges by year and removing N.A. values
maximas<-max.by.year$amount
maximas<-maximas[!is.na(maximas)]

#Sorting maxima by decreasing order
sorted.maximas<-sort(maximas,decreasing=T)

#Computing the empirical probabilities
p<-(c(1:length(sorted.maximas)))/(length(sorted.maximas)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(maximas)
para<-pelgum(fit)
para

#Estimating the parameters for Log Pearson type 3 distribution
para3<-pelpe3(fit)
para3

#Plot cumulative probability x discharges for empirical and fitting distribution
plot(1-p,sorted.maximas,ylab="Q [m3/s]",xlab="Cumulative Probability",main="Annual Maximum Discharge CDF")

#Log pearson type 3 fitting
lines(cdfpe3(sorted.maximas,para3),sorted.maximas,col="red")

#Gumbel fitting
lines(cdfgum(sorted.maximas,para),sorted.maximas,col="blue",lty=2)
# grid()
#Legend
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.5)

#Plotting empirical recurrence time and discharges
plot(tr,sorted.maximas,xlab="Recurrence Time (years)",ylab="Q [m3/s]",main="Annual Maximum Discharge Flood Recurrence",ylim=c(100,300),xlim=c(0,50))
# grid()

#Fitting recurrence time employing Gumbel distribution
y<-c(300,sorted.maximas)
gumbel.accum<-cdfgum(y,para)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

#Fitting recurrence time employing Log Pearson 3 distribution
lp3.accum<-cdfpe3(y,para3)
fitted.tr3<-1/(1-lp3.accum)
lines(fitted.tr3,y,col="red")
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.5)

##Partial Duration series on flows exceeding 50%
#Selecting 
p.drf<-drf$dischargeRF
partial.Data<-p.drf[which(p.drf>98)] #flows higher than the daily mean flows
partial.Data.sorted<-sort(partial.Data,decreasing=T)


# #Plotting the maximum discharges by year
# plot(partial.Data,type="l",ylab="Q [m3/s]",xlab="# of days when flows > 103 m3/s",main="Discharges higher than mean daily flows")

#Computing the empirical probabilities
p<-0
p<-(c(1:length(partial.Data.sorted)))/(length(partial.Data.sorted)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(partial.Data)
parag<-pelgum(fit)
parag

#Estimating the parameters for Log Pearson type 3 distribution
para3<-pelpe3(fit)
para3

# par(mfrow=c(1,2))
#Plot cumulative probability x discharges for empirical and fitting distribution
plot(1-p,partial.Data.sorted,ylab="Q [m3/s]",xlab="Cumulative probability",main="Partial Duration Series CDF")

#Log pearson type 3 fitting
lines(cdfpe3(partial.Data.sorted,para3),partial.Data.sorted,col="red")

#Gumbel fitting
lines(cdfgum(partial.Data.sorted,parag),partial.Data.sorted,col="blue",lty=2)
# grid()
#Legend
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.5)

#Plotting empirical recurrence time and discharges
plot(tr,partial.Data.sorted,xlab="Recurrence Time (years)",ylab="",main="Partial Duration Series Flood Recurrence",ylim=c(100,250),xlim=c(0,500))
# grid()

#Fitting recurrence time employing Gumbel distribution
y<-c(250,partial.Data.sorted)
gumbel.accum<-cdfgum(y,parag)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

#Fitting recurrence time emplyoing Log Pearson 3 distribution
lp3.accum<-cdfpe3(y,para3)
fitted.tr3<-1/(1-lp3.accum)
lines(fitted.tr3,y,col="red")
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.5)

#######################################################################

##Recurrence with all timeseries
flow_ts<-Q_fdc$dischargeRF

flow_ts.sorted<-sort(flow_ts,decreasing=T)


# #Plotting the maximum discharges by year
# plot(partial.Data,type="l",ylab="Q [m3/s]",xlab="# of days when flows > 103 m3/s",main="Discharges higher than mean daily flows")

#Computing the empirical probabilities
p<-0
p<-(c(1:length(flow_ts.sorted)))/(length(flow_ts.sorted)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(flow_ts)
parag<-pelgum(fit)
parag

#Estimating the parameters for Log Pearson type 3 distribution
para3<-pelpe3(fit)
para3

par(mfrow=c(1,2))
#Plot cumulative probability x discharges for empirical and fitting distribution
plot(1-p,flow_ts.sorted,ylab="Q [m3/s]",xlab="Cumulative probability",main="Full time series CDF")

#Log pearson type 3 fitting
lines(cdfpe3(flow_ts.sorted,para3),flow_ts.sorted,col="red")

#Gumbel fitting
lines(cdfgum(flow_ts.sorted,parag),flow_ts.sorted,col="blue",lty=2)
# grid()
#Legend
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.7)

#Plotting empirical recurrence time and discharges
plot(tr,flow_ts.sorted,xlab="Recurrence Time (years)",ylab="",main="Flood Recurrence",ylim=c(100,250),xlim=c(0,500))
# grid()

#Fitting recurrence time employing Gumbel distribution
y<-c(250,flow_ts.sorted)
gumbel.accum<-cdfgum(y,parag)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

#Fitting recurrence time emplyoing Log Pearson 3 distribution
lp3.accum<-cdfpe3(y,para3)
fitted.tr3<-1/(1-lp3.accum)
lines(fitted.tr3,y,col="red")
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=0.7)
# grid()

#1. Mutiwingoma FDC
Q_mut_fdc<-read.csv("./mutiwingoma_discharge_predictedRF.csv")

Q_mut.ts<-ts(Q_mut_fdc)
timeSeries::plot(Q_mut.ts)
summary(Q_mut.ts)

flow_mut<-Q_mut_fdc$dischargeRF
flow_mut<-sort(flow_mut,decreasing=T)
df_mut<-data.frame(x=100/length(flow_mut)*1:length(flow_mut),y=flow_mut)
plot(x = df_mut$x, y = df_mut$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_mut$x
y=df_mut$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

duration.dataframe
# 
# % Q,[m3/s]
# [1,]  5 8.399833
# [2,] 10 7.601508
# [3,] 20 6.834564
# [4,] 30 6.419958
# [5,] 40 6.062521
# [6,] 50 5.765611
# [7,] 60 5.470267
# [8,] 70 5.155819
# [9,] 80 4.859090
# [10,] 90 4.574465
# [11,] 95 4.356182
# [12,] 99 3.975376

#2. Ntaruka FDC
Q_nta_fdc<-read.csv("./ntaruka_discharge_predictedRF.csv")

Q_nta.ts<-ts(Q_nta_fdc)
timeSeries::plot(Q_nta.ts)
summary(Q_nta.ts)

flow_nta<-Q_nta_fdc$dischargeRF
flow_nta<-sort(flow_nta,decreasing=T)
df_nta<-data.frame(x=100/length(flow_nta)*1:length(flow_nta),y=flow_nta)
plot(x = df_nta$x, y = df_nta$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_nta$x
y=df_nta$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

duration.dataframe

# %  Q,[m3/s]
# [1,]  5 1.0547173
# [2,] 10 0.9544763
# [3,] 20 0.8581757
# [4,] 30 0.8061160
# [5,] 40 0.7612348
# [6,] 50 0.7239536
# [7,] 60 0.6868690
# [8,] 70 0.6473856
# [9,] 80 0.6101271
# [10,] 90 0.5743885
# [11,] 95 0.5469800
# [12,] 99 0.4991644

#3. Kwiterambere FDC
Q_kwi_fdc<-read.csv("./kwiterambere_discharge_predictedRF.csv")
# library(timeSeries)
Q_kwi.ts<-ts(Q_kwi_fdc)
timeSeries::plot(Q_kwi.ts)
summary(Q_kwi.ts)

flow_kwi<-Q_kwi_fdc$dischargeRF
flow_kwi<-sort(flow_kwi,decreasing=T)
df_kwi<-data.frame(x=100/length(flow_kwi)*1:length(flow_kwi),y=flow_kwi)
plot(x = df_kwi$x, y = df_kwi$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_kwi$x
y=df_kwi$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

duration.dataframe

# % Q,[m3/s]
# [1,]  5 5.553159
# [2,] 10 5.025383
# [3,] 20 4.518354
# [4,] 30 4.244256
# [5,] 40 4.007954
# [6,] 50 3.811665
# [7,] 60 3.616412
# [8,] 70 3.408530
# [9,] 80 3.212361
# [10,] 90 3.024195
# [11,] 95 2.879887
# [12,] 99 2.628135

#4. Nyarusange FDC
Q_nya_fdc<-read.csv("./nyarusange_discharge_predictedRF.csv")
# library(timeSeries)
Q_nya.ts<-ts(Q_nya_fdc)
timeSeries::plot(Q_nya.ts)
summary(Q_nya.ts)

flow_nya<-Q_nya_fdc$dischargeRF
flow_nya<-sort(flow_nya,decreasing=T)
df_nya<-data.frame(x=100/length(flow_nya)*1:length(flow_nya),y=flow_nya)
plot(x = df_nya$x, y = df_nya$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_nya$x
y=df_nya$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

round(duration.dataframe,2)

# % Q,[m3/s]
# [1,]  5     1.71
# [2,] 10     1.55
# [3,] 20     1.39
# [4,] 30     1.31
# [5,] 40     1.24
# [6,] 50     1.18
# [7,] 60     1.12
# [8,] 70     1.05
# [9,] 80     0.99
# [10,] 90     0.93
# [11,] 95     0.89
# [12,] 99     0.81

#5. Muhembe FDC
Q_muh_fdc<-read.csv("./muhembe_discharge_predictedRF.csv")
# library(timeSeries)
Q_muh.ts<-ts(Q_muh_fdc)
timeSeries::plot(Q_muh.ts)
summary(Q_muh.ts)

flow_muh<-Q_muh_fdc$dischargeRF
flow_muh<-sort(flow_muh,decreasing=T)
df_muh<-data.frame(x=100/length(flow_muh)*1:length(flow_muh),y=flow_muh)
plot(x = df_muh$x, y = df_muh$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_muh$x
y=df_muh$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

round(duration.dataframe,2)

# 
# % Q,[m3/s]
# [1,]  5     2.91
# [2,] 10     2.64
# [3,] 20     2.37
# [4,] 30     2.23
# [5,] 40     2.10
# [6,] 50     2.00
# [7,] 60     1.90
# [8,] 70     1.79
# [9,] 80     1.69
# [10,] 90     1.59
# [11,] 95     1.51
# [12,] 99     1.38

#6. Rugeshi/Gasasa FDC
Q_rug_fdc<-read.csv("./rugeshi_discharge_predictedRF.csv")
# library(timeSeries)
Q_rug.ts<-ts(Q_rug_fdc)
timeSeries::plot(Q_rug.ts)
summary(Q_rug.ts)

flow_rug<-Q_rug_fdc$dischargeRF
flow_rug<-sort(flow_rug,decreasing=T)
df_rug<-data.frame(x=100/length(flow_rug)*1:length(flow_rug),y=flow_rug)
plot(x = df_rug$x, y = df_rug$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_rug$x
y=df_rug$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

round(duration.dataframe,2)

# % Q,[m3/s]
# [1,]  5     2.04
# [2,] 10     1.85
# [3,] 20     1.66
# [4,] 30     1.56
# [5,] 40     1.47
# [6,] 50     1.40
# [7,] 60     1.33
# [8,] 70     1.25
# [9,] 80     1.18
# [10,] 90     1.11
# [11,] 95     1.06
# [12,] 99     0.97

#7. Uwumugeti FDC
Q_uwu_fdc<-read.csv("./uwumugeti_discharge_predictedRF.csv")
# library(timeSeries)
Q_uwu.ts<-ts(Q_uwu_fdc)
timeSeries::plot(Q_uwu.ts)
summary(Q_uwu.ts)

flow_uwu<-Q_uwu_fdc$dischargeRF
flow_uwu<-sort(flow_uwu,decreasing=T)
df_uwu<-data.frame(x=100/length(flow_uwu)*1:length(flow_uwu),y=flow_uwu)
plot(x = df_uwu$x, y = df_uwu$y, type = "l", log = "y",ylab="Q,[m3/s]",xlab="Percentage of Time Flow is Equaled or Exceeded (%)",main="Flow Duration Curve")
grid()

x=df_uwu$x
y=df_uwu$y

#Table with the flow duration

percentage=c(5,10,20,30,40,50,60,70,80,90,95,99)

vazoes=c(y[which.min(abs(x - 5))],y[which.min(abs(x - 10))],y[which.min(abs(x - 20))],y[which.min(abs(x - 30))],y[which.min(abs(x - 40))],y[which.min(abs(x - 50))],y[which.min(abs(x - 60))],y[which.min(abs(x - 70))],y[which.min(abs(x - 80))],y[which.min(abs(x - 90))],y[which.min(abs(x - 95))],y[which.min(abs(x - 99))])

duration.dataframe<-cbind(percentage,vazoes)


colnames(duration.dataframe)=c("%","Q,[m3/s]")

round(duration.dataframe,2)

# % Q,[m3/s]
# [1,]  5     0.51
# [2,] 10     0.46
# [3,] 20     0.41
# [4,] 30     0.39
# [5,] 40     0.37
# [6,] 50     0.35
# [7,] 60     0.33
# [8,] 70     0.31
# [9,] 80     0.29
# [10,] 90     0.28
# [11,] 95     0.26
# [12,] 99     0.24

#Using hydroTSM function

par(mfrow = c(3,3))

kwi_fdc<-fdc(Q_kwi_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,main = "Kwiterambere")
nta_fdc<-fdc(Q_nta_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new = T,col="red",xlab="",ylab="",main = "Ntaruka")
mut_fdc<-fdc(Q_mut_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,col="blue",xlab="",ylab="",main = "Mutiwingoma")
nya_fdc<-fdc(Q_nya_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,col="gray",xlab="",ylab="",main = "Nyarusange")
rug_fdc<-fdc(Q_rug_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,col="orange",xlab="",ylab="",main = "Rugeshi/Gasasa")
uwu_fdc<-fdc(Q_uwu_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,col="green",xlab="",ylab="",main = "Uwumugeti")
muh_fdc<-fdc(Q_muh_fdc$dischargeRF,hQ.thr=0.4,lQ.thr=0.8,new=T,col="navy",xlab="",ylab="",main = "Muhembe")
rul_fdc<-fdc(Q_fdc$dischargeRF,lQ.thr=0.8,hQ.thr=0.4,new=T,col="black",xlab="",ylab="",main = "Ruliba")




