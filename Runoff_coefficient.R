
install.packages("hydromad")

##

calculate_runoff_coefficient <- function(daily_rainfall, daily_runoff) {
  # Calculate the total rainfall and total runoff
  total_rainfall <- sum(daily_rainfall)
  total_runoff <- sum(daily_runoff)
  
  # Calculate the runoff coefficient
  runoff_coefficient <- total_runoff / total_rainfall
  
  # Return the runoff coefficient
  return(runoff_coefficient)
}

# Example data
rainfall <- c(10, 8, 5, 12, 15)  # Daily rainfall values
runoff <- c(2, 3, 1, 4, 5)       # Daily runoff values

# Calculate the runoff coefficient
coefficient <- calculate_runoff_coefficient(rainfall, runoff)

# Print the result
print(coefficient)


## additional analysis
test_muh_manuscript<-muhembe_ml_data
library(lubridate)
test_muh_manuscript$year<-year(test_muh_manuscript$date)
test_muh_manuscript_agg<-test_muh_manuscript[,c(2,22)]
test_muh_manuscript_agg<-aggregate(.~year,data = test_muh_manuscript_agg,sum,na.rm=T)
mean(test_muh_manuscript_agg$rf_mm)

test_nta_manuscript<-ntaruka_ml_data

test_nta_manuscript$year<-year(test_nta_manuscript$date)
test_nta_manuscript_agg<-test_nta_manuscript[,c(2,22)]
test_nta_manuscript_agg<-aggregate(.~year,data = test_nta_manuscript_agg,sum,na.rm=T)
mean(test_nta_manuscript_agg$rf_mm)

test_rug_manuscript<-rugeshi_ml_data

test_rug_manuscript$year<-year(test_rug_manuscript$date)
test_rug_manuscript_agg<-test_rug_manuscript[,c(2,22)]
test_rug_manuscript_agg<-aggregate(.~year,data = test_rug_manuscript_agg,sum,na.rm=T)
mean(test_rug_manuscript_agg$rf_mm)

test_uwu_manuscript<-uwumugeti_ml_data

test_uwu_manuscript$year<-year(test_uwu_manuscript$date)
test_uwu_manuscript_agg<-test_uwu_manuscript[,c(2,22)]
test_uwu_manuscript_agg<-aggregate(.~year,data = test_uwu_manuscript_agg,sum,na.rm=T)
mean(test_uwu_manuscript_agg$rf_mm)

test_mut_manuscript<-mutiwingoma_ml_data

test_mut_manuscript$year<-year(test_mut_manuscript$date)
test_mut_manuscript_agg<-test_mut_manuscript[,c(2,22)]
test_mut_manuscript_agg<-aggregate(.~year,data = test_mut_manuscript_agg,sum,na.rm=T)
mean(test_mut_manuscript_agg$rf_mm)


test_nya_manuscript<-nyarusange_ml_data

test_nya_manuscript$year<-year(test_nya_manuscript$date)
test_nya_manuscript_agg<-test_nya_manuscript[,c(2,22)]
test_nya_manuscript_agg<-aggregate(.~year,data = test_nya_manuscript_agg,sum,na.rm=T)
mean(test_nya_manuscript_agg$rf_mm)


test_kwi_manuscript<-kwiterambere_ml_data

test_kwi_manuscript$year<-year(test_kwi_manuscript$date)
test_kwi_manuscript_agg<-test_kwi_manuscript[,c(2,22)]
test_kwi_manuscript_agg<-aggregate(.~year,data = test_kwi_manuscript_agg,sum,na.rm=T)
mean(test_kwi_manuscript_agg$rf_mm)

test_rul_manuscript<-ruliba_timeseries_finalss3

test_rul_manuscript$year<-year(test_rul_manuscript$date)
test_rul_manuscript_agg<-test_rul_manuscript[,c(2,19)]
test_rul_manuscript_agg<-aggregate(.~year,data = test_rul_manuscript_agg,sum,na.rm=T)
mean(test_rul_manuscript_agg$rf_mm)


## additional analysis- runoff
test_muh_manuscript<-muhembe_ml_data
library(lubridate)
test_muh_manuscript$year<-year(test_muh_manuscript$date)
test_muh_manuscript_agg<-test_muh_manuscript[,c(2,22)]
test_muh_manuscript_agg<-aggregate(.~year,data = test_muh_manuscript_agg,sum,na.rm=T)
mean(test_muh_manuscript_agg$rf_mm)

test_nta_manuscript<-ntaruka_ml_data

test_nta_manuscript$year<-year(test_nta_manuscript$date)
test_nta_manuscript_agg<-test_nta_manuscript[,c(2,22)]
test_nta_manuscript_agg<-aggregate(.~year,data = test_nta_manuscript_agg,sum,na.rm=T)
mean(test_nta_manuscript_agg$rf_mm)

test_rug_manuscript<-rugeshi_ml_data

test_rug_manuscript$year<-year(test_rug_manuscript$date)
test_rug_manuscript_agg<-test_rug_manuscript[,c(2,22)]
test_rug_manuscript_agg<-aggregate(.~year,data = test_rug_manuscript_agg,sum,na.rm=T)
mean(test_rug_manuscript_agg$rf_mm)

test_uwu_manuscript<-uwumugeti_ml_data

test_uwu_manuscript$year<-year(test_uwu_manuscript$date)
test_uwu_manuscript_agg<-test_uwu_manuscript[,c(2,22)]
test_uwu_manuscript_agg<-aggregate(.~year,data = test_uwu_manuscript_agg,sum,na.rm=T)
mean(test_uwu_manuscript_agg$rf_mm)

test_mut_manuscript<-mutiwingoma_ml_data

test_mut_manuscript$year<-year(test_mut_manuscript$date)
test_mut_manuscript_agg<-test_mut_manuscript[,c(2,22)]
test_mut_manuscript_agg<-aggregate(.~year,data = test_mut_manuscript_agg,sum,na.rm=T)
mean(test_mut_manuscript_agg$rf_mm)


test_nya_manuscript<-nyarusange_ml_data

test_nya_manuscript$year<-year(test_nya_manuscript$date)
test_nya_manuscript_agg<-test_nya_manuscript[,c(2,22)]
test_nya_manuscript_agg<-aggregate(.~year,data = test_nya_manuscript_agg,sum,na.rm=T)
mean(test_nya_manuscript_agg$rf_mm)


test_kwi_manuscript<-kwiterambere_ml_data

test_kwi_manuscript$year<-year(test_kwi_manuscript$date)
test_kwi_manuscript_agg<-test_kwi_manuscript[,c(2,22)]
test_kwi_manuscript_agg<-aggregate(.~year,data = test_kwi_manuscript_agg,sum,na.rm=T)
mean(test_kwi_manuscript_agg$rf_mm)

test_rul_manuscript<-ruliba_timeseries_finalss3

test_rul_manuscript$year<-year(test_rul_manuscript$date)
test_rul_manuscript_agg<-test_rul_manuscript[,c(2,19)]
test_rul_manuscript_agg<-aggregate(.~year,data = test_rul_manuscript_agg,sum,na.rm=T)
mean(test_rul_manuscript_agg$rf_mm)


# Load required packages
library(sf)

#raster elevation stats
tr_slope<-terra::terrain(elevation,'slope', units = 'degrees', neighbors = 8, filename = 'RR_2020_Slope.grd', overwrite = T)

bridge_buffers_slope<-extract(tr_slope,bridge_buffers,mean)
ruliba_slope<-extract(tr_slope,ruliba_catchment,mean,na.rm=T)

rownames(bridge_buffers_slope)<-bridge_buffers@data$Site_Name


###plot vic simulations- calibration vs validation
vic_sim<-read.csv("../vic/output/final_vic_calibration_parameters/VIC_sim_v_Obs.csv")
vic_sim_melt<-reshape2::melt(vic_sim,id.vars = "Date",variable.name = "Data",value.name = "Q")

ggplot(vic_sim_melt,aes(x=Date,y = Q,group=Data,color=Data))+geom_line()+geom_vline(xintercept = 1461)

