#modeling with VIC like train/test splitting

##streamflow models using h2o package
#GLM, RF, GBM, XGBoost, stacked
library(mlbench)
library(caret)
library(e1071)
library(randomForest)
library(rsample)
# install.packages("h2o")
library(h2o)
library(tidyverse)
library(hydroGOF)
library(hydroTSM)

## data prepare
ruliba_for_h2o<-merge(ruliba_timeseries_pred,ruliba_discharge,by="date")
rul_disc_forh2o<-read.csv("C:/vic/h2o/ruliba_calibration_telemtry_MLflowcorrected_h2o.csv")
ruliba_for_h2o_fn<-merge(ruliba_for_h2o,rul_disc_forh2o,by="date")
ruliba_for_h2o_fn<-ruliba_for_h2o_fn %>% fill(sm,.direction = "up")
ruliba_for_h2o_fn<-ruliba_for_h2o_fn %>% fill(sm.lag1d,.direction = "up")
ruliba_for_h2o_fn<-ruliba_for_h2o_fn %>% fill(sm.lag2d,.direction = "up")
ruliba_for_h2o_fn<-ruliba_for_h2o_fn %>% fill(sm.lag3d,.direction = "up")
write.csv(ruliba_for_h2o_fn,"C:/vic/h2o/ruliba_for_h2o_fn2.csv",row.names = F)
sum(is.na(ruliba_for_h2o_fn))#check for NAs

##Additional h2o machine learning prediction of streamflow for B2P work-- matching with vic calibration and validation set

##ML Feature selection
# install and load the h2o package
# library(h2o)
length(ruliba_for_h2o_fn$date) #1675


# start an h2o cluster
h2o.init()

# import the data into h2o
# data <- h2o.importFile("C:/Users/DENNIS/MCGE Dropbox/Denis Macharia/B2P/ML/csv/ml_final_data_v7.csv")
data<-ruliba_for_h2o_fn[,-1]

# split the data into training and test sets
train <- data[1:1461,]
test <- data[1462:1675,]

train2<-as.h2o(train)
test2<-as.h2o(test)

# specify the response and predictor variables
response <- "discharge"
predictors <- setdiff(names(data), response)

# build a gradient boosting machine model
model <- h2o.gbm(x = predictors, y = response, training_frame = train2, nfolds=10, ntrees = 500, keep_cross_validation_models = TRUE, keep_cross_validation_predictions = TRUE, seed = 5)

# define the hyperparameter search space
hyper_parameters <- list(
  ntrees = seq(from = 50, to = 500, by = 50),
  learn_rate = seq(from = 0.01, to = 0.5, by = 0.1)
)

# perform hyperparameter tuning using grid search
# grid <- h2o.grid(
#   algorithm = "gbm",
#   grid_id = "gbm_grid",
#   x = predictors,
#   y = response,
#   hyper_params = hyper_parameters,
#   training_frame = train,
#   seed = 5
# )

grid2 <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_grid2",
  x = predictors,
  y = response,
  hyper_params = hyper_parameters,
  training_frame = train2,
  seed = 5
)
# print the grid search results
print(grid2)

# select the model with the best hyperparameters
best_model2 <- h2o.getModel(grid2@model_ids[[which.min(grid2@summary_table$residual_deviance)]])

# make predictions on the test set using the new model
predictions2 <- h2o.predict(best_model2, test2)

# evaluate the model performance
performance2 <- h2o.performance(best_model2, test2)

# print the mean absolute error
print(performance2@metrics$mae)

h2o.exportFile(predictions2,"C:/vic/h2o/h2o.vic.predict_final.csv")
h2o.exportFile(test2,"C:/vic/h2o/h2o.vic.testdata_final.csv")

# library(hydroGOF)
# library(hydroTSM)

h2o.vicplot<-read.csv("C:/vic/h2o/h2o.vic.predict_final.csv")
ggof(h2o.vicplot$predict,h2o.vicplot$discharge,dates = h2o.vicplot$date,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"))

h2o.vicplot_melt<-h2o.vicplot
names(h2o.vicplot_melt)<-c("date","gbm","obs")

h2o.vicplot_melt2<-reshape2::melt(h2o.vicplot_melt,id.vars="date",variable.name="model")
h2o.vicplot_melt2$date2<-as.Date(h2o.vicplot_melt2$date,format="%Y-%m-%d")

ggplot(h2o.vicplot_melt2, aes(x = date2, y = value, group=model, colour=model)) +
  geom_line()+
  geom_point(size=1)+
  xlab("Date") +
  ylab("Q [m3/s]") +
  scale_color_manual(values =c("royalblue3","Black"))+theme_bw()+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.75, 0.80),
        legend.direction="vertical",legend.key.height = unit(1,'cm'),legend.key.width = unit(1,'cm'))+
  scale_x_date(date_labels="%m/%Y")

obs<-h2o.vicplot$discharge
gbm<-h2o.vicplot$predict
ddate<-seq(as.Date("2022-03-01"),as.Date("2022-09-30"),by="days")
ggof(gbm,obs,dates=ddate,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "GBM predictions matched to VIC validation set")


####PLOTTING VIC SIMULATIONS FROM VIC CALIBRATED MODEL and GBM model for the period 2022-03-01:2022-09-30
vic_sim<-read.csv("C:/vic/IMERGC/routout/sim2022/vic_sim2022_plot.csv")
ggof(vic_sim$VIC,vic_sim$Obs,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC validation")
ggof(vic_sim$GBM,vic_sim$Obs,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC validation")

vic_gbm_sim<-vic_sim
vic_gbm_sim_melt<-reshape2::melt(vic_gbm_sim,id.vars="date",variable.name="model")
vic_gbm_sim_melt$date2<-as.Date(vic_gbm_sim_melt$date,format="%Y-%m-%d")

library(ggplot2)

ggplot(vic_gbm_sim_melt, aes(x = date2, y = value, group=model, colour=model)) +
  geom_line()+
  geom_point(size=1)+
  xlab("Date") +
  ylab("Q [m3/s]") +
  scale_color_manual(values =c("royalblue3","green","Black"))+theme_bw()+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.75, 0.80),
        legend.direction="horizontal",legend.key.height = unit(1,'cm'),legend.key.width = unit(1,'cm'))+
  scale_x_date(date_labels="%m/%Y")

##vic simulation calibration vs validation

vic_sim<-read.csv("../vic/output/final_vic_calibration_parameters/VIC_sim_v_Obs.csv")
vic_sim_melt<-reshape2::melt(vic_sim,id.vars = "Date",variable.name = "Data",value.name = "Q")

vic_sim_melt$date2<-as.Date(vic_sim_melt$Date,format="%Y-%m-%d")

ggplot(vic_sim_melt,aes(x=date2,y = Q,group=Data,color=Data))+geom_line()+
  geom_point(size=0.25)+
  xlab("Date") +
  ylab("Q [m3/s]") +
  scale_color_manual(values =c("royalblue3","Black"))+theme_bw()+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.25, 0.80),
        legend.direction="vertical",legend.key.height = unit(1,'cm'),legend.key.width = unit(1,'cm'))+
  scale_x_date(date_labels="%d/%m/%Y")+geom_vline(xintercept = as.numeric(vic_sim_melt$date2[1461]), linetype=4, colour="green",linewidth=1)

##error metrics
vic_sim_cal=vic_sim
# vic_sim_cal$date2<-as.Date(vic_sim_cal$Date,format="%Y-%m-%d")
vic_sim_cal<-vic_sim_cal[vic_sim_cal$Date<"2022-03-01",]

ggof(vic_sim_cal$VIC,vic_sim_cal$Obs,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC cal_val_plot")


#ML
glm_train_pred<-read.csv("./discharge/predicted/h2o/ruliba_glm_train_pred.csv")
ggof(glm_train_pred$glm_predict,glm_train_pred$obs_trainsample,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC cal_val_plot")

#gbm
gbm_train_pred<-read.csv("./discharge/predicted/h2o/ruliba_gbm_trainpred.csv")
ggof(gbm_train_pred$gbm_predict,gbm_train_pred$obs_trainsample,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC cal_val_plot")

#rfm
rfm_train_pred<-read.csv("./discharge/predicted/h2o/ruliba_rfm_trainpred.csv")
ggof(rfm_train_pred$rfm_predict,rfm_train_pred$obs_trainsample,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"),main = "VIC cal_val_plot")
