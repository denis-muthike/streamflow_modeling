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

setwd("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML")
#data
h2o.init()
set.seed(5)
ruliba_data_h2o<-ruliba_ml_data

ruliba_data_h2o<-as.h2o(ruliba_data_h2o[,-1])

data_split<-h2o.splitFrame(data=ruliba_data_h2o,ratios = 0.75)
data_train<-data_split[[1]]
data_test<-data_split[[2]]

h2o.exportFile(data_test,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_data_test_v1.csv")
h2o.exportFile(data_train,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_data_train_v1.csv")


h2o.table(data_train$discharge)
h2o.table(data_test$discharge)

Y="discharge"
X=c("rf_mm","rf.lag1d","rf.lag2d","rf.lag3d","tmin" ,"tmin.lag1d","tmin.lag2d","tmin.lag3d","tmax","tmax.lag1d",
    "tmax.lag2d","tmax.lag3d","sm","sm.lag1d","sm.lag2d","sm.lag3d","ndvi","lai")

#GLM

ruliba_glm<-h2o.glm(training_frame = data_train,x=X,y=Y,family = "gaussian",nfolds=10,seed=5,keep_cross_validation_predictions=TRUE)
summary(ruliba_glm)

#predict
ruliba_glm_pred<-h2o.predict(object = ruliba_glm,newdata = data_test)
summary(ruliba_glm_pred)
summary(data_test$discharge)
data_test2<-data_test
data_test2$glm_pred<-ruliba_glm_pred
data_test2<-as.data.frame(data_test2)
cor(data_test2$discharge,data_test2$glm_pred)
NSE(data_test2$discharge,data_test2$glm_pred)

h2o.exportFile(ruliba_glm_pred,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_glm_pred.csv")

h2o.exportFile(ruliba_glm_trainpred,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_glm_train_pred.csv")

#GBM
ruliba_gbm<-h2o.gbm(y=Y,x=X,training_frame = data_train,ntrees=500,nfolds=10,distribution = "gaussian",seed=5,keep_cross_validation_predictions=TRUE)
summary(ruliba_gbm)

#predict
ruliba_gbm_pred<-h2o.predict(object = ruliba_gbm,newdata = data_test)
summary(ruliba_gbm_pred)
ruliba_gbm_pred2<-as.data.frame(ruliba_gbm_pred)
data_test2$gbm_pred<-ruliba_gbm_pred2
cor(data_test2$discharge,data_test2$gbm_pred)
NSE.data.frame(data_test2$discharge,data_test2$gbm_pred)

h2o.exportFile(ruliba_gbm_pred,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_gbm_pred.csv")

#randomforest
ruliba_rfm <- h2o.randomForest(x = X,
                          y = Y,
                          training_frame = data_train,
                          nfolds = 10,
                          ntrees = 500,
                          seed = 5,
                          keep_cross_validation_predictions=TRUE)

summary(ruliba_rfm)

#predict
ruliba_rfm_pred<-h2o.predict(object = ruliba_rfm,newdata = data_test)
summary(ruliba_rfm_pred)
ruliba_rfm_pred2<-as.data.frame(ruliba_rfm_pred)
data_test2$rfm_pred<-ruliba_rfm_pred2
cor(data_test2$discharge,data_test2$rfm_pred)
NSE.data.frame(data_test2$discharge,data_test2$rfm_pred)

h2o.exportFile(ruliba_rfm_pred,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_rfm_pred.csv")

# Train a stacked random forest ensemble using the GBM, RF and GLM above
ruliba_ens1 <- h2o.stackedEnsemble(x = X,
                                   y = Y,
                                   metalearner_algorithm="drf",
                                   training_frame = data_train,
                                   base_models = list(ruliba_gbm, ruliba_rfm,ruliba_glm),metalearner_nfolds=10)

ruliba_ens2 <- h2o.stackedEnsemble(x = X,
                                   y = Y,
                                   metalearner_algorithm="gbm",
                                   training_frame = data_train,
                                   base_models = list(ruliba_gbm, ruliba_rfm,ruliba_glm),metalearner_nfolds=10)

# Eval ensemble performance on a test set
perf_ruliba_ens1 <- h2o.performance(ruliba_ens1, newdata = data_test)
perf_ruliba_ens2 <- h2o.performance(ruliba_ens2, newdata = data_test)


#R^2 on training and testing data
#ensemble
r2_ruliba_ens1<-h2o.r2(ruliba_ens1)#0.95 drf
r2_ruliba_ens2<-h2o.r2(ruliba_ens2)#0.96 gbm

#testing
r2_perf_ruliba_ens1<-h2o.r2(perf_ruliba_ens1)#0.75 drf
r2_perf_ruliba_ens2<-h2o.r2(perf_ruliba_ens2)#0.75 gbm


#individual models
r2_ruliba_glm<-h2o.r2(ruliba_glm)#0.56
r2_ruliba_gbm<-h2o.r2(ruliba_gbm)#0.99
r2_ruliba_rfm<-h2o.r2(ruliba_rfm)#0.73

#testing
perf_ruliba_glm_test<-h2o.performance(ruliba_glm,newdata = data_test)
perf_ruliba_gbm_test<-h2o.performance(ruliba_gbm,newdata = data_test)
perf_ruliba_rfm_test<-h2o.performance(ruliba_rfm,newdata = data_test)

perf_train_glm<-h2o.performance(ruliba_glm,newdata = data_train)

r2_ruliba_glm_test<-h2o.r2(perf_ruliba_glm_test)#0.53
r2_ruliba_gbm_test<-h2o.r2(perf_ruliba_gbm_test)#0.76
r2_ruliba_rfm_test<-h2o.r2(perf_ruliba_rfm_test)#0.73

#metrics
metrics_models<-read.csv("./discharge/predicted/h2o/for_plots.csv") #from exported testing series

cor.test(metrics_models$glm,metrics_models$obs) #0.73
cor.test(metrics_models$rfm,metrics_models$obs) #0.86
cor.test(metrics_models$gbm,metrics_models$obs) #0.88

mae(metrics_models$glm,metrics_models$obs) #20.45
mae(metrics_models$rfm,metrics_models$obs) #14.61
mae(metrics_models$gbm,metrics_models$obs) #13.24

NSE(metrics_models$glm,metrics_models$obs) #0.53
NSE(metrics_models$rfm,metrics_models$obs) #0.73
NSE(metrics_models$gbm,metrics_models$obs) #0.76

rmse(metrics_models$glm,metrics_models$obs) #26.8
rmse(metrics_models$rfm,metrics_models$obs) #20.42
rmse(metrics_models$gbm,metrics_models$obs) #18.91

plot2(metrics_models$gbm,metrics_models$obs,plot.type = "single")

m1p<-ggplot(metrics_models, aes(x=obs,y=glm))+
  geom_point(alpha=0.5)+
  labs(x="",y="Predicted Q [m3/s]")+
  geom_smooth(method=lm)+theme_bw()

m2p<-ggplot(metrics_models, aes(x=obs,y=rfm))+
  geom_point(alpha=0.5)+
  labs(x="Observed Q [m3/s]",y="")+
  geom_smooth(method=lm)+theme_bw()

m3p<-ggplot(metrics_models, aes(x=obs,y=gbm))+
  geom_point(alpha=0.5)+
  labs(x="Observed Q [m3/s]",y="Predicted Q [m3/s]")+
  geom_smooth(method=lm)+theme_bw()

metrics_models_melt<-reshape2::melt(metrics_models,id.vars="date_index",variable.name="Model")

m4p<-ggplot(metrics_models_melt, aes(x = date_index, y = value, group=Model, colour=Model)) +
  geom_line()+
  #geom_point(size=0.5)+
  xlab("Date_index") +
  ylab("Q [m3/s]") +
  scale_color_manual(values =c("Black","gray42","Green","royalblue3"))+theme_bw()+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.18, 0.82),
            legend.direction="vertical",legend.key.height = unit(0.2,'cm'),legend.key.width = unit(0.3,'cm'))

library(gridExtra)
library(grid)
# install.packages("ggpubr")
library(ggpubr)


grid.arrange(m1p,m2p,m3p,m4p,
          ncol = 2, nrow = 2)

h1p<-h2o.varimp_plot(ruliba_gbm,10)
h2p<-h2o.varimp_plot(ruliba_glm,10)
h3p<-h2o.varimp_plot(ruliba_rfm,10)

grid.arrange(h1p,h2p,h3p,
             ncol = 1, nrow = 3)

###checking for differences in performance without soil moisture variables
ruliba_data_h2o_wosm<-ruliba_ml_data[,c(2:13,18:20)]
ruliba_data_h2o_wosm<-as.h2o(ruliba_data_h2o_wosm)

data_split_wosm<-h2o.splitFrame(data=ruliba_data_h2o_wosm,ratios = 0.75)
data_train_wosm<-data_split_wosm[[1]]
data_test_wosm<-data_split_wosm[[2]]

X1=c("rf_mm","rf.lag1d","rf.lag2d","rf.lag3d","tmin" ,"tmin.lag1d","tmin.lag2d","tmin.lag3d","tmax","tmax.lag1d",
    "tmax.lag2d","tmax.lag3d","ndvi","lai")

#GBM
ruliba_gbm_wosm<-h2o.gbm(y=Y,x=X1,training_frame = data_train_wosm,ntrees=500,nfolds=10,distribution = "gaussian",seed=5,keep_cross_validation_predictions=TRUE)
summary(ruliba_gbm_wosm)

#predict
ruliba_gbm_pred_wosm<-h2o.predict(object = ruliba_gbm_wosm,newdata = data_test_wosm)
summary(ruliba_gbm_pred_wosm)
# ruliba_gbm_pred2_wosm<-as.data.frame(ruliba_gbm_pred_wosm)

h2o.exportFile(ruliba_gbm_pred_wosm,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_gbm_pred_wosm.csv")
h2o.exportFile(data_test_wosm,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/data_test_wosm.csv")


val_wosm<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_gbm_pred_wosm.csv")
val_wosm_melt<-reshape2::melt(val_wosm,id.vars="date_index",variable.name="model")
ggof(val_wosm$glm_wo_sm,val_wosm$obs,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"))



##checking for differences in performance without soil moisture, lai and ndvi variables
set.seed(5)
ruliba_data_h2o_wosmlndvi<-ruliba_ml_data[,c(2:13,20)]
ruliba_data_h2o_wosmlndvi<-as.h2o(ruliba_data_h2o_wosmlndvi)

data_split_wosmlndvi<-h2o.splitFrame(data=ruliba_data_h2o_wosmlndvi,ratios = 0.75)
data_train_wosmlndvi<-data_split_wosmlndvi[[1]]
data_test_wosmlndvi<-data_split_wosmlndvi[[2]]

X2=c("rf_mm","rf.lag1d","rf.lag2d","rf.lag3d","tmin" ,"tmin.lag1d","tmin.lag2d","tmin.lag3d","tmax","tmax.lag1d",
     "tmax.lag2d","tmax.lag3d")

#GBM
ruliba_gbm_wosmlndvi<-h2o.gbm(y=Y,x=X2,training_frame = data_train_wosmlndvi,ntrees=500,distribution = "gaussian",nfolds=10,seed=5,keep_cross_validation_predictions=TRUE)
summary(ruliba_gbm_wosmlndvi)

#predict
ruliba_gbm_pred_wosmlndvi<-h2o.predict(object = ruliba_gbm_wosmlndvi,newdata = data_test_wosmlndvi)
summary(ruliba_gbm_pred_wosmlndvi)
# ruliba_gbm_pred2_wosm<-as.data.frame(ruliba_gbm_pred_wosm)

h2o.exportFile(ruliba_gbm_pred_wosmlndvi,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_gbm_pred_wosmlndvi.csv")
h2o.exportFile(data_test_wosmlndvi,"C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/data_test_wosmlndvi.csv")


val_wosmlndvi<-read.csv("C:/Users/demu4180/MCGE Dropbox/Denis Muthike/B2P/ML/discharge/predicted/h2o/ruliba_gbm_pred_wosmlndvi.csv")
val_wosm_meltwsmlndvi<-reshape2::melt(val_wosmlndvi,id.vars="date_index",variable.name="model")
ggof(val_wosmlndvi$gbm,val_wosmlndvi$obs,gofs = c("r","NSE","PBIAS","RMSE","KGE","MAE"))
