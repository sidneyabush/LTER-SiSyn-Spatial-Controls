#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree"))
#install.packages("tree")
#install.packages("RRF")
#install.packages("arsenal")
require(remotes)
require(RRF)
require(caret)
require(randomForest)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
require(dplyr)
require(plot.matrix)
require(reshape2)
require(rcartocolor)
require(arsenal)
require(googledrive)
require(data.table)
require(pdp)


#function to see variable importance by regime
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

####function to test ntree - change the internal function to reflect the RF model that you are using
test_numtree_average <- function(ntree_list) {
  OOB<-list()
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(med_si~.,
                           data=drivers_df, importance=TRUE, proximity=TRUE, 
                           ntree=ntree_list[[i]])
    # OOB[[i]]<-rf_model$err.rate[,1]
    OOB[[i]]<-rf_model$mse
    
    
  }
  return(OOB)
}

#read in drivers data
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")


drivers_url<-"https://drive.google.com/file/d/102LAmZFHOg64kMvorybxMiy9vCrFF1Cd/view?usp=drive_link"

file_get<-drive_get(as_id(drivers_url))

drive_download(file_get$drive_resource, overwrite = T)

drivers<-read.csv("AllDrivers_Harmonized_20231129.csv")

#remove any duplicated rows
drivers<-drivers[!duplicated(drivers$Stream_ID),]

# #read in cluster data for average cluster, modal cluster, number of clusters
# si_clust<-read.csv("ClusterAllMetadata.csv")
# 
# #merge cluster and drivers data
# si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)
# 
# si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]
# 
# drivers<-merge(drivers, si_clust, by=c("Stream_ID"))

#remove variables not interested in ever including
drivers<-dplyr::select(drivers, -c("cycle1","X","X.1","ClimateZ","Latitude","Longitude","LTER","rndCoord.lat",
                                   "rndCoord.lon"))

#look at distribution of NA across columns
#sapply(drivers, function(x) sum(is.na(x)))

#remove sites w NA
drivers<-drivers[complete.cases(drivers$npp),]

#turn centroid name to factor
# drivers$Centroid_Name<-as.factor(drivers$Centroid_Name)

## add new driver that is N:P ratios:
drivers$N_P <- drivers$N / drivers$P

#select only features to be included in model
drivers_df<-drivers[,c("med_si","CV_Q","precip","evapotrans","temp","npp","cycle0","q_95","q_5",
                       "prop_area","N","P","Max_Daylength","q_max_day","q_min_day", "major_land",
                       "N_P")]

colnames(drivers_df) =  c('med_si','CV_Q','Precip','Evapotrans','Temp', "NPP", "Green_Up_Day", "Q_95",
                          "Q_5", "Snow_Cover", "N", "P", "Max_Daylength", "Q_max_day", "Q_min_day",
                           "Land_Cover", "N_P")

# Optionally adding in all rock/ land types, not just major
#keep_these_too<-drivers[,colnames(drivers) %like% c("rock|land")]
#drivers_df<-bind_cols(drivers_df, keep_these_too)
#drivers_df[,c(17:31)]<-replace(drivers_df[,c(17:31)], is.na(drivers_df[,c(17:31)]), 0)

#look at correlation between variables
#driver_cor<-cor(drivers_df[,c(2:9,12:15)])
#corrplot(driver_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

#original model, all parameters
#test number of trees 100-1000
OOB_list<-test_numtree_average(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

OOB_df<-as.data.frame(unlist(OOB_list))

OOB_num<-list()

for (i in 1:length(tre_list)) {
  
  OOB_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

OOB_df$tree_num<-unlist(OOB_num)

OOB_mean<-OOB_df %>% group_by(tree_num) %>%
  summarise(mean_oob=mean(`unlist(OOB_list)`))

#visualize and select number of trees that gives the minimum OOB error
ggplot(OOB_mean, aes(tree_num, mean_oob))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#tune mtry based on optimized ntree
set.seed(123)
## need to adjust these column values when removing variables
tuneRF(drivers_df[,c(2:17)], drivers_df[,1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

#run intial RF using tuned parameters
set.seed(123)
rf_model1<-randomForest(med_si~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=2000,mtry=5)

#visualize output
rf_model1

#visualize variable importance
randomForest::varImpPlot(rf_model1)

#set seeds for RFE
size=ncol(drivers_df)-1
#this is number of cross validation repeats and folds
cv_repeats = 5
cv_number = 5

total_repeats<-(cv_repeats*cv_number)+1

seeds <- vector(mode = "list", length = total_repeats)
for (i in 1:25) {
  
  seeds[[i]]<-rep(123, size)
  
}

seeds[[total_repeats]]<-123

#set control functions for recursive feature elimination on RF
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = cv_repeats, # number of repeats
                      number = cv_number,
                      seeds = seeds,
                      verbose = TRUE) # number of folds

#divide data into predictor variables (y) and response variables (x)
x<-drivers_df[,!(colnames(drivers_df)=="med_si")]

y<-drivers_df$med_si

#split into testing and training data
# inTrain <- createDataPartition(y, p = .70, list = FALSE)[,1]
# 
# x_train <- x[ inTrain, ]
# x_test  <- x[-inTrain, ]
# 
# y_train <- y[ inTrain]
# y_test  <- y[-inTrain]

#run RFE, this will take a bit
#we are allowing the number of variables retained to range from 1 to all of them here
#to change that changes input into the "sizes" variable
set.seed(123)
result_rfe <- rfe(x = x, 
                  y = y, 
                  sizes = c(1:size),
                  rfeControl = control)

#print rfe results
result_rfe

#Put selected features into variable
new_rf_input<-paste(predictors(result_rfe), collapse = "+")

#Format those features into a formula to put in the optimized random forest model
rf_formula<-formula(paste("med_si~", new_rf_input))

#retune RF after RFE optimization
test_numtree_optimized <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]])
    # OOB[[i]]<-rf_model$err.rate[,1]
    OOB[[i]]<-rf_model$mse
    
    
  }
  
  return(OOB)
  
}


OOB_list<-test_numtree_optimized(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

OOB_df<-as.data.frame(unlist(OOB_list))

OOB_num<-list()

for (i in 1:length(tre_list)) {
  
  OOB_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

OOB_df$tree_num<-unlist(OOB_num)

OOB_mean<-OOB_df %>% group_by(tree_num) %>%
  summarise(mean_oob=mean(`unlist(OOB_list)`))

#visualize and select number of trees that gives the minimum OOB error
ggplot(OOB_mean, aes(tree_num, mean_oob))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#retune mtry
kept_drivers<-drivers_df[,c(colnames(drivers_df) %in% predictors(result_rfe))]

set.seed(123)
tuneRF(kept_drivers, drivers_df[,1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

#run optimized random forest model, with retuned ntree and mtry parameters
set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=2000, mtry=3)


rf_model2

randomForest::varImpPlot(rf_model2)

lm_plot <- plot(rf_model2$predicted, drivers_df$med_si, xlab="Predicted", ylab="Observed", 
                main= "Optimized RF Model - Removed Lithology") + abline(a=0, b=1, col="red") + theme(text = element_text(size=20))
legend("topleft", bty = "n", legend = paste("R2=",format(mean(rf_model2$rsq), digits=3))) 
legend("topright", bty="n", legend = paste("MSE=", format(mean(rf_model2$mse), digits=3)))

#playing around w partial dependence plots
par.Long <- partial(rf_model2, pred.var = "P")
partial_plot <-autoplot(par.Long, contour = T) + theme_bw() + theme(text = element_text(size=20))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "Green_Up_Day")
partial_plot <-autoplot(par.Long, contour = T) + theme_bw() + theme(text = element_text(size=20))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "Max_Daylength")
partial_plot <-autoplot(par.Long, contour = T) + theme_bw() + theme(text = element_text(size=20))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "N")
partial_plot <-autoplot(par.Long, contour = T) + theme_bw() + theme(text = element_text(size=20))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "Q_95")
partial_plot <-autoplot(par.Long, contour = T) + theme_bw() + theme(text = element_text(size=20))
print(partial_plot)

setdiff(colnames(drivers_df), predictors(result_rfe))

#plot confusion matrix
# df<-as.data.frame(rf_model2$confusion)
# 
# df_new<-data.frame(t(apply(df[,c(1:5)],1, function(x) x/sum(x))))
# colnames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
# rownames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
# df_new$cluster<-rownames(df_new)
# 
# df_new_melt<-melt(df_new, id.vars = "cluster")
# df_new_melt$same<-ifelse(df_new_melt$cluster==df_new_melt$variable, "yes","no")
# 
# #visualize matrix
# ggplot(df_new_melt, aes(variable, cluster))+geom_raster(aes(fill=same))+
#   scale_fill_manual(values=c("yes"="forestgreen", "no"="salmon"))+
#   geom_text(aes(label=round(value, 2)), size=10)+theme_bw()+labs(x="",y="",fill="")+
#   theme(legend.position = "null", text = element_text(size = 15))

importance_df<-data.frame(rf_model2$importance)
importance_df$driver<-rownames(importance_df)

vars_order<-importance_df %>%
  dplyr::arrange(desc(MeanDecreaseAccuracy), driver) %>%
  dplyr::select(driver)

importance_melt<-melt(importance_df[,-7], id.vars = "driver")

importance_melt$driver<-factor(importance_melt$driver, levels = vars_order$driver)

ggplot(importance_melt, aes(variable, driver))+geom_raster(aes(fill=value))+
  scale_fill_gradient(low="grey90", high="red")+theme_bw()
#labs(x="", y="Variable",fill="Mean Decrease Accuracy")+
#theme(text = element_text(size=15))+scale_y_discrete(limits=rev)
# scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS","Overall Model"))

###plot most important variables across clusters
# centroid_abb<-as.data.frame(c("Fall Peak"="FP", "Fall Trough"="FT", "Spring Trough"="ST", "Spring Trough, Fall Peak"="STFP", 
#                               "Spring Trough, Variable Summer"="STVS"))
# colnames(centroid_abb)<-"abb"
# centroid_abb$Centroid_Name<-rownames(centroid_abb)
# 
# import_factors<-drivers_df[,c("Centroid_Name","prop_area", "Max_Daylength", "temp",
#                               "cycle0", "evapotrans","CV_Q")]
#import_factors$q_95<-log(import_factors$q_95)
#colnames(import_factors)<-c("Centroid_Name", "Max Snow Extent (proportion of WS)", "Watershed Snow Days (days)", "Latitude (degrees)",
#                           "Temperature (C)", "Green Up Day (DOY)","CV Si")
# import_factors_melt<-melt(import_factors, id.vars = "Centroid_Name")
# import_factors_melt<-merge(import_factors_melt, centroid_abb, by="Centroid_Name")
# 
# pdf("MostImportVars.pdf", width = 14, height = 9, family="Times")
# 
# ggplot(import_factors_melt, aes(abb, value))+geom_boxplot(aes(fill=abb), alpha=0.5, outlier.shape = NA)+facet_wrap(~variable, scales = "free")+theme_bw()+
#   theme(text = element_text(size = 20))+labs(x="", y="Driver Value")+
#   geom_jitter(aes(col=abb))+
#   scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
#   theme(axis.text.x = element_blank())+labs(fill="Cluster", color="Cluster")

dev.off()



