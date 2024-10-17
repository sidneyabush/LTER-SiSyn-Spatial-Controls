#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree", "pdp))
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
library(pdp)
library(dplyr)
library(data.table)
require(pdp)


#this code makes importance plots
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized_20231129.csv")
drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

## Also add in the climate classifications: 
Upscaled_KG <- read.csv("KG_Clim_Name.csv")
names(Upscaled_KG)[names(Upscaled_KG) == 'Name'] <- 'Up_KG'

## Merge by climate zone so we have a new column that gives the KG up-scaled classifications
drivers <- merge(drivers, Upscaled_KG, by = "ClimateZ")
drivers$Up_KG = as.factor(drivers$Up_KG)

## add new driver that is N:P ratios:
drivers$N_P <- drivers$N / drivers$P
drivers[,c(29:56)]<-replace(drivers[,c(29:56)], is.na(drivers[,c(29:56)]), 0)

drivers_cropped <- drivers[,c("med_si","min_Q", "max_Q", "q_95", "q_5","CV_Q", "q_max_day", "q_min_day", "drainSqKm", 
                              "prop_area","precip", "evapotrans", "temp", "npp", "cycle0" , "major_land","P",
                               "N", "elevation_mean_m", "Max_Daylength", "N_P")]

# drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped$num_days),]
drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped$drainSqKm),]
drivers_cropped<- drivers_cropped[,!c(colnames(drivers_cropped) %like% ".1")]

####test ntree ####
#this only tests on the base model, which includes all parameters. we might expect things to change once we start 
#removing variables
test_numtree_average <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    print(ntree_list[i])
    
    set.seed(123)
    
    rf_model<-randomForest(med_si~.,data=drivers_cropped, importance=TRUE, 
                           proximity=TRUE, ntree=ntree_list[i], mtry=6)
    
    OOB[[i]]<-mean(rf_model$rsq)
    
  }
  
  return(OOB)
  
}

### THINGS TO ADD INTO RF FUNCTION:
#tune mtry based on optimized ntree
set.seed(123)
tuneRF(drivers_cropped[,c(2:21)], drivers_cropped[,1], ntreeTry = 100, stepFactor = 1, improve = 0.5, plot = FALSE)

#set seeds for RFE
size=ncol(drivers_cropped)-1
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
x<-drivers_cropped[,!(colnames(drivers_cropped)=="med_si")]

y<-drivers_cropped$med_si

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
rf_formula<-formula(paste("Centroid_Name~", new_rf_input))

#retune RF after RFE optimization
test_numtree_optimized <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]])
    OOB[[i]]<-rf_model$err.rate[,1]
    
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
tuneRF(kept_drivers, drivers_df[,1], ntreeTry = 500, stepFactor = 1, improve = 0.5, plot = FALSE)

#run optimized random forest model, with retuned ntree and mtry parameters
set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=500, mtry=4, sampsize=c(30,30,30,30,30))


rf_model2

randomForest::varImpPlot(rf_model2)

setdiff(colnames(drivers_df), predictors(result_rfe))

### This is for creating a plot
importance_df<-data.frame(rf_model2$importance)
importance_df$driver<-rownames(importance_df)

vars_order<-importance_df %>%
  dplyr::arrange(desc(MeanDecreaseAccuracy), driver) %>%
  dplyr::select(driver)

importance_melt<-melt(importance_df[,-7], id.vars = "driver")

importance_melt$driver<-factor(importance_melt$driver, levels = vars_order$driver)

ggplot(importance_melt, aes(variable, driver))+geom_raster(aes(fill=value))+
  scale_fill_gradient(low="grey90", high="red")+theme_bw()+labs(x="", y="Variable",fill="Mean Decrease Accuracy")+
  theme(text = element_text(size=15))+scale_y_discrete(limits=rev)+
  scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS","Overall Model"))


## Optimizing # of trees
#test
# set.seed(123)
# 
# ntree_test<-seq(100,5000,by=100)
# 
# #this will take a while to run, depending on how many trees you test
# OOB_list<-test_numtree_average(ntree_list = ntree_test)
# 
# OOB_df<-as.data.frame(unlist(OOB_list))
# 
# OOB_df$tree_num<-ntree_test
# colnames(OOB_df)[1]<-"max_r2"
# 
# #visualize ntree test results
# ggplot(OOB_df, aes(tree_num, max_r2))+geom_point()+geom_line()+
#   theme_classic()+scale_x_continuous(breaks = ntree_test)+theme(text = element_text(size=20))


### model ####
#default model - includes all parameters
# set.seed(123)
# rf_model <- randomForest(med_si~.,
#                         data=drivers_cropped, importance=TRUE, proximity=TRUE, ntree=1600, mtry=6) # sqrt # input values (will change if lumping lith.)
# 
# mean(rf_model$rsq)
# varImpPlot(rf_model)
# 
# #visualize observed vs predicted
# plot(rf_model$predicted, drivers_cropped$med_si, xlab="Predicted Median Si", ylab="Observed Median Si")
# legend("topleft", bty = "n", legend = paste("R2=",format(mean(rf_model$rsq),
#                                                          digits=4)))
# abline(a=0, b=1, col="red")
# 
# ggplot(drivers_cropped, aes(x= Up_KG, y=log(drainSqKm), fill=Up_KG)) + 
#   geom_boxplot() + theme_bw()
# 
# #playing around w partial dependence plots
# par.Long<-partial(rf_model, pred.var = "cycle0")
# autoplot(par.Long, contour = T)


# ## Intial subsetting of Random Forest Plots: 
# ## Drainage Area:
# ## Remove medium drainage area -- just include small and large: 
# # small <- drivers_cropped[drivers_cropped$drainSqKm < 50,]
# # medium <- drivers_cropped[(drivers_cropped$drainSqKm > 50) & (drivers_cropped$drainSqKm < 1000), ]
# # large <- drivers_cropped[(drivers_cropped$drainSqKm >  1000) & (drivers_cropped$drainSqKm < 3000000),]
# 
# small <- drivers_cropped[drivers_cropped$drainSqKm < 1000,]
# large <- drivers_cropped[(drivers_cropped$drainSqKm >  1000) & (drivers_cropped$drainSqKm < 3000000),]
# 
# small <-small[,!c(colnames(small) %like% "drainSqKm")]
# #medium <-medium[,!c(colnames(medium) %like% "drainSqKm")]
# large <-large[,!c(colnames(large) %like% "drainSqKm")]
# 
# Median silica concentrations
low  <- drivers_cropped[drivers_cropped$P < 7,]
high  <- drivers_cropped[drivers_cropped$P > 7,]

# ## Need to remove P info for the  subset: 
low <-low[,!c(colnames(low) %like% "P")]
high <-high[,!c(colnames(high) %like% "P")]



# # low_Si <-low_Si[,!c(colnames(low_Si) %like% "si")]
# # high_Si <-high_Si[,!c(colnames(high_Si) %like% "si")]
# 
# ## Lithology
# pluto_volcanic <- drivers_cropped[drivers_cropped$major_rock %like% "volcanic" | drivers_cropped$major_rock %like% "pluto",]
# metamorphic <- drivers_cropped[drivers_cropped$major_rock %like% "metamorphic",]
# sed_carb <- drivers_cropped[drivers_cropped$major_rock %like% "sed" | drivers_cropped$major_rock %like% "carb",]
# 
# ## Need to remove rock info for the lithology subset: 
# pluto_volcanic <-pluto_volcanic[,!c(colnames(pluto_volcanic) %like% "rock")]
# metamorphic <-metamorphic[,!c(colnames(metamorphic) %like% "rock")]
# sed_carb <-sed_carb[,!c(colnames(sed_carb) %like% "rock")]

# Make model into function: 
#### model ####
#default model - includes all parameters
rf_func <- function(subset_of_drivers, subset_title, subset_variable, subset_variable_name){
  set.seed(123)
  rf_model<-randomForest(med_si~., 
                         data=subset_of_drivers, importance=TRUE, proximity=TRUE, ntree=1600, mtry=6) # sqrt # input values (will change if lumping lith.)
  
  mean <- mean(rf_model$rsq)
  show(mean)
  rankings <- varImpPlot(rf_model, main=subset_title)
  show(rankings)

  #visualize observed vs predicted
  lm_plot <- plot(rf_model$predicted, subset_of_drivers$med_si, xlab="Predicted", ylab="Observed", main= subset_title) + 
    abline(a=0, b=1, col="red")
    legend("topleft", bty = "n", legend = paste("R2=",format(mean(rf_model$rsq), digits=3)))
  

  #playing around w partial dependence plots
  # par.Long <- partial(rf_model, pred.var = "P")
  # partial_plot <-autoplot(par.Long, contour = T)
  # print(partial_plot)

 
}

all_results<- rf_func(drivers_cropped, subset_title = "all sites, no subsets", drivers_cropped$med_si, "Median DSi")

# # Apply function
# small_results<- rf_func(small, subset_title = "catchment size < 1000 km2", small$med_si, subset_variable_name =  "Median DSi")
# #medium_results<- rf_func(medium, subset_title = "catchment size > 50 < 1000 km2", medium$med_si, "Median DSi")
# large_results<- rf_func(large, subset_title = "catchment size > 1000 < 3000000 km2", large$med_si, "Median DSi")
# 
# ## Apply function
# pluto_volcanic_results<- rf_func(pluto_volcanic, subset_title = "Pluto-Volcanic", pluto_volcanic$med_si, "Median Si")
# metamorphic_results<- rf_func(metamorphic, subset_title = "Metamorphic", metamorphic$med_si, "Median Si")
# sed_carb_results<- rf_func(sed_carb, subset_title = "Sedimentary-Carbonate", sed_carb$med_si, "Median Si")

## Apply function:
## LATER: Look at N:P ratios or other productivity metrics
low_results<- rf_func(low, subset_title = "median P < 7 mg", low$P, "Median P")
high_results<- rf_func(high, subset_title = "median P > 7 mg", high$P, "Median P")


#### Make boxplots of distributions for each model subset -----------------
drivers_cropped2 <- drivers[,c("med_si","min_Q", "max_Q", "q_95", "q_5","CV_Q", "q_max_day", "q_min_day", "Latitude", "drainSqKm", "num_days","prop_area","precip",
"evapotrans", "temp", "npp", "cycle0", "major_rock" , "major_land",
"N", "P", "elevation_mean_m", "Max_Daylength", "N_P", "Up_KG")]


drivers_cropped2<-drivers_cropped2[complete.cases(drivers_cropped2$num_days),]
drivers_cropped2<-drivers_cropped2[complete.cases(drivers_cropped2$drainSqKm),]
drivers_cropped2<- drivers_cropped2[,!c(colnames(drivers_cropped2) %like% ".1")]

## Drainage Area:
small <- drivers_cropped2[drivers_cropped2$drainSqKm < 1000,]
#medium <- drivers_cropped2[(drivers_cropped2$drainSqKm > 50) & (drivers_cropped2$drainSqKm < 1000), ]
large <- drivers_cropped2[(drivers_cropped2$drainSqKm >  1000) & (drivers_cropped2$drainSqKm < 3000000),]

## Lithology
pluto_volcanic <- drivers_cropped2[drivers_cropped2$major_rock %like% "volcanic" | drivers_cropped2$major_rock %like% "pluto",]
metamorphic <- drivers_cropped2[drivers_cropped2$major_rock %like% "metamorphic",]
sed_carb <- drivers_cropped2[drivers_cropped2$major_rock %like% "sed" | drivers_cropped2$major_rock %like% "carb",]

## Median  concentrations
low  <- drivers_cropped2[drivers_cropped2$P < 7,]
high  <- drivers_cropped2[drivers_cropped2$P > 7,]


### Pull out the boxplots and plot outside of RF function with full suite of variables (so LTER/ Up_KG can be used for fill)
bp_func <- function(subset_of_drivers, axis_lims, subset_title, subset_variable, subset_variable_name){
  boxplot <- ggplot(subset_of_drivers, aes(x= Up_KG, y=subset_variable, fill=Up_KG)) +
    geom_boxplot() +
    ylim(axis_lims)+
    labs(x=NULL, y=subset_variable_name, title=subset_title)+
    theme_bw(base_size = 18)+
    theme(legend.position="none",
      axis.text.x=element_text(angle=45, hjust=1))
  show(boxplot)
}


all_med_si<- bp_func(drivers_cropped2, axis_lims = c(0, 20), subset_title = "all sites", drivers_cropped2$med_si, "Median DSi")
all_DA<- bp_func(drivers_cropped2, subset_title = "all sites", axis_lims = c(0, 300000), drivers_cropped2$drainSqKm, "Drainage Area (sqkm)")
# all_rock <- bp_func(drivers_cropped2, axis_lims = c(0, 20), subset_title = "all sites", drivers_cropped2$, "Median DSi")



# Apply function
small_results<- bp_func(small, subset_title = "catchment size < 50 km2", small$drainSqKm, subset_variable_name =  "Drainage Area (sqkm)")
#medium_results<- bp_func(medium, subset_title = "catchment size > 50 < 1000 km2", medium$drainSqKm, "Drainage Area (sqkm)")
large_results<- bp_func(large, subset_title = "catchment size > 1000 < 3000000 km2", large$drainSqKm, "Drainage Area (sqkm)")

## Apply function:
## LATER: Look at N:P ratios
low_results<- bp_func(low_Si, subset_title = "median Si < 3 mg", low_Si$med_si, "Median Si")
high_results<- bp_func(high_Si, subset_title = "median Si > 3 mg", high_Si$med_si, "Median Si")

## Apply function
pluto_volcanic_results<- bp_func(pluto_volcanic, subset_title = "Pluto-Volcanic", pluto_volcanic$med_si, "Median Si")
metamorphic_results<- bp_func(metamorphic, subset_title = "Metamorphic", metamorphic$med_si, "Median Si")
sed_carb_results<- bp_func(sed_carb, subset_title = "Sedimentary-Carbonate", sed_carb$med_si, "Median Si")


# #some next steps (ideas)
# 1. implement recursive feature elimination to only keep variables that are important
# 2. tune mtry (currently set to 6, default is the square root of number of input variables)
# 3. Where are we getting over/under prediction - is this predictable/can we control for some factor?
# 4. Partial dependence plots

