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


#this code made importance plots
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

## Also add in the climate classifications: 
climate_Z <- read.csv("KG_Clim_Name.csv")

drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

## Merge by climate zone so we have a new column that gives the KG up-scaled classifications
drivers <- merge(drivers, climate_Z)
names(drivers)[names(drivers) == 'Name'] <- 'Up_KG'
drivers$Up_KG = as.factor(drivers$Up_KG)

#crop to only relevant drivers
drivers_cropped <- drivers[,c("med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","Latitude", 
                              "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                            "major_rock","major_land", "major_soil", "elevation_mean_m", "cycle0")]

drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped$num_days),]
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
    
    rf_model<-randomForest(med_si~.,data=drivers_cropped, importance=TRUE, proximity=TRUE, ntree=ntree_list[i], mtry=6)
    
    OOB[[i]]<-mean(rf_model$rsq)
    
  }
  
  return(OOB)
  
}
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


## Intial subsetting of Random Forest Plots: 
## Drainage Area:
small <- drivers_cropped[drivers_cropped$drainSqKm < 50,]
medium <- drivers_cropped[(drivers_cropped$drainSqKm > 50) & (drivers_cropped$drainSqKm < 1000), ]
large <- drivers_cropped[(drivers_cropped$drainSqKm >  1000) & (drivers_cropped$drainSqKm < 3000000),]

small <-small[,!c(colnames(small) %like% "drainSqKm")]
medium <-medium[,!c(colnames(medium) %like% "drainSqKm")]
large <-large[,!c(colnames(large) %like% "drainSqKm")]

## Median silica concentrations
low_Si  <- drivers_cropped[drivers_cropped$med_si < 3,]
high_Si  <- drivers_cropped[drivers_cropped$med_si > 3,]

# low_Si <-low_Si[,!c(colnames(low_Si) %like% "si")]
# high_Si <-high_Si[,!c(colnames(high_Si) %like% "si")]

## Lithology
pluto_volcanic <- drivers_cropped[drivers_cropped$major_rock %like% "volcanic" | drivers_cropped$major_rock %like% "pluto",]
metamorphic <- drivers_cropped[drivers_cropped$major_rock %like% "metamorphic",]
sed_carb <- drivers_cropped[drivers_cropped$major_rock %like% "sed" | drivers_cropped$major_rock %like% "carb",]

## Need to remove rock info for the lithology subset: 
pluto_volcanic <-pluto_volcanic[,!c(colnames(pluto_volcanic) %like% "rock")]
metamorphic <-metamorphic[,!c(colnames(metamorphic) %like% "rock")]
sed_carb <-sed_carb[,!c(colnames(sed_carb) %like% "rock")]

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
  par.Long <- partial(rf_model, pred.var = "cycle0")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "temp")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "precip")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "slope")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "prop_area")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "num_days")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "CV_C")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "elevation_mean_m")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "Latitude")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "npp")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "evapotrans")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "CV_Q")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)
  
  par.Long <- partial(rf_model, pred.var = "cvc_cvq")
  partial_plot <-autoplot(par.Long, contour = T)
  print(partial_plot)

 
}

all_results<- rf_func(drivers_cropped, subset_title = "all sites, no subsets", drivers_cropped$med_si, "Median DSi")

# Apply function
small_results<- rf_func(small, subset_title = "catchment size < 50 km2", small$med_si, subset_variable_name =  "Median DSi")
medium_results<- rf_func(medium, subset_title = "catchment size > 50 < 1000 km2", medium$med_si, "Median DSi")
large_results<- rf_func(large, subset_title = "catchment size > 1000 < 3000000 km2", large$med_si, "Median DSi")

## Apply function
pluto_volcanic_results<- rf_func(pluto_volcanic, subset_title = "Pluto-Volcanic", pluto_volcanic$med_si, "Median Si")
metamorphic_results<- rf_func(metamorphic, subset_title = "Metamorphic", metamorphic$med_si, "Median Si")
sed_carb_results<- rf_func(sed_carb, subset_title = "Sedimentary-Carbonate", sed_carb$med_si, "Median Si")

## Apply function: 
## LATER: Look at N:P ratios or other productivity metrics
low_results<- rf_func(low_Si, subset_title = "median Si < 3 mg", low_Si$med_si, "Median Si")
high_results<- rf_func(high_Si, subset_title = "median Si > 3 mg", high_Si$med_si, "Median Si")


#### Make boxplots of distributions for each model subset -----------------
drivers_cropped2 <- drivers[,c("med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","Latitude", 
                              "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                              "major_rock","major_land", "major_soil", "elevation_mean_m", "cycle0", "Up_KG")]

drivers_cropped2<-drivers_cropped2[complete.cases(drivers_cropped2$num_days),]
drivers_cropped2<-drivers_cropped2[complete.cases(drivers_cropped2$drainSqKm),]
drivers_cropped2<- drivers_cropped2[,!c(colnames(drivers_cropped2) %like% ".1")]

## Drainage Area:
small <- drivers_cropped2[drivers_cropped2$drainSqKm < 50,]
medium <- drivers_cropped2[(drivers_cropped2$drainSqKm > 50) & (drivers_cropped2$drainSqKm < 1000), ]
large <- drivers_cropped2[(drivers_cropped2$drainSqKm >  1000) & (drivers_cropped2$drainSqKm < 3000000),]

## Lithology
pluto_volcanic <- drivers_cropped2[drivers_cropped2$major_rock %like% "volcanic" | drivers_cropped2$major_rock %like% "pluto",]
metamorphic <- drivers_cropped2[drivers_cropped2$major_rock %like% "metamorphic",]
sed_carb <- drivers_cropped2[drivers_cropped2$major_rock %like% "sed" | drivers_cropped2$major_rock %like% "carb",]

## Median silica concentrations
low_Si  <- drivers_cropped2[drivers_cropped2$med_si < 3,]
high_Si  <- drivers_cropped2[drivers_cropped2$med_si > 3,]


### Pull out the boxplots and plot outside of RF function with full suite of variables (so LTER/ Up_KG can be used for fill)
bp_func <- function(subset_of_drivers, subset_title, subset_variable, subset_variable_name){
  boxplot <- ggplot(subset_of_drivers, aes(x= Up_KG, y=subset_variable, fill=Up_KG)) + 
    geom_boxplot() + 
    labs(x=NULL, y=subset_variable_name, title=subset_title)+
    theme_bw(base_size = 18)+
    theme(legend.position="none", 
      axis.text.x=element_text(angle=45, hjust=1))
  show(boxplot)
}


all_results<- bp_func(drivers_cropped2, subset_title = "all sites, no subsets", drivers_cropped2$med_si, "Median DSi")

# Apply function
small_results<- bp_func(small, subset_title = "catchment size < 50 km2", small$drainSqKm, subset_variable_name =  "Drainage Area (sqkm)")
medium_results<- bp_func(medium, subset_title = "catchment size > 50 < 1000 km2", medium$drainSqKm, "Drainage Area (sqkm)")
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

