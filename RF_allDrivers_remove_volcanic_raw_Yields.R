## ------------------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries
# install.packages("librarian")
# install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree"))
# install.packages("tree")
# install.packages("RRF")
# install.packages("arsenal")
librarian::shelf(remotes, RRF, caret, randomForest, DAAG, party, rpart, rpart.plot, mlbench, pROC, tree, dplyr,
                 plot.matrix, reshape2, rcartocolor, arsenal, googledrive, data.table, ggplot2, corrplot, pdp)

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
# Load Functions ----
## ------------------------------------------------------- ##
# Function to see variable importance by regime
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

####function to test ntree - change the internal function to reflect the RF model that you are using
test_numtree_average <- function(ntree_list) {
  MSE<-list()
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(yields~.,
                           data=drivers_df, importance=TRUE, proximity=TRUE, 
                           ntree=ntree_list[[i]])
    # MSE[[i]]<-rf_model$err.rate[,1]
    MSE[[i]]<-rf_model$mse
    
    
  }
  return(MSE)
}

# Consider these columns for outlier removal 
cols_to_consider <- c("yields")
sd_limit <- 1.5

remove_outlier_rows <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit){
  z_scores <- as.data.frame(sapply(data_to_filter[cols], function(data) (abs(data-mean(data, na.rm = TRUE))/sd(data, na.rm = TRUE))))    
  return(subset(data_to_filter, !rowSums(z_scores>limit, na.rm = TRUE)))
}

## ------------------------------------------------------- ##
# Read in and Tidy Data ----
## ------------------------------------------------------- ##
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read in the adata
drivers <- read.csv("AllDrivers_Harmonized_20240621.csv")

# Remove any duplicated rows
drivers <- drivers[!duplicated(drivers$Stream_ID),]

# Add in yields
drivers$CQ <- drivers$med_si * drivers$mean_q
drivers$yields <- drivers$CQ / drivers$drainSqKm

#remove variables not interested in ever including
drivers <- dplyr::select(drivers, -c("cycle1","X","X.1","Name","ClimateZ","Latitude","Longitude","LTER","rndCoord.lat",
                                     "rndCoord.lon", "CQ", "drainSqKm"))

drivers$Climate <- drivers$Climate

# look at distribution of NA across columns
sapply(drivers, function(x) sum(is.na(x)))

## Remove sites w NA
## There is an issue with the Canadian sites and not having snow data:
drivers <-drivers[complete.cases(drivers$prop_area),]

# Remove sites with major volcanic rock
# drivers <- subset(drivers, !major_rock == "volcanic")

#select only features to be included in model
drivers_df <- dplyr::select(drivers, -c("Stream_Name", "Stream_ID",                                     # remove metadata
                                        "Min_Daylength", "elevation_min_m", "elevation_max_m",          # remove duplicate drivers
                                        "elevation_median_m", "num_days",
                                        "mean_si", "sd_si", "min_Si", "max_Si","CV_C", "med_si",                  # remove Si variables
                                        "mean_q", "med_q", "sd_q", "CV_Q", "min_Q", "max_Q",            # remove flow variables
                                        "cvc_cvq", "slope",                                             # remove CQ
                                        "major_rock", "major_land", "major_soil",
                                        "rocks_volcanic"))                      # remove major rock, land, soil variables

# there are also some drivers we dont want to include because they're not important to be expanded out (e.g., soil, geology if we switch to major rock)
drivers_df <- dplyr::select(drivers_df,-contains("soil"))
drivers_df <- dplyr::select(drivers_df,-contains("rock"))


# change col names so it looks pretty: 
names(drivers_df)[5]<-paste("snow_cover")
names(drivers_df)[10]<-paste("green_up_day")
names(drivers_df)[25]<-paste("max_daylength")

# there are multiple instances where we filter by row #'s
replace_na <- c(11:21) # this is to replace NAs in % land cover, geology and soils with a 0
numeric_drivers <- c(1:25) # this is for plotting correlation between all numeric drivers

# next let's replace the NA values for things like land cover % and geology % with a zero
drivers_df[,replace_na]<-replace(drivers_df[,replace_na], is.na(drivers_df[,replace_na]), 0) 

# convert all to numeric
drivers_df <- drivers_df %>% mutate_if(is.integer, as.numeric)

# remove outliers
drivers_df<-remove_outlier_rows(drivers_df)

#look at correlation between driver variables
driver_cor <- cor(drivers_df[,numeric_drivers]) # edit these rows when changing variables included 
corrplot(driver_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

#original model, all parameters
#test number of trees 100-1000
MSE_list<-test_numtree_average(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

MSE_df<-as.data.frame(unlist(MSE_list))

MSE_num<-list()

for (i in 1:length(tre_list)) {
  
  MSE_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

MSE_df$tree_num<-unlist(MSE_num)

MSE_mean<-MSE_df %>% group_by(tree_num) %>%
  summarise(mean_MSE=mean(`unlist(MSE_list)`))

#visualize and select number of trees that gives the minimum MSE error
ggplot(MSE_mean, aes(tree_num, mean_MSE))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#tune mtry based on optimized ntree
set.seed(123)
tuneRF(drivers_df[,numeric_drivers], drivers_df[,1], ntreeTry = 1200, stepFactor = 1, improve = 0.5, plot = FALSE)

#run intial RF using tuned parameters
set.seed(123)
rf_model1<-randomForest(yields~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=1300,mtry=8)

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
x<-drivers_df[,!(colnames(drivers_df)=="yields")]

y<-drivers_df$yields

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
rf_formula<-formula(paste("yields~", new_rf_input))

#retune RF after RFE optimization
test_numtree_optimized <- function(ntree_list) {
  
  MSE<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]])
    # MSE[[i]]<-rf_model$err.rate[,1]
    MSE[[i]]<-rf_model$mse
    
    
  }
  
  return(MSE)
  
}


MSE_list<-test_numtree_optimized(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

MSE_df<-as.data.frame(unlist(MSE_list))

MSE_num<-list()

for (i in 1:length(tre_list)) {
  
  MSE_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

MSE_df$tree_num<-unlist(MSE_num)

MSE_mean<-MSE_df %>% group_by(tree_num) %>%
  summarise(mean_MSE=mean(`unlist(MSE_list)`))

#visualize and select number of trees that gives the minimum MSE error
ggplot(MSE_mean, aes(tree_num, mean_MSE))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#retune mtry
kept_drivers<-drivers_df[,c(colnames(drivers_df) %in% predictors(result_rfe))]

set.seed(123)
tuneRF(kept_drivers, drivers_df[,1], ntreeTry = 400, stepFactor = 1, improve = 0.5, plot = FALSE)

#run optimized random forest model, with retuned ntree and mtry parameters
set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=400, mtry=8)


rf_model2

randomForest::varImpPlot(rf_model2)

lm_plot <- plot(rf_model2$predicted, drivers_df$yields, 
                pch=16, cex= 1.5,
                xlab="Predicted",
                ylab="Observed", 
                main= "Yields: Removed Lithology", 
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5) + 
  abline(a=0, b=1, lwd = 3, col="#76933C", lty = 2) + 
  theme(text = element_text(size=40), face = "bold")
legend("topleft", bty = "n", cex=1.5, legend = paste("R2=",format(mean(rf_model2$rsq), digits=3))) 
legend("bottomright", bty="n", cex=1.5, legend = paste("MSE=", format(mean(rf_model2$mse), digits=3)))

#playing around w partial dependence plots
par.Long <- partial(rf_model2, pred.var = "max_daylength")
partial_plot <-autoplot(par.Long, contour = T, size=2) + 
  theme_article() + 
  theme(text = element_text(size=30))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "npp")
partial_plot <-autoplot(par.Long, contour = T, size=2) + 
  theme_article() + 
  theme(text = element_text(size=30))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "green_up_day")
partial_plot <-autoplot(par.Long, contour = T, size=2) + 
  theme_article() + 
  theme(text = element_text(size=30))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "N")
partial_plot <-autoplot(par.Long, contour = T, size=2) + 
  theme_article() + 
  theme(text = element_text(size=30))
print(partial_plot)

par.Long <- partial(rf_model2, pred.var = "temp")
partial_plot <-autoplot(par.Long, contour = T, size=2) + 
  theme_article() + 
  theme(text = element_text(size=30))
print(partial_plot)

# dev.off()
