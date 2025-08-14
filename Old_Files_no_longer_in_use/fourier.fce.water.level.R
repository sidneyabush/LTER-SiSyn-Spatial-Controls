######## Pulse metrics using Fourier time-series analyses########
######## LTER Pulse Dynamics Working Group, John Kominoski August 27, 2022########


###'discharge' R package
#https://urldefense.com/v3/__https://cran.r-project.org/web/packages/discharge/discharge.pdf__;!!FjuHKAHQs5udqho!MsmKtf5lX-ZPH2JAyjRWFewbNfbOHPm-74v0qYzU6Hw3RDE9pRXSz2idBhwkaaPhFp8yWiRqX9GRsa7PJg$  

###### DATA STEPS ######
#rm(list=ls(all=TRUE)) #give R a blank slate
### Set working directory ###
#setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Code/timing analyses")

### Install Libraries ###
#install.packages('discharge_1.0.tar.gz', lib='destination_directory',repos = NULL)
library(discharge)
library(tidyverse)
library(lubridate)
library(quantreg)
library(ggthemes)

### Read in data, extract summary statistics ###
### IMPORTANT: Ensure data are continuous (no NAs) ###
### IMPORTANT: Fourier uses a log-function so data must be non-zero and non-negative ###

fce.srs3.level.data <-read.table("/Users/sidneybush/Library/CloudStorage/Box-Box/Personal/Global_Chemistry/fce.srs3.level.csv", sep=",", header=TRUE)[,1:2]

summary(fce.srs3.level.data) # OK

#positivize the data for maxair by adding 20, cannot take log of zero/negative
fce.srs3.level.data$level<-fce.srs3.level.data$level+42

### date format ###
fce.srs3.level.data$date<-as.Date(fce.srs3.level.data$date)

### Fourier Analysis & Data Extremes ###
#Create 'streamflow' object
fce.srs3.level.data<-asStreamflow(fce.srs3.level.data,start.date="2000-01-01",end.date="2021-12-31", 
                                  river.name="SRS-3")

###Plot data to visually inspect time-series pulses
fce.srs3.level<-fce.srs3.level.data$data
plot(fce.srs3.level$discharge~fce.srs3.level$date, type="l", ylab="Water Level (NAVD88 + 42 (cm)", 
     xlab="Date")

#Fourier Analysis
fce.srs3.level.seas<-fourierAnalysis(fce.srs3.level.data)
summary(fce.srs3.level.seas) # SUMMARY STATS
#1)"noise color is a measure of flashiness. Noise color ranges from 0 (white noise with no autocorrelation—highly flashy) to values
#over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
#2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
#3) "average" is long-term ordinal day average.
plot(fce.srs3.level.seas, plot.type="hydrograph") # PLOT

#Calculate annual extremes
fce.srs3.level.extremes<-annualExtremes(fce.srs3.level.data)
names(fce.srs3.level.extremes)

# compare for periods from 2002 to 2011 and 2012 to 2021
comp = compare.periods(c("2000-01-01", "2010-12-31"),
                       c("2011-01-01", "2021-12-31"), fce.srs3.level, plot=T)

# allstats function works on files
# write R 'streamflow' object data into csv file
fce.srs3.level<-fce.srs3.level.data$data
write.csv(fce.srs3.level, "fce.srs3.level.file.csv")

fce.srs3.level.allstats<-allstats(file.name = "fce.srs3.level.file.csv", river.name = "SRS-3", file.type="csv", date.col=2,
                             discharge.col=3, skipped.rows=0)

#A data frame with columns
#a.rms = Root mean squared amplitude.
#n.rms = Root mean squared noise.
#snr = Signal-to-noise ratio.
#theta.d = Daily noise color.
#theta.a = Annual noise color.
#sigma.lf = Sigma for low flow events.
#sigma.hf = Sigma for high flow events.
#q2 = 2-year return level (flood).
#q10 = 10-year return level (flood).
#l2 = 2-year return level (drought).
#l10 = 10-year return level (drought).


### Long-term median values per Julian day ###
median.year<-tapply(fce.srs3.level$discharge, list(fce.srs3.level$year), median)
median.sum<-median(median.year) #69.1
median.jday<-tapply(fce.srs3.level$discharge, list(fce.srs3.level$jday), median)
jday<-tapply(fce.srs3.level$jday, list(fce.srs3.level$jday), mean)
year<-tapply(fce.srs3.level$year, list(fce.srs3.level$year), mean)

### Long-term min and max per year ###
max.annual<-tapply(fce.srs3.level$discharge, list(fce.srs3.level$year), max)
min.annual<-tapply(fce.srs3.level$discharge, list(fce.srs3.level$year), min)

###Phenologies to assess mean date and CV
###min<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###max<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###days.min and days.max could assess the pulse phenology and character
###max.jday$annual.max - min.jday$annual.min
min<-lm(min.annual~year)
summary.min<-summary(min)
min.slope<-summary.min$coefficients[2]
min.p<-summary.min$coefficients[2,4]

#Quantile regression
#min.annual<-as.vector(min.annual)
#year<-as.vector(year)
#min.rq<-rq(min.annual~year)
#summary(min.rq)
#min.rq.slope<-0.008

#plot(min) #use to check residuals
plot(min.annual~year)

max<-lm(scale(max.annual)~scale(year))
#max<-rq(max.annual~year)
summary.max<-summary(max)
max.slope<-summary.max$coefficients[2]
max.p<-summary.max$coefficients[2,4]
max.slope
max.p

#look at graph
max<-data.frame(year,max.annual)
max.graph<-ggplot(data=max,aes(x = year, y = max.annual-42)) + 
  geom_point(color='darkslategray',size=4) +
  geom_smooth(method = "lm",  se = FALSE, color="darkslategray")+
  ylab("Maximum daily level (NAVD88, cm)")+
  xlab("Year")+
  theme_tufte(base_size = 14, base_family="sans")
max.graph
#save graphic - revise file name for your dataset
ggsave("fce.SRS3.level.max.jpg",max.graph,dpi=300,width=10, height=4)

min<-data.frame(year,min.annual)
min.graph<-ggplot(data=min,aes(x = year, y = min.annual-42)) + 
  geom_point(color='darkslategray',size=4) +
  geom_smooth(method = "lm",  se = FALSE, color="darkslategray")+
  ylab("Minimum daily level (NAVD88, cm)")+
  xlab("Year")+
  theme_tufte(base_size = 14, base_family="sans")
min.graph
#save graphic - revise file name for your dataset
ggsave("fce.SRS3.level.min.jpg",min.graph,dpi=300,width=10, height=4)



#Quantile regression
max.annual<-as.vector(max.annual)
year<-as.vector(year)
max.rq<-rq(max.annual~year)
summary.max.rq<-summary(max.rq)
summary.max.rq
#max.rq.slope<-summary.max.rq$coefficients[2]
#max.rq.p<-summary.max.rq$coefficients[2,4]

#plot(max) #use to check residuals
plot(max.annual~year)

f1<-data.frame(fce.srs3.level.extremes$annual.max)
max.window<-(max(f1$jday)-min(f1$jday))
###Maximum period in-between peak Tmax period 344 days###

f2<-data.frame(fce.srs3.level.extremes$annual.min)
min.window<-(max(f2$jday)-min(f2$jday))
###Maximum period in-between low Tmax period 83 days###


### Plot annual.min and annual.max values ###
par(mfrow=c(1,2), mar = c(5, 5, 1, 1))
plot(f1$discharge~f1$jday, ylab="Water Level (NAVD88 + 42, cm)", xlab="Julian Day", ylim=c(0,130), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,130), xlim=c(0,360), ylab="", xlab="")
abline(69.1,0)

plot(f2$discharge~f2$jday, ylab="Water Level (NAVD88 + 42, cm)", xlab="Julian Day", ylim=c(0,130), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,130), xlim=c(0,360), ylab="", xlab="")
abline(69.1,0)


####################################3
#name your LTER
lter<-"FCE"

#name your site
site<-"SRS-3"

#name your driver
driver<-"Water Level"
#units = units of driver (e.g., mm)
units<-"cm"

#maximum window
max.period<-max.window
#max.units<-"days"

#minimum window
min.period<-min.window
#min.units<-"days"

#maximum slope
max.slope<-max.slope

#maximum slope P-value
max.p<-max.p

#minimum slope
min.slope<-min.slope

#minimum slope P-value
min.p<-min.p

#what is the largest/smallest daily value in the full dataset?
extreme.max<-max(max.annual)-42
extreme.min<-min(min.annual)-42

#Fourier Analysis
fce.srs3.level.seas<-fourierAnalysis(fce.srs3.level.data)
summary(fce.srs3.level.seas) 
noise.color<- 1.882791 
avg.discharge<-0.964802 
signal.noise<- 67.42215 

######### Save metrics into data frame ##################
pulse_metrics_FCE_SRS3<-data.frame(lter,site,driver,units,max.period,min.period,fce.srs3.level.allstats,
                                max.slope,max.p,min.slope,min.p,extreme.max,extreme.min,
                                noise.color,avg.discharge, signal.noise)
write.csv(pulse_metrics_FCE_SRS3,"pulse_metrics_fce.srs3.level_fourier.csv")
#then we will merge metrics data frame across LTER datasets using rbind

