## Adapting FFT from LTER to shifting timing Si analysis: 
rm(list=ls(all=TRUE)) #give R a blank slate

library(discharge)
library(tidyverse)
library(lubridate)
library(quantreg)
library(ggthemes)
library(zoo)

### Read in data, extract summary statistics ###
### IMPORTANT: Ensure data are continuous (no NAs) ###
### IMPORTANT: Fourier uses a log-function so data must be non-zero and non-negative ###

## Need to do some cleaning to get this to work for just one site and solute initially
#raw_import_wrtds_month <-read.csv("/Users/sidneybush/Library/CloudStorage/Box-Box/Personal/Global_Chemistry/Full_Results_Monthly_GFN_WRTDS.csv", header=TRUE)
raw_import_wrtds <-read.csv("/Users/sidneybush/Library/CloudStorage/Box-Box/Personal/Global_Chemistry/Full_Results_GFN_WRTDS.csv", header=TRUE)

### date format ###
raw_import_wrtds$Date<-as.Date(raw_import_wrtds$Date)

### exclude some columns we don't need
#wrtds_daily <- raw_import_wrtds[,c(1,2,3,4,21)]
#wrtds_daily <-na.omit(wrtds_daily)

## subset chemicals of interest (do this differently later)
DSi_all <- subset(raw_import_wrtds, chemical=="DSi")
NOx_all <- subset(raw_import_wrtds, chemical=="NOx")
P_all <- subset(raw_import_wrtds, chemical=="P")

### Delineate some example sites: These are all DSi
chipp_dsi <- subset(DSi_all, stream=="CH00.1M")
chipp_nox <- subset(NOx_all, stream=="CH00.1M")
chipp_p <- subset(P_all, stream=="CH00.1M")

########### make this a function: ######################
fft_DSi <- function(site_df, stream_name, start, end, minimum, maximum){
  
  ## Plot: 
  ggplot(data = site_df, aes(x=Date, y=Yield)) + 
    geom_line() + 
    theme_bw() + 
    ylab("Yield")+
    ggtitle(stream_name)
  
  ## subset to just yield
  site_df_yield = subset(site_df, select = c(Date, Yield))
  site_df_yield <- na.omit(site_df_yield)
  
  summary(site_df_yield$Yield) # OK
  
  ### Fourier Analysis & Data Extremes ###
  #Create 'streamflow' object
  site_df_yield_data <-asStreamflow(site_df_yield,
                                    start.date=start, end.date=end,
                                    river.name= stream_name)
  
  ###Plot data to visually inspect time-series pulses
  plot(site_df_yield$Yield ~ site_df_yield$Date, type="l", ylab="DSi Yield", 
       xlab="Date") + title(stream_name)
  

  #Fourier Analysis
  site_df_yield_seas <- fourierAnalysis(site_df_yield_data)
  summary(site_df_yield_seas) # SUMMARY STATS
  #1)"noise color is a measure of flashiness. Noise color ranges from 0 
  # (white noise with no autocorrelation—highly flashy) to values
  #over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
  #2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
  #3) "average" is long-term ordinal day average.
  plot(site_df_yield_seas, plot.type="hydrograph") # PLOT
  
  #Calculate annual extremes
  site_df_yield_extremes<-annualExtremes(site_df_yield_data)
  names(site_df_yield_extremes)

  # prepare baseline signal
  x.bl = prepareBaseline(site_df_yield_data)
  # get signal parts
  x.sp = getSignalParts(x.bl$pred2, candmin = minimum, candmax = maximum,
                        years = site_df_yield_data$data$year,
                        months = site_df_yield_data$data$month,
                        jdays = site_df_yield_data$data$jday)
  
  ### Do these calcs for each year and for full years 
  # get HSAM values
  hsam = getHSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(hsam$HSAM ~ hsam$year, type="p", ylab="HSAM DSi Yield",
       xlab="Year") + title(stream_name)
  
  # get LSAM values
  lsam = getLSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(lsam$LSAM ~ lsam$year, type="p", ylab="LSAM DSi Yield",
       xlab="Year") + title(stream_name)
  
  # timing HSAM
  thsam = getTimingHSAM(hsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(thsam$timing.hsam ~ thsam$year, type="p", ylab="DSi Timing of max (day)",
       xlab="Year") + title(stream_name)

  # timing LSAM
  tlsam = getTimingLSAM(lsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(tlsam$timing.lsam ~ lsam$year, type="p", ylab="DSi Timing of min (day)",
       xlab="Year") + title(stream_name)

  # transition time
  tt = getTransitionTime(hsam$Index.all, lsam$Index.all, hsam$year)
  plot(tt$transition.time ~ tt$year, type="p", ylab="DSi Transition Time (days)",
       xlab="Year") + title(stream_name)

  # IDI
  idi = getIDI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$HF.window.start,
               x.sp$HF.window.end, x.sp$year)
  plot(idi$IDI ~ idi$year, type="p", ylab="IDI",
       xlab="Year") + title(stream_name)

  # IFI
  ifi = getIFI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$LF.window.start,
               x.sp$LF.window.end, x.sp$year)
  plot(ifi$IFI ~ ifi$year, type="p", ylab="IFI",
       xlab="Year") + title(stream_name)

  ## NAA
  naa = getNAA(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(naa$NAA ~ naa$year, type="p", ylab="DSi Net Annual Anomoly (NAA)",
       xlab="Year") + title(stream_name)
  
}

fft_DSi(site_df = chipp_dsi, stream_name = "DSi - Chippewa", 
        start ="2000-01-01", end = "2022-01-01",
        minimum = c(200:250), maximum = c(50:150))

### TESTING ANALYSIS ON ONE SITE:
## subset to just yield

# chipp_dsi_yield = subset(chipp_dsi, select = c(Date, Yield))
# chipp_dsi_yield <- na.omit(chipp_dsi_yield)
# 
# chipp_dsi_yield_data <-asStreamflow(chipp_dsi_yield,
#                                   start.date="2000-01-01", end.date="2022-01-01",
#                                   river.name= "chipp")
# chipp_dsi_seas <- fourierAnalysis(chipp_dsi_yield_data)
# summary(chipp_dsi_seas) 


######################################################## NOx

fft_NOx <- function(site_df, stream_name, start, end, minimum, maximum){
  
  ## Plot: 
  ggplot(data = site_df, aes(x=Date, y=Yield)) + 
    geom_line() + 
    theme_bw() + 
    ylab("Yield")+
    ggtitle(stream_name)
  
  ## subset to just yield
  site_df_yield = subset(site_df, select = c(Date, Yield))
  site_df_yield <- na.omit(site_df_yield)
  
  summary(site_df_yield$Yield) # OK
  
  ### Fourier Analysis & Data Extremes ###
  #Create 'streamflow' object
  site_df_yield_data <-asStreamflow(site_df_yield,
                                    start.date=start, end.date=end,
                                    river.name= stream_name)
  
  ###Plot data to visually inspect time-series pulses
  plot(site_df_yield$Yield ~ site_df_yield$Date, type="l", ylab="NOx Yield", 
       xlab="Date") + title(stream_name)
  
  
  #Fourier Analysis
  site_df_yield_seas <- fourierAnalysis(site_df_yield_data)
  summary(site_df_yield_seas) # SUMMARY STATS
  #1)"noise color is a measure of flashiness. Noise color ranges from 0 
  # (white noise with no autocorrelation—highly flashy) to values
  #over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
  #2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
  #3) "average" is long-term ordinal day average.
  plot(site_df_yield_seas, plot.type="hydrograph") # PLOT
  
  #Calculate annual extremes
  site_df_yield_extremes<-annualExtremes(site_df_yield_data)
  names(site_df_yield_extremes)
  
  # prepare baseline signal
  x.bl = prepareBaseline(site_df_yield_data)
  # get signal parts
  x.sp = getSignalParts(x.bl$pred2, candmin = minimum, candmax = maximum,
                        years = site_df_yield_data$data$year,
                        months = site_df_yield_data$data$month,
                        jdays = site_df_yield_data$data$jday)
  # get HSAM values
  hsam = getHSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(hsam$HSAM ~ hsam$year, type="p", ylab="HSAM NOx Yield",
       xlab="Year") + title(stream_name)
  
  # timing HSAM
  thsam = getTimingHSAM(hsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(thsam$timing.hsam ~ thsam$year, type="p", ylab="NOx Timing of max (day)",
       xlab="Year") + title(stream_name)
  
  # get LSAM values
  lsam = getLSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(lsam$LSAM ~ lsam$year, type="p", ylab="LSAM NOx Yield",
       xlab="Year") + title(stream_name)
  
  # timing LSAM
  tlsam = getTimingLSAM(lsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(tlsam$timing.lsam ~ lsam$year, type="p", ylab="NOx Timing of min (day)",
       xlab="Year") + title(stream_name)
  
  # transition time
  tt = getTransitionTime(hsam$Index.all, lsam$Index.all, hsam$year)
  plot(tt$transition.time ~ tt$year, type="p", ylab="NOx Transition Time (days)",
       xlab="Year") + title(stream_name)
  
  # IDI
  idi = getIDI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$HF.window.start,
               x.sp$HF.window.end, x.sp$year)
  plot(idi$IDI ~ idi$year, type="p", ylab="NOx IDI",
       xlab="Year") + title(stream_name)
  
  # IFI
  ifi = getIFI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$LF.window.start,
               x.sp$LF.window.end, x.sp$year)
  plot(ifi$IFI ~ ifi$year, type="p", ylab="NOx IFI",
       xlab="Year") + title(stream_name)
  
  ## NAA
  naa = getNAA(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(naa$NAA ~ naa$year, type="p", ylab="NOx Net Annual Anomoly (NAA)",
       xlab="Year") + title(stream_name)
  
}

fft_NOx(site_df = chipp_nox, stream_name = "NOx - Chippewa", 
        start ="2000-01-01", end = "2022-01-01",
        minimum = c(200:250), maximum = c(50:150))

  
######################################################## DIN

fft_P <- function(site_df, stream_name, start, end, minimum, maximum){
  
  ## Plot: 
  ggplot(data = site_df, aes(x=Date, y=Yield)) + 
    geom_line() + 
    theme_bw() + 
    ylab("Yield")+
    ggtitle(stream_name)
  
  ## subset to just yield
  site_df_yield = subset(site_df, select = c(Date, Yield))
  site_df_yield <- na.omit(site_df_yield)
  
  summary(site_df_yield$Yield) # OK
  
  ### Fourier Analysis & Data Extremes ###
  #Create 'streamflow' object
  site_df_yield_data <-asStreamflow(site_df_yield,
                                    start.date=start, end.date=end,
                                    river.name= stream_name)
  
  ###Plot data to visually inspect time-series pulses
  plot(site_df_yield$Yield ~ site_df_yield$Date, type="l", ylab="P Yield", 
       xlab="Date") + title(stream_name)
  
  
  #Fourier Analysis
  site_df_yield_seas <- fourierAnalysis(site_df_yield_data)
  summary(site_df_yield_seas) # SUMMARY STATS
  #1)"noise color is a measure of flashiness. Noise color ranges from 0 
  # (white noise with no autocorrelation—highly flashy) to values
  #over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
  #2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
  #3) "average" is long-term ordinal day average.
  plot(site_df_yield_seas, plot.type="hydrograph") # PLOT
  
  #Calculate annual extremes
  site_df_yield_extremes<-annualExtremes(site_df_yield_data)
  names(site_df_yield_extremes)
  
  # prepare baseline signal
  x.bl = prepareBaseline(site_df_yield_data)
  # get signal parts
  x.sp = getSignalParts(x.bl$pred2, candmin = minimum, candmax = maximum,
                        years = site_df_yield_data$data$year,
                        months = site_df_yield_data$data$month,
                        jdays = site_df_yield_data$data$jday)
  # get HSAM values
  hsam = getHSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(hsam$HSAM ~ hsam$year, type="p", ylab="HSAM P Yield",
       xlab="Year") + title(stream_name)
  
  # timing HSAM
  thsam = getTimingHSAM(hsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(thsam$timing.hsam ~ thsam$year, type="p", ylab="P Timing of max (day)",
       xlab="Year") + title(stream_name)
  
  # get LSAM values
  lsam = getLSAM(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(lsam$LSAM ~ lsam$year, type="p", ylab="LSAM P Yield",
       xlab="Year") + title(stream_name)
  
  # timing LSAM
  tlsam = getTimingLSAM(lsam$Index.all, x.sp$peak.index, x.sp$year)
  plot(tlsam$timing.lsam ~ lsam$year, type="p", ylab="P Timing of min (day)",
       xlab="Year") + title(stream_name)
  
  # transition time
  tt = getTransitionTime(hsam$Index.all, lsam$Index.all, hsam$year)
  plot(tt$transition.time ~ tt$year, type="p", ylab="P Transition Time (days)",
       xlab="Year") + title(stream_name)
  
  # IDI
  idi = getIDI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$HF.window.start,
               x.sp$HF.window.end, x.sp$year)
  plot(idi$IDI ~ idi$year, type="p", ylab="P IDI",
       xlab="Year") + title(stream_name)
  
  # IFI
  ifi = getIFI(x.bl$resid.sig, site_df_yield_data$data$year, x.sp$LF.window.start,
               x.sp$LF.window.end, x.sp$year)
  plot(ifi$IFI ~ ifi$year, type="p", ylab="P IFI",
       xlab="Year") + title(stream_name)
  
  ## NAA
  naa = getNAA(x.bl$resid.sig, site_df_yield_data$data$year)
  plot(naa$NAA ~ naa$year, type="p", ylab="P Net Annual Anomoly (NAA)",
       xlab="Year") + title(stream_name)
  
}

fft_P(site_df = chipp_p, stream_name = "P - Chippewa", 
      start ="2000-01-01", end = "2022-01-01",
      minimum = c(200:250), maximum = c(50:150))


