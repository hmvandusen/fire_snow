library(readr)
library(lubridate) # date management
library(forecast) 
library(stlplus) 
library(fpp) 
library(dplyr) 
library(reshape2) 
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

Snotel_Johnsoncreek <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/439:ID:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value", 
                                comment = "#")
View(Snotel_Johnsoncreek)


#SWE starts on the 576 data entry (which is Oct 1 1980)
Snotel_Johnsoncreek$`Snow Water Equivalent (in) Start of Day Values`

#Precip starts on the 576 data entry as well
Snotel_Johnsoncreek$`Precipitation Accumulation (in) Start of Day Values`

n<-dim(Snotel_Johnsoncreek)[1]
Snotel_JC<-Snotel_Johnsoncreek[576:n,]
head(Snotel_JC)
tail(Snotel_JC)

Snotel_2000_2022<-Snotel_JC[Snotel_JC$Date >= '2000-01-01',]
head(Snotel_2000_2022)

#Plots of all years from 2000 - 2021 and add in vertical line for fire
plot(Snotel_2000_2022$Date,Snotel_2000_2022$`Snow Water Equivalent (in) Start of Day Values`, type='l')
abline(v=as.Date('2007-07-17'), col='red')

snotel_analysis<-function(snotel.df){

  #changing names for more simple coding
  colnames(snotel.df)<-c('Date', 'swe', 'precip_acc', 'temp_max', 'temp_min', 'temp_avg', 'precip_inc')
  snotel.df$stationID <- 439
  
  #Making time series in water year
  #I am separating the date for each column 
  snotel.df$year <-lubridate::year(snotel.df$Date) # add year col
  snotel.df$month <-lubridate::month(snotel.df$Date) # add month col
  snotel.df$day <-lubridate::day(snotel.df$Date) # add day of month col
  snotel.df$doy <-lubridate::yday(snotel.df$Date)# add day of year col
  snotel.df<-addWaterYear(snotel.df) #add water year
  
  snotel.df$wyyear.fact<-as.factor(snotel.df$waterYear)
  snotel_ls<<-split(snotel.df, snotel.df$wyyear.fact)
}

####
##function 2 for each year
###

eachyear<-function(yearperiod){
  
  #This is calculating the 30 day running average. This is one sided instead of two 
  rollmean <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}
  yearperiod$rollmean <- as.numeric(rollmean(yearperiod$discharge_cfs)) 
  
  #This calculates 30 day moving average 15 days on both sides
  ma30 <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=2)}
  yearperiod$ma30 <- as.numeric(ma30(yearperiod$discharge_cfs)) 
  
  #Ploting 30-day rolling mean vs 30-day moving average
  ## Moving average is much sooner than the rolling average. This will provide you with a very different date
  plot(yearperiod$Date,yearperiod$rollmean, type='l', col='red', xlim=as.Date(plot_date))
  lines(yearperiod$Date, yearperiod$ma30)
  
  ##
  ##Painter et al. Rising Limb
  #Running mean flow since 1 January
  
  for (i in 1:dim(yearperiod)[1]) {
    yearperiod$runmeanPainter[i]<-sum(yearperiod$discharge_cfs[1:i])/i
  }
  
  
  plot(yearperiod$Date,yearperiod$rollmean, type='l', col='red', xlim=as.Date(plot_date), ylim = c(0,5000))
  lines(yearperiod$Date, yearperiod$ma30)
  lines(yearperiod$Date, yearperiod$discharge_cfs, col='blue')
  lines(yearperiod$Date, yearperiod$runmeanPainter, col='green')
  
  ##Find percent change of Painter running mean:
  
  for (i in 2:dim(yearperiod)[1]) {
    yearperiod$perchange[1]<-NA
    yearperiod$perchange[i]<- ((yearperiod$runmeanPainter)[i]/yearperiod$runmeanPainter[i-1]*100)-100
    
  }
  
  plot(yearperiod$Date, yearperiod$perchange, type='l', col='red', xlim=as.Date(plot_date), ylim = c(0,20))
  abline(h=1)
  
  assign(paste0("watershed_", yearperiod$year[1]), yearperiod, envir = globalenv())
  
}

#Application of functions 
snotel_allyears<-snotel_analysis(Snotel_2000_2022)

#not sure if this is necessary
#watershed_allyears.ls<-lapply(watershed_allyears, function(y) eachyear(y))

#Creating a usable data frame
snotel_metrics<-data.frame(time=1:23, wyyear=NA, peak_date=as.Date(NA), peakswe_mm=NA, time_of_melt=as.Date(NA), slope_melt=NA)

for (i in 1:length(snotel_allyears)) {
  snotel_metrics$wyyear[i]<-snotel_allyears[[i]]$waterYear[1]
  
  peak_swe<- max(snotel_allyears[[i]]$swe)
  snotel_metrics$peakswe_mm[i]<-peak_swe*25.4
  snotel_metrics$peak_swe_in[i]<-peak_swe
  
  date<-snotel_allyears[[i]][snotel_allyears[[i]]$swe == peak_swe,]$Date
  snotel_metrics$peak_date[i]<- date[1]
  
  time_of_full_melt = snotel_allyears[[i]][snotel_allyears[[i]]$month > 2 & snotel_allyears[[i]]$swe == 0,]$Date[1]
  
  snotel_metrics$time_of_melt[i] <- time_of_full_melt 
  
  #Find mean slope from start to peak flow 
  snotel_metrics$slope_melt[i]<- (peak_swe - snotel_allyears[[i]][snotel_allyears[[i]]$Date == as.Date(time_of_full_melt), ]$swe)/ 
    as.numeric(time_of_full_melt[1]- date[1])
  
  #snotel_metrics$temp_avg[i] <- mean(c(snotel_ls[[i-1]][snotel_ls[[i -1]]$month > 11,]$temp_avg, 
                                  #snotel_ls[[i]][snotel_ls[[i]]$month > 1 & snotel_ls[[i]]$month < 4,]$temp_avg))
    
  #snotel_metrics$temp_max_w[i] <- mean(c(snotel_ls[[i-1]][snotel_ls[[i -1]]$month > 11,]$temp_max, 
                                      #snotel_ls[[i]][snotel_ls[[i]]$month > 1 & snotel_ls[[i]]$month < 4,]$temp_max))
  
  snotel_metrics$precip_max[i] <- snotel_ls[[i]][snotel_ls[[i]]$swe == peak_swe,]$precip_acc
  #snotel_metrics$swe_precip[i]<-snotel_ls[[i]]$swe/snotel_ls[[i]]$precip_acc
} #moving average mean slope is still not working 


#snotel_metrics$precip_max[i] <- snotel_ls[[i]][snotel_ls[[i]]$swe == peak_swe,]$precip_acc


snotel_metrics$peak_date<-as.Date(format(snotel_metrics$peak_date, format="%m-%d"), "%m-%d")
snotel_metrics$time_of_melt<-as.Date(format(snotel_metrics$time_of_melt, format="%m-%d"), "%m-%d")


####
#Dry day for loop
####

swe_diff <- list(NA) 
max_dry <- numeric(length = 0)
median_dry <- numeric(length = 0)

for (j in 1:length(snotel_allyears)) {
  swe_diff.v <- numeric(length = 0)
  
  #Snotel dry days after Oct 1 and before May 1: Snow Season
  #This defines dry days as 0 accumulation 
  snotel_drydays<-snotel_ls[[j]][snotel_ls[[j]]$precip_inc == 0.0 & (snotel_ls[[j]]$month < 5 | snotel_ls[[j]]$month > 10), ]
  
  #this splits up all the days that are consecutive to their own list object
  drydaysls<-split(snotel_drydays$doy, cumsum(c(1, diff(snotel_drydays$doy) != 1)))
  
  median_dry[j]<-median(lengths(drydaysls))
  max_dry[j] <-max(lengths(drydaysls))
  
  for (i in 1:length(drydaysls)){
    if (length(drydaysls[[i]]) > 2 ){ # I would change this 2 to either median dry days for each year or max dry days 
      
      swe_diff.v[i] <- snotel_drydays[snotel_drydays$doy == min(drydaysls[[i]]),]$swe - 
        snotel_drydays[ snotel_drydays$doy == max(drydaysls[[i]]),]$swe
    }
  }
  
  swe_diff[[j]]<-swe_diff.v
  #names(swe_diff)<-2000:2022
  
}

boxplot(swe_diff)

swe_diff_mean<-as.data.frame(do.call(rbind, lapply(swe_diff, function(x) mean(na.omit(x)))))

snotel_metrics$mean_swe_loss<-swe_diff_mean$V1
snotel_metrics$median_dryd<-median_dry
snotel_metrics$max_dryd<-max_dry

#creating a factor for before fire and 6 years after fire

snotel_metrics$before_after_f<- as.factor(with(snotel_metrics, ifelse( wyyear <= fire_year, 'prefire', 
                                                                       ifelse(wyyear > fire_year & snotel_metrics$wyyear <= fire_year+6, 'postfire_6yr', 
                                                                              'postfire'))))


#Mean date of spring onset (mean slope is pretty much the same)
mean(na.omit(snotel_metrics[snotel_metrics$year <= fire_year,]$peak_date))
mean(na.omit(snotel_metrics[snotel_metrics$year > fire_year & snotel_metrics$year <= fire_year+3,]$peak_date))

mean(snotel_metrics[snotel_metrics$year <= fire_year,]$peakswe_mm)
mean(na.omit(snotel_metrics[snotel_metrics$year > fire_year & snotel_metrics$year <= fire_year+3,]$peakswe_mm))

#mean(na.omit(snotel_metrics_precip[snotel_metrics_precip$year <= fire_year,]$rration_maPeak))
#mean(na.omit(snotel_metrics_precip[snotel_metrics_precip$year > fire_year & snotel_metrics_precip$year <= fire_year+3,]$rration_maPeak))


boxplot(peak_date ~ before_after_f, data = snotel_metrics)
boxplot(peakswe_mm ~ before_after_f, data = snotel_metrics)
boxplot(slope_melt ~ before_after_f, data = snotel_metrics)
boxplot(time_of_melt ~ before_after_f, data = snotel_metrics)

ggplot(snotel_metrics %>% filter(!is.na(before_after_f)), 
       aes(x=before_after_f, y=max_dryd, fill=before_after_f)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2") +
  xlab( "Pre or Post Fire") + ylab('midwintermelt loss') + ggtitle( "Snotel site") 


 

plot(snotel_metrics$year, snotel_metrics$median_dryd, type='l')




