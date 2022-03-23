library(readr)
library(lubridate) # date management
library(forecast) 
library(stlplus) 
library(fpp) 
library(dplyr) 
library(reshape2) 
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

  
  #Making time series in water year
  #I am separating the date for each column 
  snotel.df$year <-lubridate::year(snotel.df$Date) # add year col
  snotel.df$month <-lubridate::month(snotel.df$Date) # add month col
  snotel.df$day <-lubridate::day(snotel.df$Date) # add day of month col
  snotel.df$doy <-lubridate::yday(snotel.df$Date) # add day of year col
  
 
  #Check for missing days, if so, add NA rows:
  if(as.numeric(diff(range(snotel.df$Date))) != (nrow(snotel.df)+1)){
    fullDates <- seq(from=min(snotel.df$Date),
                     to = max(snotel.df$Date), by="1 day")
    fullDates <- data.frame(Date = fullDates, 
                            agency_cd = snotel.df$agency_cd[1],
                            site_no = snotel.df$site_no[1],
                            stringsAsFactors = FALSE)
    snotel.df <- full_join(snotel.df, fullDates,
                                by=c("Date")) %>%
      arrange(Date)
  }
  
  snotel.df$year.fact<-as.factor(snotel.df$year)
  snotel_ls<<-split(snotel.df, snotel.df$year.fact)
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


watershed_allyears<-streamflow_analysis(siteNumber, parameterCd, startDate, endDate)
watershed_allyears.ls<-lapply(watershed_allyears, function(y) eachyear(y))

streamflow_metrics<-data.frame(time= 1:22,year=NA, springonset_date=as.Date(NA), peakflow_cfs=NA, meanslope=NA, meanslope_ma=NA)

for (i in 1:length(watershed_allyears.ls)) {
  streamflow_metrics$year[i]<-watershed_allyears.ls[[i]]$year[1]
  
  date<-watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$perchange >= 1 & watershed_allyears.ls[[i]]$month > 2  ,]$Date[1]
  streamflow_metrics$springonset_date[i]<- date[1]
  
  peakflow<-max(watershed_allyears.ls[[i]]$discharge_cfs)
  streamflow_metrics$peakflow_cms[i]<-peakflow*0.0283168
  streamflow_metrics$peakflow_cfs[i]<-peakflow
  
  #Find mean slope from start to peak flow 
  streamflow_metrics$meanslope[i]<- (peakflow - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$discharge_cfs)/ 
    (watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$discharge_cfs == peakflow, ]$doy - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  max_ma30<-max(watershed_allyears.ls[[i]]$ma30, na.rm=T)
  streamflow_metrics$max_ma30[i]<-max_ma30
  
  streamflow_metrics$meanslope_ma[i]<- (max_ma30 - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == date, ]$ma30)/(na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$ma30 == max_ma30, ]$doy) - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  streamflow_metrics$annual_tot[i]<-sum(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  streamflow_metrics$annual_mean[i]<-mean(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  
  
  
} #moving average mean slope is still not working 

plot(streamflow_metrics$year, streamflow_metrics$peakflow_cfs, type = "l", xlab="Time", ylab='Streamflow (cfs)')
abline(v= fire_year, col="red", lwd =3)
