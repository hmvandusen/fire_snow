

knitr::opts_chunk$set(echo = TRUE)
#install.packages('dataRetrieval')
library(dataRetrieval) # packaage for downloading USGS stream flow data
#install.packages('lubridate')
library(lubridate) # date management
library(forecast) 
library(stlplus) 
library(fpp) 
library(dplyr) 
library(reshape2) 
library(data.table) #rbind list function
library(ggplot2)
library(viridis)
library(grDevices)
library(cowplot)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 



#####
#Objects that change:: additional object to change and save product at bottom of script
watershdfire.df <- read.csv('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/watersheds_unburned.csv', sep=',', header = T)

#adding leading 0 to ID values that are less than 8 digets
watershdfire.df$GAGE_ID<-ifelse(floor(log10(watershdfire.df$GAGE_ID)) + 1 < 8, paste0("0", watershdfire.df$GAGE_ID), watershdfire.df$GAGE_ID)
#create a list for stream metrics dataframe
streamflow_metrics.ls<-list(NA)



for (gage in 1:dim(watershdfire.df)[1]) { #

  tryCatch({
# Pull in gage for fire identifying parameters for data download
siteNumber <- watershdfire.df$GAGE_ID[gage] # USGS gauge number
#fire_year <- watershdfire.df$year[gage] 


#window to plot in function: suggested window year 1 after fire 
plot_date<- c("2021-01-01", "2021-12-31")
parameterCd <- "00060"  # mean daily discharge in cfs
startDate <- "1959-10-01" # period of record of MODIS
endDate <- "2022-09-30" # Current year

#### Cleaning in Gage data 

# download data using readNWISdv function
streamflow_analysis<-function(siteNumber, parameterCd, startDate, endDate){
watershed_name <- dataRetrieval::readNWISdv(siteNumber, parameterCd, startDate, endDate)
head(watershed_name) # check
colnames(watershed_name)[4]<- 'discharge_cfs'
watershed_name<-watershed_name[,-5]

#Making time series in water year
#I am separating the date for each column 
watershed_name$year <-lubridate::year(watershed_name$Date) # add year col
watershed_name$month <-lubridate::month(watershed_name$Date) # add month col
watershed_name$day <-lubridate::day(watershed_name$Date) # add day of month col
watershed_name$doy <-lubridate::yday(watershed_name$Date) # add day of year col
watershed_name<-addWaterYear(watershed_name)

#Plots of all years from 2000 - 2021 and add in vertical line for fire
#plot(watershed_name$Date, watershed_name$discharge_cfs, type='l', ylab='Discharge (cfs)', xlab='Time (days)')
#abline(v=as.Date(fire_year), col='red')

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(watershed_name$Date))) != (nrow(watershed_name)+1)){
  fullDates <- seq(from=min(watershed_name$Date),
                   to = max(watershed_name$Date), by="1 day")
  fullDates <- data.frame(Date = fullDates, 
                          agency_cd = watershed_name$agency_cd[1],
                          site_no = watershed_name$site_no[1],
                          stringsAsFactors = FALSE)
  watershed_name <- full_join(watershed_name, fullDates,
                               by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

watershed_name$year.fact<-as.factor(watershed_name$waterYear)
watershed_ls<<-split(watershed_name, watershed_name$year.fact)
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

###Application of functions 

watershed_allyears<-streamflow_analysis(siteNumber, parameterCd, startDate, endDate)
watershed_allyears.ls<-lapply(watershed_allyears, function(y) eachyear(y))


#Creating a usable dataframe and following for loop
nyear<-length(watershed_allyears.ls)
#streamflow_metrics<-data.frame(time= 1:22,year=NA, springonset_date=as.Date(NA), peakflow_cfs=NA, meanslope=NA, meanslope_ma=NA)

streamflow_metrics<-data.frame(time= 1960:2022, year=NA, 
                               springonset_date=as.Date(NA), peakflow_cfs=NA, 
                               meanslope=NA, meanslope_ma=NA, annual_tot = NA, 
                               annual_mean=NA, baseflow=NA, peakflow_cms=NA, max_ma30 =NA
)

for (i in 1:length(watershed_allyears.ls)) {
  
  numrow<-nrow(streamflow_metrics[streamflow_metrics$time <= names(watershed_allyears.ls)[i],])
  streamflow_metrics[numrow,]$year<-watershed_allyears.ls[[i]]$waterYear[i]
  
  date<-na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$perchange >= 1 & watershed_allyears.ls[[i]]$month > 2 & watershed_allyears.ls[[i]]$month < 10 ,])$Date[1]
  streamflow_metrics$springonset_date[numrow]<- date[1]
  
  peakflow<-max(watershed_allyears.ls[[i]]$discharge_cfs)
  streamflow_metrics$peakflow_cms[numrow]<-peakflow*0.0283168
  streamflow_metrics$peakflow_cfs[numrow]<-peakflow
  
  #Find mean slope from start to peak flow 
  streamflow_metrics$meanslope[numrow]<- (peakflow - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$discharge_cfs)/ 
    (watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$discharge_cfs == peakflow, ]$doy - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  max_ma30<-max(watershed_allyears.ls[[i]]$ma30, na.rm=T)
  streamflow_metrics$max_ma30[numrow]<-max_ma30
  
  streamflow_metrics$meanslope_ma[numrow]<- (max_ma30 - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == date, ]$ma30)/(na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$ma30 == max_ma30, ]$doy) - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  streamflow_metrics$annual_tot[numrow]<-sum(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  streamflow_metrics$annual_mean[numrow]<-mean(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  
  
  streamflow_metrics$baseflow[numrow]<-median(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$month == 8,]$discharge_cfs)*0.0283168
  
  
}

#streamflow_metrics$before_after_f<- as.factor(with(streamflow_metrics, ifelse( year <= fire_year, 'prefire', 
                                                                               #ifelse(year > fire_year & streamflow_metrics$year <= fire_year+3, 'postfire_3yr', 
                                                                                      #'postfire'))))
streamflow_metrics$springonset_md<-as.Date(format(streamflow_metrics$springonset_date, format="%m-%d"), "%m-%d")

streamflow_metrics$GAGE_ID <- siteNumber
#streamflow_metrics$fire_year <- fire_year
streamflow_metrics$snow <- watershdfire.df$snow[gage]
#streamflow_metrics$percentageFire <- watershdfire.df$percentageFire[gage]
#streamflow_metrics$Hseverity <- watershdfire.df$Hseverity[gage]
#streamflow_metrics$meanQtotal<-mean(na.omit(streamflow_metrics[streamflow_metrics$before_after_f == 'prefire',]$annual_tot))
#streamflow_metrics$meanpeakflow_cms<-mean(na.omit(streamflow_metrics[streamflow_metrics$before_after_f == 'prefire',]$peakflow_cms))



streamflow_metrics.ls[[gage]]<- streamflow_metrics
rm(list=ls(pattern="watershed_"))}, 

  error = function(e){
    message(paste("An error occurred for item", gage, conditionMessage(e),":\n"), e)})
} #dim(watershdfire.df)[1]

#These gages are not active after 2000
numbers<-which(lengths(streamflow_metrics.ls) == 0)
inactive_gages<-vector(mode = 'character')
for (i in 1:length(numbers)){
  inactive_gages[i]<-watershdfire.df$GAGE_ID[numbers[i]]
}

for (w in 1:length(watershed.lsna)){
for (i in 1:length(2000:2019)){
  y$before_after_fire<- ifelse(y$wyr > i, 'post_fire', 'pre_fire')
}
}
#condense list to dataframe
streamflow_metrics<-rbindlist(streamflow_metrics.ls)
streamflow_metrics$GAGE_ID.fact<-as.factor(streamflow_metrics$GAGE_ID)
by(streamflow_metrics, INDICES = streamflow_metrics[,'GAGE_ID.fact'], function(y) mean(y[y$before_after_f == 'prefire',]$annual_tot))

#Cleaning data
#Removing NULL lists 
streamflow_metrics.lsna<-Filter(function(x) length(x) > 0, streamflow_metrics.ls) #currently 214 watersheds
#lapply(streamflow_metrics.lsna, function(x) boxplotfun(x)) # This just plots all list objects

#Trying to figure out which data frames have all 3 fire status (I could combine into an lapply ifesle statement)
firestate<-lapply(streamflow_metrics.lsna, function(x) nlevels(x[['before_after_f']]) == 3)
streamflow_metrics_prepost <- streamflow_metrics.lsna
for (i in 1:length(firestate)) {
  if(firestate[[i]]==TRUE){
    streamflow_metrics_prepost[i][[1]] <- streamflow_metrics.lsna[i][[1]]
  }else{
    streamflow_metrics_prepost[i][[1]] <- 999
  }
  
}

#which ones do not have pre,3yr,post statements
which(lengths(streamflow_metrics_prepost) == 1)
streamflow_metrics_prepost<-Filter(function(x) length(x) > 1, streamflow_metrics_prepost) #144 watersheds
lapply(streamflow_metrics_prepost, function(x) boxplotfun(x)) #Visual aid to see if there is data from pre and post fire

#Scale values 
streamflow_metrics_scaled<-streamflow_metrics_prepost
scale_fun <- function(x) {scale(x[c("peakflow_cfs","meanslope","meanslope_ma", "annual_tot" ,"annual_mean", "baseflow" , "peakflow_cms","max_ma30")])}
for (i in 1:length(streamflow_metrics_scaled)) {
  streamflow_metrics_scaled[[i]][c("peakflow_cfs","meanslope","meanslope_ma", "annual_tot" ,"annual_mean","baseflow" , "peakflow_cms","max_ma30")]<-
    scale_fun(streamflow_metrics_scaled[[i]])
}

###DONE WITH CLEANING DATA
###SAVE streamflow_metrics table as a more specific name!####
#Example: 
streamflow_metrics_df<-rbindlist(streamflow_metrics_prepost)
write.csv(streamflow_metrics_df,'~/Desktop/EcoForecasting/streamflow_metrics.csv')

streamflow_metrics<-read.csv('streamflow_metrics.csv')

percentfire_snow.df<-data.frame(length=1:100,percentsnow=NA, percenfire=NA)
df_raw<-rbindlist(streamflow_metrics_scaled)
df_raw$GAGE_ID.f<-as.factor(df_raw$GAGE_ID)
df_raw$snow_per<- df_raw$snow*100


#Line plot by modualting metrics with percent snow
metricplots <- function(metric){
#calculating one metric for different levels of fire and snow in watershed: 
for (i in 1:10) {
  for (j in 1:10) {
    df<-df_raw[df_raw$percentageFire >= (i*10) & df_raw$snow >= (j*.1),]
    #Add number of watersheds
    percentfire_snow.df$samplesize[(10*i - 10 + j)]<-length(unique(df$GAGE_ID.f))
    #calculate mean and standard deviation
    prefiremean<-mean(na.omit(df[df$before_after_f =='prefire',][[metric]]))
    postfire3yrmean<-mean(na.omit(df[df$before_after_f =='postfire_3yr',][[metric]]))
    prefiresd<-sd(na.omit(df[df$before_after_f =='prefire',][[metric]]))
    postfiresd<-sd(na.omit(df[df$before_after_f =='postfire_3yr',][[metric]]))
    percentfire_snow.df$percentsnow[(10*i - 10 + j)]<-j*.1
    percentfire_snow.df$percenfire[(10*i - 10 + j)]<-i*10
    percentfire_snow.df[[metric]][(10*i - 10 + j)]<-postfire3yrmean - prefiremean
    percentfire_snow.df$sd[(10*i - 10 + j)] <- sqrt(prefiresd + postfiresd)
    #Add number of watersheds
  }
}
percentfire_snow.df$percentsnow.f<-as.factor(percentfire_snow.df$percentsnow)
percentfire_snow.df$percentfire.f <-as.factor(percentfire_snow.df$percenfire)

#point plots  
plot1 <- ggplot(percentfire_snow.df, aes_string(x= "percenfire", y= metric, color="percentsnow.f"))  +
  geom_point(alpha=.7)  + 
  geom_line(aes(size = samplesize)) +
  #geom_errorbar(aes(ymin=differexnce-sd, ymax=difference+sd), width=.2,
                #position=position_dodge(0.05)) +
  scale_colour_brewer(palette = "RdYlBu") +
  ylab(expression(paste(Delta, metric))) +
  xlab("Percent of watershed Burned") +
  theme_cowplot(12) 

plot1 + theme(
        axis.text=element_text(size=40),
        axis.title=element_text(size=48,face="bold"), 
        plot.margin = margin(t = 20,  # Top margin
                             r = 80,  # Right margin
                             b = 80,  # Bottom margin
                             l = 80), 
        legend.text = element_text(size=28), 
        legend.title = element_text(size = 30),
        legend.key.size = unit(.1, 'cm')) +
  labs(color = 'Percent Snow \nin Watershed', size = 'Sample size \n(number of watersheds)') +
  guides(color=guide_legend(ncol=2, override.aes = list(size = 6)))
}
metrics<-names(df_raw)[4:11]
for (i in 1:length(metrics)) {
  print(metricplots(metrics[i]))
}



boxplotfun<-function(y,title){
  p<-ggplot(y %>% filter(!is.na(before_after_f)), 
            aes(x=before_after_f, y=springonset_md, fill=before_after_f)) + 
    geom_boxplot(alpha=0.7, lwd = 1) +
    theme(legend.position="none") +
    scale_fill_brewer(palette="RdYlBu") +
    xlab( 'Time relative to fire') + ylab(title) +
    theme_cowplot(12) +
    theme(axis.text=element_text(size=32),
        axis.title=element_text(size=40,face="bold"), 
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 30))
  p
}

##point range function
df_raw$firesnow50<-as.factor(with(df_raw, ifelse(snow >= 0.5, ">50snow", "<50snow"))) #percentageFire >= 50 & 
pointfun<-function(y,title){
  e <- ggplot(df_raw %>% filter(!is.na(before_after_f)), 
              aes(x=factor(before_after_f, level = c('prefire', 'postfire_3yr', 'postfire')), 
                  y=springonset_md)) + 
    stat_summary(
      aes(color = firesnow50),
      fun.data = "mean_cl_normal", 
      #fun = mean,  fun.min = min, fun.max = max, 
      geom = "pointrange",  size = 4,  
      position = position_dodge(0.4))
  
  e +
    #scale_fill_brewer(palette="RdYlBu") +
    xlab( 'Time after fire') + ylab('Spring melt date') +
    labs(color = 'Percent burn in snow zone\nwith 95% CI') +
    scale_color_manual(labels = c("< 50% snow zone area", "> 50% snow zone area"), 
                       values = c("orangered3", "steelblue3")) +
    scale_x_discrete(labels = c('Pre-fire','3 years','3+ years'))+
    theme_cowplot(12) +
    theme(
      axis.text=element_text(size=40),
      axis.title=element_text(size=48,face="bold"), 
      plot.margin = margin(t = 20,  # Top margin
                           r = 80,  # Right margin
                           b = 80,  # Bottom margin
                           l = 80), 
      legend.text = element_text(size=28), 
      legend.title = element_text(size = 30),
          legend.key.size = unit(2, 'cm'))
}


#Density funtion 
watershdfire.df$snow_per<-watershdfire.df$snow*100
df_density<-watershdfire.df[,c('percentageFire', "snow_per")]
df_density<-melt(df_density)
d <- ggplot(df_density, aes(x = value, fill= variable))+
  geom_density(aes(y = stat(count), alpha = 0.5))
d

##
##Distribution of watershed in burned percentage and snow percentage
##
df_percent <- watershdfire.df
df_percent$snow_per<-df_percent$snow*100
df_percent<-df_percent[,c('percentageFire', "snow_per")]
breaks<-c(0,20,40,60,80,100)
df_percent$snow_cut<-cut(df_percent$snow_per, breaks = breaks, include.lowest = T, right = F)  #add labels
#df_percent$fire_cut<-cut(df_percent$percentageFire, breaks = breaks, include.lowest = T, right = F)

#GGplot
p <- ggplot(df_percent, aes(x = percentageFire, fill = snow_cut))
p + geom_area(bins = 10, stat = 'bin') +theme_cowplot(12) + 
  scale_colour_brewer(palette = 'Blues')+
  xlab( 'Percent of Watershed Burned') + ylab('Number of Watersheds') +
  labs(fill = 'Percent burn in snow zone') +
  scale_fill_brewer( palette = 'RdYlBu')+
  theme_cowplot(12) +
  theme(
    axis.text=element_text(size=40),
    axis.title=element_text(size=48,face="bold"), 
    plot.margin = margin(t = 20,  # Top margin
                         r = 80,  # Right margin
                         b = 80,  # Bottom margin
                         l = 80), 
    legend.text = element_text(size=28), 
    legend.title = element_text(size = 30),
    legend.key.size = unit(2, 'cm')) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) 
    #scale_y_continuous(expand = c(0, 0), limits = c(0, 100))


  
percentseverity_snow.df<-data.frame(length=1:100,percentsnow=NA, percentHseverity=NA)  
severityplots <- function(metric){
    #calculating one metric for different levels of fire and snow in watershed: 
    for (i in 1:10) {
      for (j in 1:10) {
        df<-df_raw[df_raw$Hseverity >= (i*.1) & df_raw$snow >= (j*.1),]
        #Add number of watersheds
        percentseverity_snow.df$samplesize[(10*i - 10 + j)]<-length(unique(df$GAGE_ID.f))
        #calculate mean and standard deviation
        prefiremean<-mean(na.omit(df[df$before_after_f =='prefire',][[metric]]))
        postfire3yrmean<-mean(na.omit(df[df$before_after_f =='postfire_3yr',][[metric]]))
        prefiresd<-sd(na.omit(df[df$before_after_f =='prefire',][[metric]]))
        postfiresd<-sd(na.omit(df[df$before_after_f =='postfire_3yr',][[metric]]))
        percentseverity_snow.df$percentsnow[(10*i - 10 + j)]<-j*.1
        percentseverity_snow.df$percentHseverity[(10*i - 10 + j)]<-i*.1
        percentseverity_snow.df[[metric]][(10*i - 10 + j)]<-postfire3yrmean - prefiremean
        percentseverity_snow.df$sd[(10*i - 10 + j)] <- sqrt(prefiresd + postfiresd)
        #Add number of watersheds
      }
    }
    percentseverity_snow.df$percentsnow.f<-as.factor(percentseverity_snow.df$percentsnow)
    percentseverity_snow.df$percentfHseverity.f <-as.factor(percentseverity_snow.df$percentHseverity)
    
    #point plots  
    plot1 <- ggplot(percentseverity_snow.df[1:50,], aes_string(x= 'percentHseverity', y= metric, color='percentsnow.f'))  +
      geom_point(alpha=.7)  + 
      geom_line(aes(size = samplesize)) +
      scale_colour_brewer(palette = "RdYlBu") +
      ylab(expression(paste(Delta, metric))) +
      xlab("Percent of high severity burn") +
      theme_cowplot(12) 
    
    plot1 + theme(
      axis.text=element_text(size=40),
      axis.title=element_text(size=48,face="bold"), 
      plot.margin = margin(t = 20,  # Top margin
                           r = 80,  # Right margin
                           b = 80,  # Bottom margin
                           l = 80), 
      legend.text = element_text(size=18), 
      legend.title = element_text(size = 20),
      legend.key.size = unit(.1, 'cm')) +
      labs(color = 'Percent Snow \nin Watershed', size = 'Sample size \n(number of watersheds)') +
      guides(color=guide_legend(ncol=2, override.aes = list(size = 6)))
  }
metrics<-names(df_raw)[4:11]
for (i in 1:length(metrics)) {
  print(severityplots(metrics[i]))
}

metric = metrics[8]

##
##
##Plots for all Years
####


#Notes:
#These plots are for testing 
#Wrong year on springonset_md, but it don't matter for plotting purposes: just want month and day
#Red Post fire, Blue Pre-Fire
plot(streamflow_metrics$springonset_md, streamflow_metrics$meanslope, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     pch=19,  xlab = "Day of spring onset", ylab="Mean hydrograph slope (cfs/day)", cex= 4)
plot(streamflow_metrics$peakflow_cfs, streamflow_metrics$meanslope, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     xlab = "Peak streamflow (cfs)", ylab="Mean hydrograph slope", pch = 19)
plot(streamflow_metrics$springonset_md, streamflow_metrics$peakflow_cfs, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     xlab = "Day of spring onset", ylab="Peak streamflow (cfs)", pch = 19, cex=4)

#Plots with gradient color
#Darker colors mean year closer to fire
library(RColorBrewer)
precols<-brewer.pal(4,"Blues")
postcols<-rev(brewer.pal(4,"Reds"))
prepal<-colorRampPalette(precols)
postpal<-colorRampPalette(postcols)

par(bg="white") # This series of code makes just plot back ground grey and outside white: for better contrast
plot(streamflow_metrics$springonset_md, streamflow_metrics$meanslope,
     xlab = "Day of spring onset", 
     ylab="Mean hydrograph slope (cfs/day)")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey")
par(new = TRUE)
points(streamflow_metrics$springonset_md, streamflow_metrics$meanslope, 
     col = ifelse(streamflow_metrics$year>fire_year, 
     postpal(10)[as.numeric(cut(streamflow_metrics[streamflow_metrics$year>fire_year,]$year, breaks = 10))],
     "turquoise3"), 
     pch=19, cex=2)
##gradient pre fire colors:: prepal(10)[as.numeric(cut(streamflow_metrics[streamflow_metrics$year<fire_year,]$year, breaks = 10))]) 


#Bar plot of mean hydro graph slope, colored by pre and post fire
streamflow_metrics_slopeordered<-streamflow_metrics[order(streamflow_metrics$meanslope, decreasing = T),]
barplot(streamflow_metrics_slopeordered$meanslope, breaks = 50, col = ifelse(streamflow_metrics_slopeordered$year>fire_year, 
    postpal(10)[as.numeric(cut(streamflow_metrics_slopeordered[streamflow_metrics_slopeordered$year>fire_year,]$year, breaks = 10))],
    prepal(10)[as.numeric(cut(streamflow_metrics_slopeordered[streamflow_metrics_slopeordered$year<fire_year,]$year, breaks = 10))]),
     xlab = "Mean slope")

#Table of top 6 years with the largest mean slope
head(streamflow_metrics_slopeordered)

#Barplot of top years 
streamflow_metrics_3y<-streamflow_metrics[streamflow_metrics$year>(fire_year-4) & streamflow_metrics$year<(fire_year+4),]
barplot(streamflow_metrics_3y$springonset_date, breaks = 50, col = ifelse(streamflow_metrics_3y$year>fire_year, 
      postpal(3)[as.numeric(cut(streamflow_metrics_3y[streamflow_metrics_3y$year>fire_year,]$year, breaks = 3))],
      prepal(4)[as.numeric(cut(streamflow_metrics_3y[streamflow_metrics_3y$year<fire_year,]$year, breaks = 4))]),
      xlab = "Mean slope")


#creating a factor for before fire and 3 years after fire

streamflow_metrics$before_after_f<- as.factor(with(streamflow_metrics, ifelse( year <= fire_year, 'prefire', 
                                                                               ifelse(year > fire_year & streamflow_metrics$year <= fire_year+3, 'postfire_3yr', 
                                                                                      'postfire'))))

boxplot(springonset_md ~ before_after_f, data = streamflow_metrics.ls[[255]])
boxplot(meanslope ~ before_after_f, data = streamflow_metrics, ylim = c(0,7))
boxplot(meanslope_ma ~ before_after_f, data = streamflow_metrics)
boxplot(peakflow_cms ~ before_after_f, data = streamflow_metrics)






ggplot(percentfire_snow.df, aes(x = percenfire, y = difference, color=percentsnow.f)) +
  geom_point(alpha=.5, , size = 5)  + custom_colors +
  ylab("Change in Normalized Q (prefire to 3yr post fire)") +
  xlab("Percent of watershed Burned")


myColors <- brewer.pal(10, "Spectral")
names(myColors) <- levels(percentfire_snow.df$percentsnow.f)
custom_colors <- scale_colour_manual(name = "Percent Snow", values = myColors)
par(mar = c(6, 7, 5, 4))
plot(
  difference ~ percenfire, 
  data = percentfire_snow.df, 
  ylab ="Change of Discharge (Q)",
  xlab = "Percent fire in watershed", 
  cex = 2, 
  pch = 16,
  col = myColors, 
  cex.lab = 2,
  cex.axis= 2, 
  #lwd.axis = 2, 
  xaxp = c(10, 100, 9), yaxp = c(.1, 1, 9)
)
legend('bottomright', 
       legend = levels(percentfire_snow.df$percentsnow.f),
       col = myColors, pch = 16, cex = .5, bty = 'o', box.lwd = 3, ncol = 2,
       title)

snowzone_samples<-data.frame(length=1:10)
for (j in 1:10) {
  df<-df_raw[df_raw$snow_per >= (j* 10),]
  #Add number of watersheds
  snowzone_samples$samplesize[j]<-length(unique(df$GAGE_ID.f))
  snowzone_samples$percent_snowzone[j]<-j*10
}

snowzone_samples<-data.frame(length=1:10)
for (j in 1:10) {
  df<-df_raw[df_raw$snow_per >= (j* 10),]
  #Add number of watersheds
  snowzone_samples$samplesize[j]<-length(unique(df$GAGE_ID.f))
  snowzone
  
