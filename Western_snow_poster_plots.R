##
## Plots for streamflow metrics
##
percentfire_snow.df<-data.frame(length=1:100,percentsnow=NA, percenfire=NA)
df_raw<-rbindlist(streamflow_metrics_scaled) #make list into dataframe
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
  print(metricplots(metrics[3]))
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