library(readr)


snotel_middleforkCO <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport,metric/daily/start_of_period/1014:CO:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/PREC::value", 
                                  +     comment = "#")

head(snotel_middleforkCO)

yearly_snotel <- read_csv("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport,metric/annual_calendar_year/start_of_period/1014:CO:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/PREC::value", 
                          comment = "#")

names(yearly_snotel)[1]<-'year'
streamflow_metrics_snotel_snotel<-full_join(streamflow_metrics_snotel, yearly_snotel)
streamflow_metrics_snotel_snotel$peakratio<-streamflow_metrics_snotel_snotel$peakratio/streamflow_metrics_snotel_snotel$`Precipitation Accumulation (mm) Start of Month Values`

par(bg="white") # This series of code makes just plot back ground grey and outside white: for better contrast
plot(streamflow_metrics_snotel$springonset_md, streamflow_metrics_snotel$meanslope,
     xlab = "Day of spring onset", 
     ylab="Mean hydrograph slope (cfs/day)")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey")
par(new = TRUE)
points(streamflow_metrics_snotel$springonset_md, streamflow_metrics_snotel$meanslope, 
       col = ifelse(streamflow_metrics_snotel$year>fire_year, 
                    postpal(10)[as.numeric(cut(streamflow_metrics_snotel[streamflow_metrics_snotel$year>fire_year,]$year, breaks = 10))],
                    "turquoise3"), 
       pch=19, cex=2)
##gradient pre fire colors:: prepal(10)[as.numeric(cut(streamflow_metrics_snotel[streamflow_metrics_snotel$year<fire_year,]$year, breaks = 10))]) 


#Bar plot of mean hydro graph slope, colored by pre and post fire
streamflow_metrics_snotel_slopeordered<-streamflow_metrics_snotel[order(streamflow_metrics_snotel$meanslope, decreasing = T),]
barplot(streamflow_metrics_snotel_slopeordered$meanslope, breaks = 50, col = ifelse(streamflow_metrics_snotel_slopeordered$year>fire_year, 
                                                                             postpal(10)[as.numeric(cut(streamflow_metrics_snotel_slopeordered[streamflow_metrics_snotel_slopeordered$year>fire_year,]$year, breaks = 10))],
                                                                             prepal(10)[as.numeric(cut(streamflow_metrics_snotel_slopeordered[streamflow_metrics_snotel_slopeordered$year<fire_year,]$year, breaks = 10))]),
        xlab = "Mean slope")
