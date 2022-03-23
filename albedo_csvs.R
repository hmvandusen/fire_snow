library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


albedowshdtst <- read.csv("~/Downloads/albedowshdtst.csv")

albedowshdtst$month<-lubridate::month(albedowshdtst$date)
albedowshdtst$year<-lubridate::year(albedowshdtst$date)
albedowshdtst$year.f<-as.factor(albedowshdtst$year)
albedowshdtst$year_num<-as.numeric(albedowshdtst$year.f) # making year a number factor


albedo_wintersp <-albedowshdtst[albedowshdtst$month == 1  | albedowshdtst$month == 2   | albedowshdtst$month == 3,]
albedo_winterw <- albedowshdtst[albedowshdtst$month == 12,]
albedo_ablation<-albedowshdtst[albedowshdtst$month == 3  | albedowshdtst$month == 4   | albedowshdtst$month == 5,]

#is the mean of median the right value?
albedo_mean<-as.vector(1:max(albedowshdtst$year_num))
for (i in 1:(max(albedowshdtst$year_num))) {
  albedo_mean[i]<-median(c(albedo_wintersp[albedo_wintersp$year_num == i,]$mean, albedo_winterw[albedo_winterw$year_num == (i - 1),]$mean))
}

albedo.df<- data.frame(year = levels(albedowshdtst$year.f), albedo_mean = albedo_mean, abalation=NA)

for (i in 1:(max(albedowshdtst$year_num))) {
  albedo.df$abalation[i]<-median(albedo_ablation[albedo_ablation$year_num == i,]$mean)
}

plot(albedo.df$year, albedo.df$albedo_mean, type = 'l')
abline(v = '2003.5', col ='red', lwd=3)
plot(albedo.df$year, albedo.df$abalation, type = 'l', add=T)
abline(v = '2003.5', col ='red', lwd=3)

plot(as.Date(albedowshdtst$date), albedowshdtst$mean, type='l')
abline(v = as.Date('2007-07-17'), col='red', lwd=3)


