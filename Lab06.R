##Start of tutorial 
ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")
unique(ETdat$crop)
##install.packages("lubridate")
##install.packages("ggplot2")
##install.packages("forecast")
##install.packages("dplyr")

library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
##9.4.1 Setting up time series data type 
#this next group of code first starts by creating "almond", which includes a summary of the ET value by date for just almond. 
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

##visualize data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")
##we see that there is strong seasonality, which will be a big part of working with this time series! 

##next is forming the data as a time series object. 
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1),
                frequency= 12)

#section 9.4.2 decomposing time series. Remember that this will break up the TS into trend, seasonality, error 
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)
acf(na.omit(almond_ts), # remove missing data
    lag.max = 24)

pacf.plot <- pacf(na.omit(almond_ts))

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0))
model1

almond_y <- na.omit(almond_ts)
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")
#Forecasting!
newAlmond <- forecast(model4)
newAlmond

newAlmondF <- data.frame(newAlmond)

#sort of unclear what's happening here...
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
