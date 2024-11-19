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

###homework problem 2 
# average fields for each month for almonds has already been done ("almond")
#for pistachios 
pistachios <- ETdat %>% 
  filter(crop == "Pistachios") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

#fallow/idle fields 
fallow <- ETdat %>% 
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))

##corn 
corn <- ETdat %>% 
  filter(crop == "Corn") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))


##table grapes 
grapes <- ETdat %>% 
  filter(crop == "Grapes (Table/Raisin)") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm = TRUE))


##create TS objects 
fallow_ts <- ts(fallow$ET.in, 
                    start = c(2016,1),
                    frequency = 12)

pistachios_ts <- ts(pistachios$ET.in, 
                start = c(2016,1),
                frequency = 12)

corn_ts <- ts(corn$ET.in, 
                    start = c(2016,1),
                    frequency = 12)


grapes_ts <- ts(grapes$ET.in, 
              start = c(2016,1),
              frequency = 12)

#next part of the problem: pistachios 
#decompose the time series, means that we extract the trend, seasonality, and random error 
plot(almond_dec)
pistachios_dec <- decompose(pistachios_ts)
plot(pistachios_dec)

fallow_dec <- decompose(fallow_ts)
plot(fallow_dec)

corn_dec <- decompose(corn_ts)
plot(corn_dec)

grapes_dec <- decompose(grapes_ts)
plot(grapes_dec)

## question 3 
pistachios_y <- na.omit(pistachios_ts)
model1P <- arima(pistachios_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1P
#run the ARIMA for 4th order 
model4P <- arima(pistachios_y , # data 
                 order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4P
##maybe 2nd order would work better?
model2P <- arima(pistachios_y , # data 
                 order = c(2,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model2P

#make the plots to see which works best!
# calculate fit
AR_fit1P <- pistachios_y - residuals(model1P) 
AR_fit4P <- pistachios_y - residuals(model4P)
AR_fit2P <- pistachios_y - residuals(model2P)
#plot data
plot(pistachios_y)
# plot fit
points(AR_fit1P, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit2P, type = "l", col = "blue3", lty = 2, lwd=2)
points(AR_fit4P, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","blue3", "darkgoldenrod4"),
       bty="n")


#seems like the 4th order works best! That means that evapotranspiration from the past 1-4 months effects current values. 

#time to do it again with fallow fields! 
# calculate fit
fallow_y <- na.omit(fallow_ts)
model1F <- arima(fallow_y , # data 
                 order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1F
#run the ARIMA for 4th order 
model4F <- arima(fallow_y , # data 
                 order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4F
##maybe 2nd order would work better?
model2F <- arima(fallow_y , # data 
                 order = c(2,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model2F
AR_fit1F <- fallow_y - residuals(model1F) 
AR_fit4F <- fallow_y - residuals(model4F)
AR_fit2F <- fallow_y - residuals(model2F)
#plot data
plot(fallow_y)
# plot fit
points(AR_fit1F, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit2F, type = "l", col = "blue3", lty = 2, lwd=2)
points(AR_fit4F, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","blue3", "darkgoldenrod4"),
       bty="n")


##seems like the fourth order is the best fit.
newFallow <- forecast(model4F)
newFallow

#next step is to format a date and data frame to plot this! 
newFallowDF <- data.frame(newFallow)
years <- c(rep(2021,4), rep(2022,12), rep(2023, 8)) ## what do the second numbers mean?
newFallowDF$dateF <- ymd(paste(years, "/", month, "/", 1))

##make a plot!!
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]),newFallowDF$dateF[24])+  
  geom_line(data = newFallowDF, aes(x = dateF, y = Point.Forecast),
            col="blue4") +  # Plotting model forecasts
  geom_ribbon(data= newFallowDF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#perfect. now do it again with fallow/idle fields 



#forecast for pistachios! 
newPistachio <- forecast(model4P)
newPistachio

#next step is to format a date and data frame to plot this! 
newPistachioDatFrame <- data.frame(newPistachio)
years <- c(rep(2021,4), rep(2022,12), rep(2023, 8)) ## what do the second numbers mean?
newPistachioDatFrame$dateF <- ymd(paste(years, "/", month, "/", 1))

##make a plot!!
ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachios$date[1]),newPistachioDatFrame$dateF[24])+  
  geom_line(data = newPistachioDatFrame, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data= newPistachioDatFrame, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#perfect. now do it again with fallow/idle fields 