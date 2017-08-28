rm(list=ls())


library(fpp) # forecasting 
library(astsa) # advanced stastical time series analysis
library(forecast) #forecasting methodologies
library(quantmod) #for obtaining stock prices from yahoo
library(ggplot2) # to visualize data
library(plotly)  #for making charts
library(ggfortify) # used in conjunction with ggplot
library(tseries) # time series package
library(gridExtra) # plotting multiple graphs
library(zoo) # for carrying out merging on dataframes
library(plyr) # for merging dataframe 

# Function for merging datafrmes to aid in Plotitng
MergeDF <- function(dn,df){
  require(zoo)
  ds <-as.data.frame(window(dn))
  names(ds) <- 'observed'
  ds$month <- as.yearmon(time(window(dn)))
  
  dfit<-as.data.frame(df$fitted)
  dfit$month<-as.yearmon(time(df$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T)
  ds[,'forecast'] <- NA
  ds[,'lo80'] <- NA
  ds[,'hi80'] <- NA
  ds[,'lo95'] <- NA
  ds[,'hi95'] <- NA
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  fcastn<-as.data.frame(df)
  fcastn$Month<-as.yearmon(row.names(fcastn))
  names(fcastn)<-c('forecast','lo80','hi80','lo95','hi95','month')
  
  library(plyr) # for combining dataframe row wise
  pd<-rbind.fill(ds,fcastn)
  return(pd)
  
}


startDate = as.Date("2006-01-01") 

endDate = as.Date("2016-12-31")

#getting data from yahoo finance using start and end date
getSymbols("^GSPC", from = startDate, to = endDate,src="yahoo")

# convert data into monthly wise
GSPC_monthly <- to.monthly(GSPC)
monthly <- as.vector(GSPC_monthly$GSPC.Adjusted)

#data is split month wise from 2006 to 2016
SNP16<- ts(monthly,c(2006,1),frequency=12)
SNP16

Obersved <- 

#plot of data from  2006 to 2016
S <- autoplot(SNP16, 
              main = "Plot of S&P 500 Time Series(2006-2016)", 
              ts.colour = "turquoise4", size=1.25) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  labs(x = "Year", y = "Adjusted Price")
ggplotly(S)


#data is split month wise from 2006 to 2015 
SNP15<- ts(monthly,c(2006,1),c(2015,12),frequency=12)

#plot of data from  2006 to 2015
N <- autoplot(SNP15, 
              main = "Plot of S & P 500 Time Series(2006-2015)",
              ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.ticks  = element_blank(),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  labs(x = "Year", y = "Adjusted Price")
ggplotly(N)


#-------------------------------------------Decomposition of the SNP15 data --------------------------------------------------
# By decomposing data , we are breaking into seasonal, trend and noise components 
# The remainder/noise is then the original data minus the seasonal and trend components.
# We are decomposing our data by setting seasonality window as 'periodic'
# Observations: Due to stock market break down in the year 2009 , there is a dip in our data and the remainders aslo show negative values those times
# is our data seasonal?

x <- stl(SNP15,s.window= 'periodic')
x
stl <- autoplot(x, main = "Decomposition Plot of S&P 500 (2006-2015)",ts.colour = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),axis.line.y   = element_line(colour="gray"),axis.line.x = element_line(colour="gray"))
ggplotly(stl)

#seasonal trend of our data year wise
sp <- ggseasonplot(SNP15, xlab="Year", 
                   main="Seasonal Plot S&P 500 Index - Year Wise",
                   year.labels=TRUE, year.labels.left=TRUE, 
                   col=1:20, pch=19) +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

#In the year 2009 and 2013 , the trend is reversed
ggplotly(sp)

# check for sesonality using ets functions
s <- ets(SNP15)
seasonal1 <- !is.null(s$seasonal)
print(seasonal1)
#Data is not seasonal

library(fma)
fits <- ets(SNP15)
fitp <- ets(SNP15,model="ANN")

deviance <- 2*c(logLik(fits) - logLik(fitp))
df <- attributes(logLik(fits))$df - attributes(logLik(fitp))$df 
#P value
1-pchisq(deviance,df)

#************************************************ARIMA MODEL BUILDING*********************************************************

#------------------------------------- Plot ACF and PACF to determine p,d and q in our ARIMA MODEL----------------------------

# Plot ACF and PACF for SNP15
# Observation : ACF shows a slow decay and PACF cuts off after time lag 1
# ACF and PACF component on plotting shows that the data exhibits an AR pattern and MA cuts off after time lag 1

#Acf(SNP15,lag.max = 60)
a <- autoplot(Acf(SNP15, plot = FALSE , lag.max =60), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  ggtitle("ACF and PACF plots of S&P 500")

b <- autoplot(pacf(SNP15, plot = FALSE,lag.max =60), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + labs(y="PACF") 

#ACF part of our data shows the series is decaying and above the significane level 
#Thereby depicting a non-stationary time series and indicating us to use Auto Regression on the data.
#PACF shows a spike at 1 , indicating an AR(1) model 
#Thus suggesting we should try fitting an ARIMA model with p = 1

grid.arrange(a, b)


#------------------------------------------------Checking and making Data Stationary-----------------------------------------
#We observed from ACF and PACF that our data in not stationary
library(tseries) #package for using ADF Test and KPSS test for checking data stationarity
# ADF and KPSS test carried out again to determine our data is non-stationarity
# ADF test says differences is required if p-value is > 0.05
adf.test(SNP15)
# Kipps test says differences is required if p-value is < 0.05
kpss.test(SNP15)

#Function to determine the differencing order of the data
#for seasonal data
nsdiffs(SNP15)
# for non-seasonal part in data
ndiffs(SNP15)
#displays the ACF,PACF and time series plot
ggtsdisplay(SNP15)
# Observations : We determined our data doesnt require any seasonally differencing

# Differencing the data by order 1 obtained from above
SNP15diff <- diff(SNP15, differences=1)
# plotting the ACf ,PACF and time series plot of the differenced data 
ggtsdisplay(SNP15diff)
#Observation: 95% of ACF and PACF are within the significance levels 

#Checking if differenced data of order 1 is stationary by carrying out ADF KPSS Test
kpss.test(SNP15diff)
adf.test(SNP15diff)
# the differenced data of order 1 is determined  to be stationary

#We now know the d part of our ARIMA model. Now we will determine the p and q part
#We plot the ACF and PACF for our differenced data
c <- autoplot(Acf(diff(SNP15), plot = FALSE , lag.max = 60), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  ggtitle("ACF and PACF plots of Differenced S&P500")


d <- autoplot(pacf(diff(SNP15), plot = FALSE , lag.max = 60), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + labs(y="PACF") 

grid.arrange(c, d)

# from the above Acf and pacf we observe that there are lags at higher significane levels
# so possible models are ARMA(0,0), 
# Principle of Parsimony says choose one with the least number of parameters



# You can also be lazy and just call auto.arima and let it do the work for you.
auto.arima(SNP15)
xyz <-auto.arima(SNP15)

fit <- Arima(SNP15, order = c(0,1,0), include.drift = TRUE)
fit$fitted
fit$fitted

#forecasting the next 12 months values
SP500ARIMA <- forecast(fit, h = 12)
# plotting standardized residuals , ACF and p -value of SNP15
tsdiag(fit)

summary(SP500ARIMA)

# plot the residuals 
residFit <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white" , bins = 10) +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of S&P 500 ARIMA Model Residuals") 

ggplotly(residFit)
# standard residuals/noise shows normal distribution

# creating data frame for plotting SNP15 forecasted

x <-MergeDF(SNP15,SP500ARIMA)

library(ggplot2)

r <- ggplot(data = x, aes(x = month)) + 
  geom_line(aes(y = observed, colour = "Observed"), size = 1) + 
  geom_line(aes(y = fitted, colour = "Fitted"), size = 1) + 
  geom_line(aes(y = forecast, colour = "Forecast"), size = 1) +
  scale_colour_manual("",breaks = c("Fitted","Observed","Forecast"),values = c("Black","orange","turquoise")) + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = .25) +
  scale_x_yearmon(name = "Month / Year") +
  scale_y_continuous(breaks = seq(0,850,by=50)) +
  theme(axis.text.x = element_text(size = 10)) + 
  ggtitle("'Arima Fit to Simulated Data\n (Orange  =Forecast, Turquoise = Fitted, Black = Data, shadow=95% conf. interval)')") +
  ylab ( " Adjusted Price of S&P500")
ggplotly(r)

#*************************************************Holts Winter Model**********************************************************
# Holts Winter function has alpha , beta and gamma, 
# Alpha represents over all smoothing paramneter
# Beta represents trend smoothing parameter
# Gamma represents seasonal smoothing parameter
# the model by default is additive
# As there is no sesonality in our data we set GAMMMA =FALSE in our Holts Winter Model

HoltWintersFit <- HoltWinters(SNP15, gamma=FALSE)

HoltWintersFitForecast <- forecast.HoltWinters(HoltWintersFit, h=12)

#plot(HoltWintersFit)

h <-MergeDF(SNP15,HoltWintersFitForecast)

library(ggplot2)

t <- ggplot(data = h, aes(x = month)) + 
  geom_line(aes(y = observed, colour = "Observed"), size = 1) + 
  geom_line(aes(y = fitted, colour = "Fitted"), size = 1) + 
  geom_line(aes(y = forecast, colour = "Forecast"), size = 1) +
  scale_colour_manual("",breaks = c("Fitted","Observed","Forecast"),values = c("black","orange","red")) + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = .25) +
  scale_x_yearmon(name = "Month / Year") +
  scale_y_continuous(breaks = seq(0,850,by=50)) +
  theme(axis.text.x = element_text(size = 10)) + 
  ggtitle("'Holts Winter Model to Simulated Data\n (Orange = forecast, Red =fitted, Black = data, shadow=95% conf. interval)')") +
  ylab ( " Adjusted Price of S&P500")
ggplotly(t)


#****************************************************EXPONENTIAL SMOOTHING***************************************************

# Exponential Time Series Forecasting makes use of weigted average past 
# More Recent observations hold higher weights
Expo <- ets(SNP15)

ExpoForecast <- forecast(Expo ,h =12)

e <- MergeDF(SNP15,ExpoForecast)

ets <- ggplot(data = e, aes(x = month)) + 
  geom_line(aes(y = observed, colour = "Observed"), size = 1) + 
  geom_line(aes(y = fitted, colour = "Fitted"), size = 1) + 
  geom_line(aes(y = forecast, colour = "Forecast"), size = 1) +
  scale_colour_manual("",breaks = c("Fitted","Observed","Forecast"),values = c("black","orange","blue")) + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = .25) +
  scale_x_yearmon(name = "Month / Year") +
  scale_y_continuous(breaks = seq(0,850,by=50)) +
  theme(axis.text.x = element_text(size = 10)) + 
  ggtitle("'Exponential Smoothing Forecasting\n (Orange = forecast, Blue = fitted, Black = data, shadow=95% conf. interval)')") +
  ylab ( " Adjusted Price of S&P500")
ggplotly(ets)


#****************************************************** NAIVE FORECASTING***************************************************
naiveForecast <- naive(SNP15,h = 12)



na <- MergeDF(SNP15,naiveForecast)

Naive <- ggplot(data = na, aes(x = month)) + 
  geom_line(aes(y = observed, colour = "Observed"), size = 1) + 
  geom_line(aes(y = fitted, colour = "Fitted"), size = 1) + 
  geom_line(aes(y = forecast, colour = "Forecast"), size = 1) +
  scale_colour_manual("",breaks = c("Fitted","Observed","Forecast"),values = c("black","orange","green")) + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = .25) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = .25) +
  scale_x_yearmon(name = "Month / Year") +
  scale_y_continuous(breaks = seq(0,850,by=50)) +
  theme(axis.text.x = element_text(size = 10)) + 
  ggtitle("'Naive Forecasting\n (Orange = forecast, Green = fitted, Black = data, shadow=95% conf. interval)')") +
  ylab ( "Adjusted Price of S&P500")
ggplotly(Naive)



#***************************************************TABLE OF RESULTS********************************************************




# for plotting the forecasted values
pr1 <- as.data.frame(SP500ARIMA)
pr2 <- as.data.frame(HoltWintersFitForecast)
pr4 <- as.data.frame(ExpoForecast)
pr5 <- as.data.frame(naiveForecast)

pr1[2:5] <- NULL
pr2[2:5] <- NULL
pr4[2:5] <- NULL
pr5[2:5] <- NULL

res1 <- cbind.data.frame(pr1,pr2,pr4,pr5)

rt <- as.data.frame(window(SNP16))
names(rt) <- 'Observed'
library(zoo)
rt$Month <- as.yearmon(time(window(SNP16)))
rt <- data.frame(rt[121:132,])
rownames(rt) <- rt$Month
results<-cbind(rt,res1)
results$Month <- NULL
colnames(results)<-c('Observed Value','ARIMA','Holts Winter', 'Exponential Smoothing','Naive')
results


#*******************************************************FORECAST ERROR********************************************************

library(forecast)
v <- as.data.frame(accuracy(results$ARIMA,results$`Observed Value`))
y <- as.data.frame(accuracy(results$`Holts Winter`,results$`Observed Value`))
et <- as.data.frame(accuracy(results$`Exponential Smoothing`,results$`Observed Value`))
nai <- as.data.frame(accuracy(results$Naive,results$`Observed Value`))

Method <- c('Arima','Holts Winter',"Exponential Smoothing","Naive")
accu <- rbind.fill(v,y,et,nai)
dispaccu <- cbind(Method ,accu)
# plots accuracy measure for each forecasting technique
dispaccu


#***************************************************MODEL ERROR********************************************************
ACCUA <- as.data.frame(accuracy(fit$fitted,SNP15))

fit$fitted


Holt <- as.ts(HoltWintersFit)
ACCUH <- as.data.frame(accuracy(Holt$fitted,SNP15))

Expo <- as.ts(Expo)
ACCUE <- as.data.frame(accuracy(Expo))

ACCUN <- as.data.frame(accuracy(naiveForecast))

Method1 <- c('Arima','Holts Winter',"Exponential Smoothing",'Naive')
accu1 <- rbind.fill(ACCUA,ACCUH,ACCUE,ACCUN)
dispaccu1 <- cbind(Method1 ,accu1)
dispaccu1 <- dispaccu1[,-(8:9)]
# plots accuracy measure for each forecasting technique
dispaccu1
















