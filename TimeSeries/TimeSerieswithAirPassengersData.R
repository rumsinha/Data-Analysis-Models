#Time Series Analysis and Modeling with the Air Passengers Dataset

#### LOAD PACKAGES

library(ggfortify)

library(tseries)
library(forecast)


#### LOAD DATA

data(AirPassengers)
AP <- AirPassengers
# Take a look at the class of the dataset AirPassengers
class(AP)

# The AirPassenger dataset in R provides monthly totals of a US airline passengers, from 1949 to 1960. 
# This dataset is already of a time series class therefore no further class or date manipulation is required.

#### PERFORM EXPLORATORY DATA ANALYSIS
# To perform exploratory analysis, let's first review the data with summary statistics and plots in R.

# Take a look at the entries
AP

# Check for missing values
sum(is.na(AP))

# Check the frequency of the time series
frequency(AP)

# Check the cycle of the time series
cycle(AP)

# Review the table summary
summary(AP)

# Plot the raw data using the base plot function
plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")

autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

# Let's use the boxplot function to see any seasonal effects.

boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

#From these exploratory plots, we can make some initial inferences:

# The passenger numbers increase over time with each year which may be indicative of an increasing linear trend, perhaps due to increasing demand for flight travel and commercialisation of airlines in that time period.
# In the boxplot there are more passengers travelling in months 6 to 9 with higher means and higher variances than the other months, indicating seasonality with a apparent cycle of 12 months. The rationale for this could be more people taking holidays and fly over the summer months in the US.
# AirPassengers appears to be multiplicative time series as the passenger numbers increase, it appears so does the pattern of seasonality.
# There do not appear to be any outliers and there are no missing values. Therefore no data cleaning is required.

# TIME SERIES DECOMPOSITION

#   We will decompose the time series for estimates of trend, seasonal, and random components using moving average method.
# 
# The multiplicative model is:
#   
#   Y[t]=T[t]???S[t]???e[t]
# 
# where
# 
# Y(t) is the number of passengers at time t,
# T(t) is the trend component at time t,
# S(t) is the seasonal component at time t,
# e(t) is the random error component at time t.


decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

# TEST STATIONARITY OF THE TIME SERIES

#nm   A stationary time series has the conditions that the mean, variance and covariance are not functions of time. In order to fit arima models, the time series is required to be stationary.


adf.test(AP) 

#As a rule of thumb, where the p-value is less than 5%, we strong evidence against the null hypothesis, so we reject the null hypothesis. As per the test results above, the p-value is 0.01 which is <0.05 therefore we reject the null in favour of the alternative hypothesis that the time series is stationary.


# Test stationarity of the time series (Autocorrelation)
# 
# Another way to test for stationarity is to use autocorrelation. We will use autocorrelation function (acf) in from 
# the base stats R package. This function plots the correlation between a series and its lags ie previous observations 
# with a 95% confidence interval in blue. If the autocorrelation crosses the dashed blue line, it means that specific 
# lag is significantly correlated with current series.

autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 
# Review random time series for any missing values
decomposeAP$random

# Autoplot the random time series from 7:138 which exclude the NA values
autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961") 


# FIT A TIME SERIES MODEL
# 1. Linear Model
# 
# Since there is an upwards trend we will look at a linear model first for comparison. We plot AirPassengers raw dataset with a blue linear model.

autoplot(AP) + geom_smooth(method="lm")+ labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

#This may not be the best model to fit as it doesn't capture the seasonality and multiplicative effects over time.

# ARIMA Model
# 
# Use the auto.arima function from the forecast R package to fit the best model and coefficients, given the default parameters including seasonality as TRUE.

arimaAP <- auto.arima(AP)
arimaAP

# Series: AP 
# ARIMA(2,1,1)(0,1,0)[12] 
# 
# Coefficients:
#   ar1     ar2      ma1
# 0.5960  0.2143  -0.9819
# s.e.  0.0888  0.0880   0.0292
# 
# sigma^2 estimated as 132.3:  log likelihood=-504.92
# AIC=1017.85   AICc=1018.17   BIC=1029.35

# The ARIMA(2,1,1)(0,1,0)[12] model parameters are lag 1 differencing (d), an autoregressive term of second lag (p) and a moving average model of order 1 (q). Then the seasonal model has an autoregressive term of first lag (D) at model period 12 units, in this case months.
# 
# The ARIMA fitted model is:
#   
#   Y^=0.5960Yt???2+0.2143Yt???12???0.9819et???1+E
# 
# where E is some error.

#The ggtsdiag function from ggfortify R package performs model diagnostics of the residuals and the acf. will include a autocovariance plot.

ggtsdiag(arimaAP)

#The residual plots appear to be centered around 0 as noise, with no pattern. the arima model is a fairly good fit.
# 
# CALCULATE FORECASTS
# Finally we can plot a forecast of the time series using the forecast function, again from the forecast R package, with a 95% confidence interval where h is the forecast horizon periods in months.

forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)

