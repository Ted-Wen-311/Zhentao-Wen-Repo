#Pakckege download
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR', 'xts', 'dygraphs', 'assertthat')
my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}
purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)
sample_num = 5
library(dplyr)
library(caret)
library(glmnet)
library(naivebayes)
library(ggplot2)
library(MASS)
library(mltools)
library(data.table)
library(class)
library(readxl)

#Loading Data
s_data<-read_excel("/users/tedwen/desktop/book2.xlsx")

#Data Overview
summary(s_data)
str(s_data)
install.packages("Hmisc")
library(Hmisc)
describe(s_data)

#Data Cleaning

summary(s_data)
summary(s_datatimeseiries)

p1 = ggplot(s_data, aes(NBP)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p2 = ggplot(s_data, aes(GDP)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p3 = ggplot(s_data, aes(CEEP)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p4 = ggplot(s_data, aes(CEEA)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

sdata<-window(s_data, start = 1977)

s_datatimeseiries<-ts(s_data, start = 1977, frequency = 1)
s_datatimeseiries

plot.ts(s_datatimeseiries)

autoplot(s_datatimeseiries[,"NBP"]) +
  ggtitle("Newborn Population") +
  xlab("Year") +
  ylab("Thousands of People")

autoplot(s_datatimeseiries[,"GDP"]) +
  ggtitle("GDP in College Entrance Examination Year") +
  xlab("Year") +
  ylab("CNY")

autoplot(s_datatimeseiries[,"CEEP"]) +
  ggtitle("Amount of Participants in the College Entrance Examination") +
  xlab("Year") +
  ylab("Ten Thousands of People")

autoplot(s_datatimeseiries[,"CEEA"]) +
  ggtitle("Amount of Admissions of the College Entrance Examination") +
  xlab("Year") +
  ylab("Ten Thousands of People")

autoplot(s_datatimeseiries[,"CEEAR"]) +
  ggtitle("Amount of Admissions of the College Entrance Examination") +
  xlab("Year") +
  ylab("Ten Thousands of People")

autoplot(s_datatimeseiries[,"GER"]) +
  ggtitle("Gross Enrollment Rate") +
  xlab("Year") +
  ylab("Rate")

library(reshape2)
s_data<-melt(data, id.vars="year")



ggseasonplot(s_datatimeseries, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")
cor(s_datatimeseiries)

fit.consMR3<-tslm(GDP~NBP+CEEP+CEEA+GER+CEEAR, data=s_datatimeseiries)
summary(fit.consMR3)

fit.consMR<-tslm(CEEAR~NBP+GDP+CEEP+CEEA+GER,
                 data=s_datatimeseiries)
summary(fit.consMR)

checkresiduals(fit.consMR)
checkresiduals(fit.consMR2)
checkresiduals(fit.consMR3)

summary(fit.exp)
checkresiduals(fit.arima)
checkresiduals(fit.exp)
fit.consMR2<-tslm(CEEAR~NBP+GDP+CEEP+CEEA+GER+trend, data=s_datatimeseiries)
summary(fit.consMR2)

fit.linear<-tslm(CEEAR~GDP, data=s_datatimeseiries)

summary(fit.linear)
checkresiduals(fit.linear)


autoplot(s_datatimeseiries, series="Data") +
  autolayer(fitted(fit.consMR2), series="Fitted") +
  xlab("Year") + ylab("Value") +
  ggtitle("Time Seiries of Chinese College Entrance Examination Admission")

CV(fit.consMR2)

s_datatimeseiries <- window(s_datatimeseiries, start=1977)
fit.consMR2 <- tslm(NBP~trend, data=s_datatimeseiries)
fcast <- forecast(fit.consMR2)
autoplot(fcast) +
  ggtitle("Forecasts of New Born population") +
  xlab("Year") + ylab("Ten thousand people")
h<-10
fit.exp<-tslm(CEEAR~trend, data=s_datatimeseiries, lamdda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

fc<-ses(s_data$CEEAR, h = 5)
round(accuracy(fc),2)

fc2<-ses(s_data$CEEP, h = 5)
round(accuracy(fc2),2)

fc3<-ses(s_data$CEEA, h = 5)
round(accuracy(fc3),2)

fc4<-ses(s_data$NBP, h = 5)
round(accuracy(fc4),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("CEEAR(Acceptence Rate)") + xlab("Year")

autoplot(fc2) +
  autolayer(fitted(fc2), series="Fitted") +
  ylab("CEEP(Ten Thousand People)") + xlab("Year")

autoplot(fc3) +
  autolayer(fitted(fc3), series="Fitted") +
  ylab("CEEA(Ten Thousand People)") + xlab("Year")

autoplot(fc4) +
  autolayer(fitted(fc4), series="Fitted") +
  ylab("NBP(Ten Thousand People)") + xlab("Year")

fc5<-holt(s_datatimeseiries, h=15)
fc6<-holt(s_datatimeseiries, damped = TRUE, phi = 0.9, h=15)

autoplot(s_datatimeseiries) +
  autolayer(fc5, series="Holt's method", PI=FALSE) +
  autolayer(fc6, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("College entrance examination admission rate (rate)") +
  guides(colour=guide_legend(title="Forecast"))

fit.arima <- auto.arima(s_datatimeseiries[,"CEEAR"], seasonal=FALSE)
fit.arima2 <- auto.arima(s_datatimeseiries[,"GDP"], seasonal=FALSE)
fit.arima
fit.arima2
fit.arima %>% forecast(h=10) %>% autoplot(include=80)
fit.arima2 %>% forecast(h=10) %>% autoplot(include=80)
ggAcf(s_datatimeseiries[,"CEEAR"])
ggPacf(s_datatimeseiries[,"CEEAR"])
checkresiduals(fit.arima2)

fcast <- forecast(fit.linear)
summary(fit.arima2)
