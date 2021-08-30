# Code for a hibrid model for Time Series Forecast combining Wavelets Theory and ARIMA model
# Autor: Matheus Carneiro Nogueira


#packages needed
library(forecast)
library(tseries)
library(FitARMA)
library(wavelets)

#Set correct directory -> must be careful
setwd("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets")

#Importing the data 
setwd("Dados")
arquivo=read.csv(file="IAG_L2010a2019.csv")

#Selecting just AdjClose -> we will ignore the other entries
AdjClose=arquivo[c(6)]

#Convert to numeric in order to clean the NA and NANs
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Visualizing the original data
jpeg(file="C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/IAG/AdjClose_IAG.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose IAG 2010-2019",type="l")
dev.off()

#Converting to TimeSeries class
serie_IAG=ts(AdjClose_vec)
serie_IAG=tsclean(serie_IAG)

#Checking if it is stationary -> seems not
adf.test(serie_IAG, alternative = "stationary")
#p-value = 0.31 -> not stationary

#We are interested in the log-return, not the price itself. Let's calculate it
#We have rt = ln(Pt)-ln(Pt-1)
logret_IAG=diff(log(serie_IAG),lag=1)
logret_IAG=logret_IAG[!is.na(logret_IAG)]
logret_IAG=logret_IAG[!is.nan(logret_IAG)]


#Let's check stationarity againg, since we have made a differentiation
adf.test(logret_IAG,alternative="stationary")
#p-value smaller than 0.01 -> seems stationary

#Visualizing the series after the log-return
jpeg(file="C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/IAG/Logret_IAG.jpeg",width = 658, height = 553)
plot(logret_IAG,main="Log Return IAG 2010-2019",type="l")
dev.off()

#Converting to TimeSeries class
logret_IAG=ts(logret_IAG)
logret_IAG=tsclean(logret_IAG)


#Discrete Wavelet Transform
#Filter chosen ->
#N.limits chosen -> 
nlevels=3
dwt_IAG=dwt(logret_IAG,n.levels = nlevels,filter="haar")

#Plotting the DWT
jpeg(file="C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/IAG/DWT_IAG.jpeg")
plot.dwt(dwt_IAG,plot.V = TRUE,plot.W = TRUE)
dev.off()

#Getting the coefficients in order to use ARIMA
dwt_IAG_coef = c()
for (i in 1:nlevels) {
  dwt_IAG_coef[i]=dwt_IAG@W[i]
  dwt_IAG_coef[[i]]=as.numeric(dwt_IAG_coef[[i]])
}

#Separating the series
serie1=dwt_IAG_coef[[1]]
serie2=dwt_IAG_coef[[2]]
serie3=dwt_IAG_coef[[3]]

#Train and test -> we will make 90%/10%
limit1=as.integer(0.9*length(serie1))

serie_2fit=ts(serie1[c(1:limit1)])
serie_2for=ts(serie1[c((limit1+1):length(serie1))])


#PACF and ACF
jpeg(file="C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/IAG/ACF_IAG.jpeg",width = 658, height = 553)
Acf(serie_2fit,main='ACF IAG')#lags relevantes para MA(q) -> 18
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/IAG/PACF_IAG.jpeg",width = 658, height = 553)
Pacf(serie_2fit,main='PACF for IAG')#lags relevantes para AR(p) -> 18
dev.off()


#ARIMA coeficients for the training series
serie1_arima=arima(serie_2fit,order=c(8,0,0))

print(serie1_arima)
checkresiduals(serie1_arima)


for100=forecast(serie1_arima,h=115,level=c(80,95))
