# Code for a hibrid model for Time Series Forecast combining Wavelets Theory and ARIMA model
# Autor: Matheus Carneiro Nogueira


#packages needed
library(forecast)
library(tseries)
library(WaveletArima)
library(Metrics)
library(ggplot2)
#library(wavelets) 

#Set correct directory -> must be careful
setwd("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets")

#Importing the data 
setwd("Dados")
arquivo_IAG=read.csv(file="IAG_L2010a2019.csv")
arquivo_Japan=read.csv(file="9201_T2010a2019.csv")
arquivo_ChinaSouth=read.csv(file="1055_HK2010a2019.csv")
arquivo_ChinaEast=read.csv(file="0670_HK2010a2019.csv")
arquivo_Latam=read.csv(file="LTMAQ2010a2019.csv")
arquivo_Delta=read.csv(file="DAL2010a2019.csv")
arquivo_United=read.csv(file="UAL2010a2019.csv")
arquivo_American=read.csv(file="AAL2010a2019.csv")
arquivo_Luft=read.csv(file="LHA_DE2010a2019.csv")
arquivo_AirFranceKLM=read.csv(file="AF_PA2010a2019.csv")

#Selecting just AdjClose -> we will ignore the other entries
AdjClose_IAG=arquivo_IAG[c(6)]
AdjClose_Japan=arquivo_Japan[c(6)]
AdjClose_IAG=arquivo_IAG[c(6)]
AdjClose_Japan=arquivo_Japan[c(6)]
AdjClose_ChinaEast=arquivo_ChinaEast[c(6)]
AdjClose_ChinaSouth=arquivo_ChinaSouth[c(6)]
AdjClose_Delta=arquivo_Delta[c(6)]
AdjClose_United=arquivo_United[c(6)]
AdjClose_American=arquivo_American[c(6)]
AdjClose_Luft=arquivo_Luft[c(6)]
AdjClose_Latam=arquivo_Latam[c(6)]
AdjClose_AirFranceKLM=arquivo_AirFranceKLM[c(6)]


#Putting all of them in a vector in order to make th code cleaner
AdjClose = c(AdjClose_IAG,AdjClose_Japan,AdjClose_ChinaSouth,AdjClose_ChinaEast,AdjClose_Latam,AdjClose_Delta,AdjClose_United,AdjClose_American,AdjClose_Luft,AdjClose_AirFranceKLM)
len=length(AdjClose)
names=c("IAG","Japan","ChinaSouth","ChinaEast","Latam","Delta","United","American","Lufthansa","AirFranceKLM")

#Convert to numeric in order to clean the NA and NANs

for (i in 1:len) {
  AdjClose[[i]]=as.numeric(t(AdjClose[[i]]))
  AdjClose[[i]]=AdjClose[[i]][!is.na(AdjClose[[i]])]
  AdjClose[[i]]=AdjClose[[i]][!is.nan(AdjClose[[i]])]
}


#Visualizing the original data
for(i in 1:len){
  path = paste("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/AdjClose_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  plot(AdjClose[[i]],main=paste("AdjClose",names[i]),type="l")
  dev.off()
}

#Converting to TimeSeries class
series=c()
for (i in 1:len) {
  series[[i]]=ts(AdjClose[[i]])
  series[[i]]=tsclean(series[[i]])
}


#Checking if it is stationary -> seems not
tests=c()
for(i in 1:len){
  print(names[i])
  tests[[i]]=adf.test(series[[i]], alternative = "stationary")
  print(tests[[i]])
}


#We are interested in the log-return, not the price itself. Let's calculate it
#We have rt = ln(Pt)-ln(Pt-1)
logret=c()
for(i in 1:len){
    logret[[i]]=diff(log(series[[i]]),lag=1)
    logret[[i]]=logret[[i]][!is.na(logret[[i]])]
    logret[[i]]=logret[[i]][!is.nan(logret[[i]])]
    logret[[i]]=ts(logret[[i]])
    logret[[i]]=tsclean(logret[[i]])
}


#Let's check stationarity againg, since we have made a differentiation
tests_logret=c()
for(i in 1:len){
  print(names[i])
  tests_logret[[i]]=adf.test(logret[[i]], alternative = "stationary")
  print(tests_logret[[i]])
}
#p-value smaller than 0.01 for all of them-> seems stationary

#Visualizing the series after the log-return
for(i in 1:len){
  path = paste("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/Logret_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  plot(logret[[i]],main=paste("LogReturn",names[i]),type="l")
  dev.off()
}

#Train and test -> we will make 90%/10%
serie_2fit=c()
serie_2for=c()
wave=c()
forecast_values=c()
for(i in 1:len){
  train_limit=as.integer(0.9*length(logret[[i]]))
  
  serie_2fit[[i]]=ts(logret[[i]][c(1:train_limit)])
  serie_2for[[i]]=ts(logret[[i]][c((train_limit+1):length(logret[[i]]))])
  
  #Fitting and Forecasting with Wavelets + ARIMA
  levels = floor(log(length(serie_2fit[[i]])))
  wave[[i]] = WaveletFittingarma(logret[[i]],Waveletlevels=levels,boundary='periodic',FastFlag=TRUE,NForecast = length(serie_2for[[i]]),MaxARParam = 15,MaxMAParam = 15)
  
  forecast_values[[i]] = wave[[i]]$Finalforecast
  
  #Calculating the Root Mean Square Error
  error = rmse(serie_2for[[i]],forecast_values[[i]])
  print(paste("RMSE Series",names[i],"=",error))
}

#Plotting the results to compare

#All of them
for (i in 1:len) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]]),forecasted = forecast_values[[i]])
  
  plot=ggplot(seriesDF,aes(x=c(1:length(serie_2for[[i]])))) + 
    ggtitle(paste("Forecast Comparison (All)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/ForecastAll_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
  
  
  error = rmse(serie_2for[[i]],forecast_values[[i]])
  print(paste("RMSE Series",names[i],"=",error))
}


#Only the 50 first
for (i in 1:len) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]][c(1:50)]),forecasted = forecast_values[[i]][c(1:50)])
  
  plot=ggplot(seriesDF,aes(x=c(1:50)))+
    ggtitle(paste("Forecast Comparison (50 first)",names[i])) + 
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))

  path = paste("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/Forecast50_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
  
  serie_2for_50=serie_2for[[i]]
  serie_2for_50=serie_2for_50[c(1:50)]
  forecast_values_50=forecast_values[[i]]
  forecast_values_50=forecast_values_50[c(1:50)]
  error = rmse(serie_2for_50,forecast_values_50)
  print(paste("RMSE Series (50 first)",names[i],"=",error))
  
  
}


#New split -> leaving just 20 entries to forecast and compare


#Train and test -> we will leave just 20 entries to forecast
serie_2fit=c()
serie_2for=c()
wave=c()
forecast_values=c()
for(i in 1:len){
  train_limit=length(logret[[i]])-20
  
  serie_2fit[[i]]=ts(logret[[i]][c(1:train_limit)])
  serie_2for[[i]]=ts(logret[[i]][c((train_limit+1):length(logret[[i]]))])
  
  #Fitting and Forecasting with Wavelets + ARIMA
  levels = floor(log(length(serie_2fit[[i]])))
  wave[[i]] = WaveletFittingarma(logret[[i]],Waveletlevels=levels,boundary='periodic',FastFlag=TRUE,NForecast = length(serie_2for[[i]]),MaxARParam = 15,MaxMAParam = 15)
  
  forecast_values[[i]] = wave[[i]]$Finalforecast
  
  #Calculating the Root Mean Square Error
  error = rmse(serie_2for[[i]],forecast_values[[i]])
  print(paste("RMSE Series",names[i],"=",error))
}

#Plotting the results to compare

for (i in 1:len) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]]),forecasted = forecast_values[[i]])
  
  plot=ggplot(seriesDF,aes(x=c(1:length(serie_2for[[i]])))) + 
    ggtitle(paste("Forecast Comparison (20)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/햞ea de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/Forecast20_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
}
