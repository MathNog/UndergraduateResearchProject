library(forecast)
library(tseries)
library(FitARMA)
library(vars)
library(ggplot2)
library(Metrics)


#Set correct directory -> must be careful
setwd("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR")

#Importing the data 

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
  path = paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/AdjClose_",names[i],".jpeg",sep = "")
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


'Selecionar o tamanho da menor serie para definir tamanho da variavel VAR'
vec_tam=c()
for(i in 1:len){
  vec_tam[i]=length(logret[[i]])
}

len=10000
for (tam in vec_tam) {
  if(tam<len){
    len=tam
  }
}

len2=0
for (tam in vec_tam) {
  if(tam>len2){
    len2=tam
  }
}

'Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
max_len=1700 #mudar para o tamanho correto das series -> preciso que todas tenham o mesmo tamanho -> limitar pela menor'


#Train and test -> we will make 90%/10%
serie_2fit=c()
serie_2for=c()
fit_len=as.integer(len*0.9)
for_len=as.integer(len*0.1)

for(i in 1:10){
  serie_2fit[[i]]=ts(logret[[i]][c(1:fit_len)])
  serie_2for[[i]]=ts(logret[[i]][c((fit_len+1):(fit_len+178))])
}


'Visualizacao da Serie - Inutil'
VAR_data=cbind(serie_2fit[[1]],serie_2fit[[2]],serie_2fit[[3]],serie_2fit[[4]],serie_2fit[[5]],serie_2fit[[6]],serie_2fit[[7]],serie_2fit[[8]],serie_2fit[[9]],serie_2fit[[10]])
colnames(VAR_data) <- names
print(tail(VAR_data))


'Seleção de Lag de acordo com os critérios AIC e afins'
lagselect = VARselect(VAR_data, lag.max = 15, type = "const")
print(lagselect)

'Estimando modelo VAR com p definido acima'
VAR_est = VAR(VAR_data, p = 9,season=NULL)

'Plotando resultado da estimativa'
#summary(VAR_est)

'Escrevendo resultado da estimativa em um arquivo txt'
sink("Imagens/VAR_est/summary_VAR_est_all.txt")
print(summary(VAR_est))
sink()


'Plotando modelo estimado'
plot(VAR_est) #it plots one for each click -> saved individually

'Fazendo a previsão'
VAR_for = predict(VAR_est, n.ahead = for_len, ci = 0.9)

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for_all.jpeg",width = 658, height = 553)
plot(VAR_for)
dev.off()

forecast_values=c()
forecast_values[[1]]=VAR_for$fcst$IAG[1:178]
forecast_values[[2]]=VAR_for$fcst$Japan[1:178]
forecast_values[[3]]=VAR_for$fcst$ChinaSouth[1:178]
forecast_values[[4]]=VAR_for$fcst$ChinaEast[1:178]
forecast_values[[5]]=VAR_for$fcst$Delta[1:178]
forecast_values[[6]]=VAR_for$fcst$United[1:178]
forecast_values[[7]]=VAR_for$fcst$American[1:178]
forecast_values[[8]]=VAR_for$fcst$Lufthansa[1:178]
forecast_values[[9]]=VAR_for$fcst$AirFranceKLM[1:178]
forecast_values[[10]]=VAR_for$fcst$Latam[1:178]


#Plotting the forecast comparrison

#For all entries
for (i in 1:10) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]]),forecasted = forecast_values[[i]])
  
  plot=ggplot(seriesDF,aes(x=c(1:length(serie_2for[[i]])))) + 
    ggtitle(paste("VAR Forecast Comparison (All)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for/ForecastAll_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
  
}

#Calculating the Root Mean Square Error
for(i in i:10){
  error = rmse(serie_2for[[i]],forecast_values[[i]])
  print(paste("RMSE Series (all)",names[i],"=",error))
}

#For the 50 first entries only
for (i in 1:10) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]][c(1:50)]),forecasted = forecast_values[[i]][c(1:50)])
  
  plot=ggplot(seriesDF,aes(x=c(1:50))) + 
    ggtitle(paste("VAR Forecast Comparison (50 first)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for/50first/Forecast50first_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
  
  #Calculating the Root Mean Square Error
  error = rmse(serie_2for[[i]][c(1:50)],forecast_values[[i]][c(1:50)])
  print(paste("RMSE Series (50 first)",names[i],"=",error))
}


#New split, leaving just the last 20 entries out of the training set
#Train and test -> we will make 90%/10%
serie_2fit=c()
serie_2for=c()
fit_len=as.integer(len-20)
for_len=20

for(i in 1:10){
  serie_2fit[[i]]=ts(logret[[i]][c(1:fit_len)])
  serie_2for[[i]]=ts(logret[[i]][c((fit_len+1):(fit_len+20))])
}


'Visualizacao da Serie - Inutil'
VAR_data=cbind(serie_2fit[[1]],serie_2fit[[2]],serie_2fit[[3]],serie_2fit[[4]],serie_2fit[[5]],serie_2fit[[6]],serie_2fit[[7]],serie_2fit[[8]],serie_2fit[[9]],serie_2fit[[10]])
colnames(VAR_data) <- names
print(tail(VAR_data))


'Seleção de Lag de acordo com os critérios AIC e afins'
lagselect = VARselect(VAR_data, lag.max = 15, type = "const")
print(lagselect)

'Estimando modelo VAR com p definido acima'
VAR_est = VAR(VAR_data, p = 9,season=NULL)

'Plotando resultado da estimativa'
#summary(VAR_est)

'Escrevendo resultado da estimativa em um arquivo txt'
sink("Imagens/VAR_est/summary_VAR_est_20last.txt")
print(summary(VAR_est))
sink()


'Plotando modelo estimado'
plot(VAR_est) #it plots one for each click -> saved individually

'Fazendo a previsão'
VAR_for = predict(VAR_est, n.ahead = for_len, ci = 0.9)

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for_20last.jpeg",width = 658, height = 553)
plot(VAR_for)
dev.off()

forecast_values=c()
forecast_values[[1]]=VAR_for$fcst$IAG[1:20]
forecast_values[[2]]=VAR_for$fcst$Japan[1:20]
forecast_values[[3]]=VAR_for$fcst$ChinaSouth[1:20]
forecast_values[[4]]=VAR_for$fcst$ChinaEast[1:20]
forecast_values[[5]]=VAR_for$fcst$Delta[1:20]
forecast_values[[6]]=VAR_for$fcst$United[1:20]
forecast_values[[7]]=VAR_for$fcst$American[1:20]
forecast_values[[8]]=VAR_for$fcst$Lufthansa[1:20]
forecast_values[[9]]=VAR_for$fcst$AirFranceKLM[1:20]
forecast_values[[10]]=VAR_for$fcst$Latam[1:20]


#Plotting the forecast comparrison

#For all entries
for (i in 1:10) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]]),forecasted = forecast_values[[i]])
  
  plot=ggplot(seriesDF,aes(x=c(1:20))) + 
    ggtitle(paste("VAR Forecast Comparison (20 last)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for/20last/Forecast_20last_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
  
  #Calculating the Root Mean Square Error
  error = rmse(serie_2for[[i]],forecast_values[[i]])
  print(paste("RMSE Series (20 last)",names[i],"=",error))
  
}


'Até aqui apenas utilizamos funcionalidades de estimativa e previsão a fim de comparar a qualidade das previsoes realizadas com o 
ferramental VAR com o anteriormente estudado ARIMA
Agora vamos introduzir 3 análises novas - Granger Causality Test, Impulse Response e Forecast Error Variance Decomposition'


'Granger Causality'
for (i in 1:10) {
  path=paste("Imagens/granger_causality/granger_causality_",names[i],".txt",sep = "")
  sink(path)
  print(causality(VAR_est,cause=names[i]))
  sink()
}



'Impulse Response'

for(i in 1:10){
  impRes = irf(VAR_est,impulse = names[i],n.ahead = 50, boot = TRUE)
  path=paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_",names[i],".jpeg",sep="")
  jpeg(file=path,width = 658, height = 553)
  plot(impRes)
  dev.off()
}

impRes_Japan = irf(VAR_est,impulse = "Japan",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_Japan.jpeg",width = 658, height = 553)
plot(impRes_Japan)
dev.off()

impRes_IAG = irf(VAR_est,impulse = "IAG",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_IAG.jpeg",width = 658, height = 553)
plot(impRes_IAG)
dev.off()

impRes_ChinaSouth = irf(VAR_est,impulse = "ChinaSouth",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_ChinaSouth.jpeg",width = 658, height = 553)
plot(impRes_ChinaSouth)
dev.off()

impRes_ChinaEast = irf(VAR_est,impulse = "ChinaEast",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_ChinaEast.jpeg",width = 658, height = 553)
plot(impRes_ChinaEast)
dev.off()

impRes_Delta = irf(VAR_est,impulse = "Delta",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_Delta.jpeg",width = 658, height = 553)
plot(impRes_Delta)
dev.off()

impRes_United = irf(VAR_est,impulse = "United",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_United.jpeg",width = 658, height = 553)
plot(impRes_United)
dev.off()

impRes_American = irf(VAR_est,impulse = "American",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_American.jpeg",width = 658, height = 553)
plot(impRes_American)
dev.off()

impRes_Lufthansa = irf(VAR_est,impulse = "Lufthansa",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_Lufthansa.jpeg",width = 658, height = 553)
plot(impRes_Lufthansa)
dev.off()

impRes_AirFranceKLM = irf(VAR_est,impulse = "AirFranceKLM",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_AirFranceKLM.jpeg",width = 658, height = 553)
plot(impRes_AirFranceKLM)
dev.off()

impRes_Latam = irf(VAR_est,impulse = "Latam",n.ahead = 50, boot = TRUE)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Impulse_Response/IRF_Latam.jpeg",width = 658, height = 553)
plot(impRes_Latam)
dev.off()


'Forecast Error Variance Decomposition'

VAR_fevd = fevd(VAR_est, n.ahead = 50)
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/FEVD.jpeg",width = 658, height = 553)
plot(VAR_fevd)
dev.off()


sink("Imagens/FEVD.txt")
print(VAR_fevd)
sink()

