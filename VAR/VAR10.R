library(forecast)
library(tseries)
library(FitARMA)
library(vars)
library(ggplot2)
library(Metrics)


'Setar diretório correto'
setwd("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR")
#setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/VAR")

#Leitura dos dados 
arq_IAG=read.csv(file="IAG_L2010a2019.csv")
arq_Japan=read.csv(file="9201_T2010a2019.csv")
arq_ChinaEast=read.csv(file="0670_HK2010a2019.csv")
arq_ChinaSouth=read.csv(file="1055_HK2010a2019.csv")
arq_Delta=read.csv(file="DAL2010a2019.csv")
arq_United=read.csv(file="UAL2010a2019.csv")
arq_American=read.csv(file="AAL2010a2019.csv")
arq_Luft=read.csv(file="LHA_DE2010a2019.csv")
arq_Latam=read.csv(file="LTMAQ2010a2019.csv")
arq_AirFrance=read.csv(file="AF_PA2010a2019.csv")


'Selecionando apenas o AdjClose -> podemos ignorar os outros'
AdjClose_IAG=arq_IAG[c(6)]
AdjClose_Japan=arq_Japan[c(6)]
AdjClose_ChinaEast=arq_ChinaEast[c(6)]
AdjClose_ChinaSouth=arq_ChinaSouth[c(6)]
AdjClose_Delta=arq_Delta[c(6)]
AdjClose_United=arq_United[c(6)]
AdjClose_American=arq_American[c(6)]
AdjClose_Luft=arq_Luft[c(6)]
AdjClose_Latam=arq_Latam[c(6)]
AdjClose_AirFrance=arq_AirFrance[c(6)]


'Convertendo para numeric a fim de limpar NA e NAN'
AdjClose_vec_IAG=as.numeric(t(AdjClose_IAG))
AdjClose_vec_IAG=AdjClose_vec_IAG[!is.na(AdjClose_vec_IAG)]
AdjClose_vec_IAG=AdjClose_vec_IAG[!is.nan(AdjClose_vec_IAG)]

AdjClose_vec_Japan=as.numeric(t(AdjClose_Japan))
AdjClose_vec_Japan=AdjClose_vec_Japan[!is.na(AdjClose_vec_Japan)]
AdjClose_vec_Japan=AdjClose_vec_Japan[!is.nan(AdjClose_vec_Japan)]

AdjClose_vec_ChinaEast=as.numeric(t(AdjClose_ChinaEast))
AdjClose_vec_ChinaEast=AdjClose_vec_ChinaEast[!is.na(AdjClose_vec_ChinaEast)]
AdjClose_vec_ChinaEast=AdjClose_vec_ChinaEast[!is.nan(AdjClose_vec_ChinaEast)]

AdjClose_vec_ChinaSouth=as.numeric(t(AdjClose_ChinaSouth))
AdjClose_vec_ChinaSouth=AdjClose_vec_ChinaSouth[!is.na(AdjClose_vec_ChinaSouth)]
AdjClose_vec_ChinaSouth=AdjClose_vec_ChinaSouth[!is.nan(AdjClose_vec_ChinaSouth)]

AdjClose_vec_Delta=as.numeric(t(AdjClose_Delta))
AdjClose_vec_Delta=AdjClose_vec_Delta[!is.na(AdjClose_vec_Delta)]
AdjClose_vec_Delta=AdjClose_vec_Delta[!is.nan(AdjClose_vec_Delta)]

AdjClose_vec_United=as.numeric(t(AdjClose_United))
AdjClose_vec_United=AdjClose_vec_United[!is.na(AdjClose_vec_United)]
AdjClose_vec_United=AdjClose_vec_United[!is.nan(AdjClose_vec_United)]

AdjClose_vec_American=as.numeric(t(AdjClose_American))
AdjClose_vec_American=AdjClose_vec_American[!is.na(AdjClose_vec_American)]
AdjClose_vec_American=AdjClose_vec_American[!is.nan(AdjClose_vec_American)]

AdjClose_vec_Luft=as.numeric(t(AdjClose_Luft))
AdjClose_vec_Luft=AdjClose_vec_Luft[!is.na(AdjClose_vec_Luft)]
AdjClose_vec_Luft=AdjClose_vec_Luft[!is.nan(AdjClose_vec_Luft)]

AdjClose_vec_Latam=as.numeric(t(AdjClose_Latam))
AdjClose_vec_Latam=AdjClose_vec_Latam[!is.na(AdjClose_vec_Latam)]
AdjClose_vec_Latam=AdjClose_vec_Latam[!is.nan(AdjClose_vec_Latam)]

AdjClose_vec_AirFrance=as.numeric(t(AdjClose_AirFrance))
AdjClose_vec_AirFrance=AdjClose_vec_AirFrance[!is.na(AdjClose_vec_AirFrance)]
AdjClose_vec_AirFrance=AdjClose_vec_AirFrance[!is.nan(AdjClose_vec_AirFrance)]


'Para obter o log-return a partir dos preços
Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]'
logret_IAG=diff(log(AdjClose_vec_IAG),lag=1)
logret_IAG=logret_IAG[!is.na(logret_IAG)]
logret_IAG=logret_IAG[!is.nan(logret_IAG)]

logret_Japan=diff(log(AdjClose_vec_Japan),lag=1)
logret_Japan=logret_Japan[!is.na(logret_Japan)]
logret_Japan=logret_Japan[!is.nan(logret_Japan)]

logret_ChinaEast=diff(log(AdjClose_vec_ChinaEast),lag=1)
logret_ChinaEast=logret_ChinaEast[!is.na(logret_ChinaEast)]
logret_ChinaEast=logret_ChinaEast[!is.nan(logret_ChinaEast)]

logret_ChinaSouth=diff(log(AdjClose_vec_ChinaSouth),lag=1)
logret_ChinaSouth=logret_ChinaSouth[!is.na(logret_ChinaSouth)]
logret_ChinaSouth=logret_ChinaSouth[!is.nan(logret_ChinaSouth)]

logret_Delta=diff(log(AdjClose_vec_Delta),lag=1)
logret_Delta=logret_Delta[!is.na(logret_Delta)]
logret_Delta=logret_Delta[!is.nan(logret_Delta)]

logret_United=diff(log(AdjClose_vec_United),lag=1)
logret_United=logret_United[!is.na(logret_United)]
logret_United=logret_United[!is.nan(logret_United)]

logret_American=diff(log(AdjClose_vec_American),lag=1)
logret_American=logret_American[!is.na(logret_American)]
logret_American=logret_American[!is.nan(logret_American)]

logret_Luft=diff(log(AdjClose_vec_Luft),lag=1)
logret_Luft=logret_Luft[!is.na(logret_Luft)]
logret_Luft=logret_Luft[!is.nan(logret_Luft)]

logret_Latam=diff(log(AdjClose_vec_Latam),lag=1)
logret_Latam=logret_Latam[!is.na(logret_Latam)]
logret_Latam=logret_Latam[!is.nan(logret_Latam)]

logret_AirFrance=diff(log(AdjClose_vec_AirFrance),lag=1)
logret_AirFrance=logret_AirFrance[!is.na(logret_AirFrance)]
logret_AirFrance=logret_AirFrance[!is.nan(logret_AirFrance)]


'Colocando logret em uma variável do tipo TimeSeries'
serie_IAG=ts(logret_IAG)
serie_IAG=tsclean(serie_IAG)

serie_Japan=ts(logret_Japan)
serie_Japan=tsclean(serie_Japan)

serie_ChinaEast=ts(logret_ChinaEast)
serie_ChinaEast=tsclean(serie_ChinaEast)

serie_ChinaSouth=ts(logret_ChinaSouth)
serie_ChinaSouth=tsclean(serie_ChinaSouth)

serie_Delta=ts(logret_Delta)
serie_Delta=tsclean(serie_Delta)

serie_United=ts(logret_United)
serie_United=tsclean(serie_United)

serie_American=ts(logret_American)
serie_American=tsclean(serie_American)

serie_Luft=ts(logret_Luft)
serie_Luft=tsclean(serie_Luft)

serie_Latam=ts(logret_Latam)
serie_Latam=tsclean(serie_Latam)

serie_AirFrance=ts(logret_AirFrance)
serie_AirFrance=tsclean(serie_AirFrance)


'Selecionar o tamanho da menor serie para definir tamanho da variavel VAR'
vec_tam=c(length(serie_IAG),length(serie_Japan),length(serie_ChinaEast),length(serie_ChinaSouth),length(serie_Delta),length(serie_United),length(serie_American),length(serie_Luft),length(serie_Latam),length(serie_AirFrance))

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
fit_len=as.integer(len*0.9) #90% da serie para ajuste
for_len=as.integer(len*0.1)


serie_2fit_IAG=serie_IAG[c(1:fit_len)]
serie_2for_IAG=serie_IAG[c(fit_len+1:for_len)]
serie_2fit_IAG=ts(serie_2fit_IAG)
serie_2for_IAG=ts(serie_2for_IAG)

serie_2fit_Japan=serie_Japan[c(1:fit_len)]
serie_2for_Japan=serie_Japan[c(fit_len+1:for_len)]
serie_2fit_Japan=ts(serie_2fit_Japan)
serie_2for_Japan=ts(serie_2for_Japan)

serie_2fit_ChinaEast=serie_ChinaEast[c(1:fit_len)]
serie_2for_ChinaEast=serie_ChinaEast[c(fit_len+1:for_len)]
serie_2fit_ChinaEast=ts(serie_2fit_ChinaEast)
serie_2for_ChinaEast=ts(serie_2for_ChinaEast)

serie_2fit_ChinaSouth=serie_ChinaSouth[c(1:fit_len)]
serie_2for_ChinaSouth=serie_ChinaSouth[c(fit_len+1:for_len)]
serie_2fit_ChinaSouth=ts(serie_2fit_ChinaSouth)
serie_2for_ChinaSouth=ts(serie_2for_ChinaSouth)

serie_2fit_Delta=serie_Delta[c(1:fit_len)]
serie_2for_Delta=serie_Delta[c(fit_len+1:for_len)]
serie_2fit_Delta=ts(serie_2fit_Delta)
serie_2for_Delta=ts(serie_2for_Delta)

serie_2fit_United=serie_United[c(1:fit_len)]
serie_2for_United=serie_United[c(fit_len+1:for_len)]
serie_2fit_United=ts(serie_2fit_United)
serie_2for_United=ts(serie_2for_United)

serie_2fit_American=serie_American[c(1:fit_len)]
serie_2for_American=serie_American[c(fit_len+1:for_len)]
serie_2fit_American=ts(serie_2fit_American)
serie_2for_American=ts(serie_2for_American)

serie_2fit_Luft=serie_Luft[c(1:fit_len)]
serie_2for_Luft=serie_Luft[c(fit_len+1:for_len)]
serie_2fit_Luft=ts(serie_2fit_Luft)
serie_2for_Luft=ts(serie_2for_Luft)

serie_2fit_Latam=serie_Latam[c(1:fit_len)]
serie_2for_Latam=serie_Latam[c(fit_len+1:for_len)]
serie_2fit_Latam=ts(serie_2fit_Latam)
serie_2for_Latam=ts(serie_2for_Latam)

serie_2fit_AirFrance=serie_AirFrance[c(1:fit_len)]
serie_2for_AirFrance=serie_AirFrance[c(fit_len+1:for_len)]
serie_2fit_AirFrance=ts(serie_2fit_AirFrance)
serie_2for_AirFrance=ts(serie_2for_AirFrance)



'Visualizacao da Serie - Inutil'
VAR_data=cbind(serie_2fit_Japan,serie_2fit_IAG,serie_2fit_ChinaEast,serie_2fit_ChinaSouth,serie_2fit_Delta,serie_2fit_United,serie_2fit_American,serie_2fit_Luft,serie_2fit_Latam,serie_2fit_AirFrance)
colnames(VAR_data) <- c("Japan","IAG","ChinaSouth","ChinaEast","Delta","United","American","Lufthansa","Latam","AirFranceKLM")
print(tail(VAR_data))


'Seleção de Lag de acordo com os critérios AIC e afins'
lagselect = VARselect(VAR_data, lag.max = 15, type = "const")
print(lagselect)

'Estimando modelo VAR com p definido acima'
VAR_est = VAR(VAR_data, p = 9,season=NULL)

'Plotando resultado da estimativa'
#summary(VAR_est)

'Escrevendo resultado da estimativa em um arquivo txt'
sink("Imagens/VAR_est/summary_VAR_est.txt")
print(summary(VAR_est))
sink()


'Plotando modelo estimado'
plot(VAR_est) #plotar 10 grafios 1 de cada vez e ir salvando se desejado

'Fazendo a previsão'
VAR_for = predict(VAR_est, n.ahead = for_len, ci = 0.9)

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/VAR_for.jpeg",width = 658, height = 553)
plot(VAR_for)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/VAR/Imagens/Fancharts_forecast/fanchart_Japan.jpeg",width = 658, height = 553)
fanchart(VAR_for, names = "Japan", main = "Fanchart for Japan", xlab = "Horizon", ylab = "Log-Return")
dev.off()
fanchart(VAR_for, names = "IAG", main = "Fanchart for IAG", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "ChinaSouth", main = "Fanchart for China South", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "ChinaEast", main = "Fanchart for China East", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "Delta", main = "Fanchart for Delta", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "United", main = "Fanchart for United", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "American", main = "Fanchart for American", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "Lufthansa", main = "Fanchart for Lufthansa", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "Latam", main = "Fanchart for Latam", xlab = "Horizon", ylab = "Log-Return")
fanchart(VAR_for, names = "AirFranceKLM", main = "Fanchart for AirFrance-KLM", xlab = "Horizon", ylab = "Log-Return")


for (i in 1:len) {
  seriesDF = data.frame(original = as.numeric(serie_2for[[i]]),forecasted = forecast_values[[i]])
  
  plot=ggplot(seriesDF,aes(x=c(1:length(serie_2for[[i]])))) + 
    ggtitle(paste("Forecast Comparison (All)",names[i])) +
    geom_line(aes(y = original, colour = "original")) + 
    geom_line(aes(y = forecasted, colour = "predicted"))
  
  path = paste("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Wavelets/Imagens/",names[i],"/ForecastAll_",names[i],".jpeg",sep = "")
  jpeg(file=path,width = 658, height = 553)
  print(plot)
  dev.off()
}


'RMSE Error'

forecast_values_IAG=VAR_for$fcst$IAG[1:178]
forecast_values_Japan=VAR_for$fcst$Japan[1:178]
forecast_values_ChinaSouth=VAR_for$fcst$ChinaSouth[1:178]
forecast_values_ChinaEast=VAR_for$fcst$ChinaEast[1:178]
forecast_values_Delta=VAR_for$fcst$Delta[1:178]
forecast_values_United=VAR_for$fcst$United[1:178]
forecast_values_American=VAR_for$fcst$American[1:178]
forecast_values_Lufthansa=VAR_for$fcst$Lufthansa[1:178]
forecast_values_AirFranceKLM=VAR_for$fcst$AirFranceKLM[1:178]
forecast_values_Latam=VAR_for$fcst$Latam[1:178]

error=c()
error[1] = rmse(serie_2for_IAG,forecast_values_IAG)
error[2] = rmse(serie_2for_Japan,forecast_values_Japan)
error[3] = rmse(serie_2for_ChinaSouth,forecast_values_ChinaSouth)
error[4] = rmse(serie_2for_ChinaEast,forecast_values_ChinaEast)
error[5] = rmse(serie_2for_Delta,forecast_values_Delta)
error[6] = rmse(serie_2for_United,forecast_values_United)
error[7] = rmse(serie_2for_American,forecast_values_American)
error[8] = rmse(serie_2for_Luft,forecast_values_Lufthansa)
error[9] = rmse(serie_2for_AirFrance,forecast_values_AirFranceKLM)
error[10] = rmse(serie_2for_Latam,forecast_values_Latam)

names=c("IAG","Japan","ChinaSouth","ChinaEast","Delta","United","American","Lufthansa","AirFranceKLM","Latam")

for(i in 1:10){
  print(paste("RMSE Series",names[i],"=",error[i]))
}

'Até aqui apenas utilizamos funcionalidades de estimativa e previsão a fim de comparar a qualidade das previsoes realizadas com o 
ferramental VAR com o anteriormente estudado ARIMA
Agora vamos introduzir 3 análises novas - Granger Causality Test, Impulse Response e Forecast Error Variance Decomposition'


'Granger Causality'
sink("Imagens/granger_causality/granger_causality_Japan.txt")
print(causality(VAR_est,cause="Japan"))
sink()

sink("Imagens/granger_causality/granger_causality_IAG.txt")
print(causality(VAR_est,cause="IAG"))
sink()

sink("Imagens/granger_causality/granger_causality_ChinaSouth.txt")
print(causality(VAR_est,cause="ChinaSouth"))
sink()

sink("Imagens/granger_causality/granger_causality_ChinaEast.txt")
print(causality(VAR_est,cause="ChinaEast"))
sink()

sink("Imagens/granger_causality/granger_causality_Delta.txt")
print(causality(VAR_est,cause="Delta"))
sink()

sink("Imagens/granger_causality/granger_causality_United.txt")
print(causality(VAR_est,cause="United"))
sink()

sink("Imagens/granger_causality/granger_causality_American.txt")
print(causality(VAR_est,cause="American"))
sink()

sink("Imagens/granger_causality/granger_causality_Lufthansa.txt")
print(causality(VAR_est,cause="Lufthansa"))
sink()

sink("Imagens/granger_causality/granger_causality_AirFranceKLM.txt")
print(causality(VAR_est,cause="AirFranceKLM"))
sink()

sink("Imagens/granger_causality/granger_causality_Latam.txt")
print(causality(VAR_est,cause="Latam"))
sink()

'Impulse Response'

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

