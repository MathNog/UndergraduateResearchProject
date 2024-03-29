library(forecast)
library(tseries)
library(FitARMA)
library(ggplot2)
library(Metrics)

#Setar diret�rio correto
setwd("C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK")

#Leitura dos dados 
arquivo=read.csv(file="1055_HK2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualiza��o dos dados originais
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/AdjClose_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose ChinaSouthern 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/LogRet_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose ChinaSouthern 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma vari�vel do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:2300)]
serie_2for=serie[c(2301:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da s�rie separada para ajuste
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/Serie_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose ChinaSouthern 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/ACF_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose ChinaSouthern 2010-2019')#lags relevantes para MA(q) -> 1, 10 e 16
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/PACF_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose ChinaSouthern 2010-2019')#lags relevantes para AR(p) -> 1, 10 e 16
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=arima(serie_ts_2fit, order = c(1,0,0))
fit001=arima(serie_ts_2fit, order = c(0,0,1))
fit1000=arima(serie_ts_2fit, order = c(10,0,0))
fit0010=arima(serie_ts_2fit, order = c(0,0,10))
fit1600=arima(serie_ts_2fit, order = c(16,0,0))
fit0016=arima(serie_ts_2fit, order = c(0,0,16))
fit101=arima(serie_ts_2fit, order = c(1,0,1))
fit1010=arima(serie_ts_2fit, order = c(1,0,10))
fit1016=arima(serie_ts_2fit, order = c(1,0,16))
fit1001=arima(serie_ts_2fit, order = c(10,0,1))
fit1601=arima(serie_ts_2fit, order = c(16,0,1))

#coeficientes dos modelos 
print(fit100)

print(fit001)

print(fit1000)

print(fit0010)

print(fit1600)

print(fit0016)

print(fit101)

print(fit1010)

print(fit1016)

print(fit1001)

print(fit1601)


#Modelos com menores AIC e maior loglikelihood: (1,0,0); (16,0,1)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/Res(1,0,0)_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit100)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/Res(16,0,1)_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit1601)
dev.off()

#Forecasting
for100=forecast(fit100,h=149,level=c(80,95))
for1601=forecast(fit1601,h=149,level=c(80,95))

accuracy(for100,serie_2for)
accuracy(for1601,serie_2for)

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/For(1,0,0)_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
plot(for100)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/For(16,0,1)_ChinaSouthern_10a19.jpeg",width = 658, height = 553)
plot(for1601)
dev.off()


#All entries
forecast_values=for1601$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (all) ChinaSouth") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/ForecastAll_ChinaSouth.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))

#50 first
forecast_values=for1601$mean
seriesDF = data.frame(original = as.numeric(serie_2for[c(1:50)]),forecasted = forecast_values[c(1:50)])

plot=ggplot(seriesDF,aes(x=c(1:50)))+
  ggtitle("Forecast Comparison (50 first) ChinaSouth") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/Forecast50first_ChinaSouth.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for[c(1:50)],forecast_values[c(1:50)])
print(paste("RMSE Series =",error))

#New split
#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:2429)]
serie_2for=serie[c(2430:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

fit1601=arima(serie_ts_2fit, order = c(16,0,1))

for1601=forecast(fit1601,h=20,level=c(80,95))

#accuracy(for10016,serie_2for)

forecast_values=for1601$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (20 last) ChinaSouth") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/ChinaSouthern_1055_HK/Dados_10a19FOR/Forecast20last_ChinaSouth.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))
