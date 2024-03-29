library(forecast)
library(tseries)
library(FitARMA)
library(ggplot2)
library(Metrics)

#Setar diret�rio correto
setwd("C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL")

#Leitura dos dados 
arquivo=read.csv(file="UAL2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualiza��o dos dados originais
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/AdjClose_United_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose United 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/LogRet_United_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose United 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma vari�vel do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:2400)]
serie_2for=serie[c(2401:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da s�rie separada para ajuste
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/Serie_United_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose United 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/ACF_United_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose United 2010-2019')#lags relevantes para MA(q) -> 23??
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/PACF_United_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose United 2010-2019')#lags relevantes para AR(p) -> 23
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit2300=arima(serie_ts_2fit, order = c(23,0,0))
fit0023=arima(serie_ts_2fit, order = c(0,0,23))
fit23023=arima(serie_ts_2fit, order = c(23,0,23))



#coeficientes dos modelos 
print(fit2300)

print(fit0023)

print(fit23023)


#Modelos com menores AIC e maior loglikelihood: (23,0,0); (23,0,23)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/Res(23,0,0)_United_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit2300)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/Res(23,0,23)_United_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit23023)
dev.off()

#Forecasting
for2300=forecast(fit2300,h=113,level=c(80,95))
for23023=forecast(fit23023,h=113,level=c(80,95))

accuracy(for2300,serie_2for)
accuracy(for23023,serie_2for)

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/For(23,0,0)_United_10a19.jpeg",width = 658, height = 553)
plot(for2300)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/For(23,0,23)_United_10a19.jpeg",width = 658, height = 553)
plot(for23023)
dev.off()

#All entries
forecast_values=for2300$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (all) United") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/ForecastAll_United.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))

#50 first
forecast_values=for2300$mean
seriesDF = data.frame(original = as.numeric(serie_2for[c(1:50)]),forecasted = forecast_values[c(1:50)])

plot=ggplot(seriesDF,aes(x=c(1:50)))+
  ggtitle("Forecast Comparison (50 first) United") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/Forecast50first_United.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for[c(1:50)],forecast_values[c(1:50)])
print(paste("RMSE Series =",error))

#New split
#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:2493)]
serie_2for=serie[c(2494:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

fit2300=arima(serie_ts_2fit, order = c(23,0,0))

for2300=forecast(fit2300,h=20,level=c(80,95))

#accuracy(for230016,serie_2for)

forecast_values=for2300$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (20 last) United") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/�rea de Trabalho/PUC/IC/GitPrivado/IC/Modelos/United_UAL/Dados_10a19FOR/Forecast20last_United.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))
