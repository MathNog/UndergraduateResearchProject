library(forecast)
library(tseries)
library(FitARMA)
library(ggplot2)
library(Metrics)

#Setar diretório correto
setwd("C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA")

#Leitura dos dados 
arquivo=read.csv(file="AF_PA2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/AdjClose_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose AirFranceKLM 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/LogRet_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose AirFranceKLM 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2400)]
serie_2for=serie[c(2401:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Serie_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose AirFranceKLM 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/ACF_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose AirFranceKLM 2010-2019')#lags relevantes para MA(q) -> 1, 11 e 21
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/PACF_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose AirFranceKLM 2010-2019')#lags relevantes para AR(p) -> 1, 11 e 21
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=arima(serie_ts_2fit, order = c(1,0,0))
fit001=arima(serie_ts_2fit, order = c(0,0,1))
fit1100=arima(serie_ts_2fit, order = c(11,0,0))
fit0011=arima(serie_ts_2fit, order = c(0,0,11))
fit0021=arima(serie_ts_2fit, order = c(0,0,21))
fit101=arima(serie_ts_2fit, order = c(1,0,1))
fit1011=arima(serie_ts_2fit, order = c(1,0,11))
fit1021=arima(serie_ts_2fit, order = c(1,0,21))
fit1101=arima(serie_ts_2fit, order = c(11,0,1))
fit11011=arima(serie_ts_2fit, order = c(11,0,11))
fit2101=arima(serie_ts_2fit, order = c(21,0,1))

#coeficientes dos modelos 
print(fit100)

print(fit001)

print(fit1100)

print(fit0011)

print(fit0021)

print(fit101)

print(fit1011)

print(fit1021)

print(fit1101)

print(fit11011)

print(fit2101)


#Modelos com menores AIC e maior loglikelihood: (1,0,0); (11,0,11)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Res(1,0,0)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit100)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Res(11,0,11)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit11011)
dev.off()

#Forecasting
for100=forecast(fit100,h=153,level=c(80,95))
for11011=forecast(fit11011,h=153,level=c(80,95))

accuracy(for100,serie_2for)
accuracy(for11011,serie_2for)

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/For(1,0,0)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(for100)
dev.off()

jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/For(11,0,11)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(for11011)
dev.off()

#All entries
forecast_values=for11011$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (all) AirFrance-KLM") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/ForecastAll_AirFranceKLM.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))

#50 first
forecast_values=for11011$mean
seriesDF = data.frame(original = as.numeric(serie_2for[c(1:50)]),forecasted = forecast_values[c(1:50)])

plot=ggplot(seriesDF,aes(x=c(1:50)))+
  ggtitle("Forecast Comparison (50 first) AirFrance-KLM") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Forecast50first_AirFranceKLM.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for[c(1:50)],forecast_values[c(1:50)])
print(paste("RMSE Series =",error))

#New split
#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2533)]
serie_2for=serie[c(2534:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

fit11011=arima(serie_ts_2fit, order = c(11,0,11))

for11011=forecast(fit11011,h=20,level=c(80,95))

accuracy(for11011,serie_2for)

forecast_values=for11011$mean
seriesDF = data.frame(original = as.numeric(serie_2for),forecasted = forecast_values)

plot=ggplot(seriesDF,aes(x=c(1:length(forecast_values))))+
  ggtitle("Forecast Comparison (20 last) AirFrance-KLM") + 
  geom_line(aes(y = original, colour = "original")) + 
  geom_line(aes(y = forecasted, colour = "predicted"))


jpeg(file="C:/Users/matno_5/OneDrive/Área de Trabalho/PUC/IC/GitPrivado/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Forecast20last_AirFranceKLM.jpeg",width = 658, height = 553)
print(plot)
dev.off()

error = rmse(serie_2for,forecast_values)
print(paste("RMSE Series =",error))
