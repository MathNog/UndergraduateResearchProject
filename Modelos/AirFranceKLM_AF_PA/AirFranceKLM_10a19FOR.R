library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA")

#Leitura dos dados 
arquivo=read.csv(file="AF_PA2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualiza��o dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/AdjClose_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose AirFranceKLM 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/LogRet_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose AirFranceKLM 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Serie_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose AirFranceKLM 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/ACF_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose AirFranceKLM 2010-2019')#lags relevantes para MA(q) -> 1, 11 e 21
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/PACF_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
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

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Res(1,0,0)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/Res(11,0,11)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit11011)
dev.off()

#Forecasting
for100=forecast(fit100,h=153,level=c(80,95))
for11011=forecast(fit11011,h=153,level=c(80,95))

accuracy(for100,serie_2for)
accuracy(for11011,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/For(1,0,0)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(for100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_10a19FOR/For(11,0,11)_AirFranceKLM_10a19.jpeg",width = 658, height = 553)
plot(for11011)
dev.off()
