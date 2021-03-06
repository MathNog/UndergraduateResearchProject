library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL")

#Leitura dos dados 
arquivo=read.csv(file="AAL2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualiza��o dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/AdjClose_American_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose American 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/LogRet_American_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose American 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/Serie_American_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose American 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/ACF_American_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose American 2010-2019')#lags relevantes para MA(q) -> 8, 12, 14
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/PACF_American_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose American 2010-2019')#lags relevantes para AR(p) -> 8, 12, 14
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit800=arima(serie_ts_2fit, order = c(8,0,0))
fit008=arima(serie_ts_2fit, order = c(0,0,8))
fit1200=arima(serie_ts_2fit, order = c(12,0,0))
fit0012=arima(serie_ts_2fit, order = c(0,0,12))
fit1400=arima(serie_ts_2fit, order = c(14,0,0))
fit0014=arima(serie_ts_2fit, order = c(0,0,14))
fit808=arima(serie_ts_2fit, order = c(8,0,8))
fit1208=arima(serie_ts_2fit, order = c(12,0,8))
fit1408=arima(serie_ts_2fit, order = c(14,0,8))


#coeficientes dos modelos 
print(fit800)

print(fit008)

print(fit1200)

print(fit0012)

print(fit1400)

print(fit0014)

print(fit808)

print(fit1208)

print(fit1408)

#Modelos com menores AIC e maior loglikelihood: (12,0,8); (14,0,8)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/Res(12,0,8)_American_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit1208)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/Res(14,0,8)_American_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit1408)
dev.off()

#Forecasting
for1208=forecast(fit1208,h=114,level=c(80,95))
for1408=forecast(fit1408,h=114,level=c(80,95))

accuracy(for1208,serie_2for)
accuracy(for1408,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/For(12,0,8)_American_10a19.jpeg",width = 658, height = 553)
plot(for1208)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19FOR/For(14,0,8)_American_10a19.jpeg",width = 658, height = 553)
plot(for1408)
dev.off()
