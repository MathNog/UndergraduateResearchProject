library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK")

#Leitura dos dados 
arquivo=read.csv(file="0670_HK2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/AdjClose_ChinaEastern_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose ChinaEastern 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/LogRet_ChinaEastern_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose ChinaEastern 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2300)]
serie_2for=serie[c(2301:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/Serie_ChinaEastern_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose ChinaEastern 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/ACF_ChinaEastern_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose ChinaEastern 2010-2019')#lags relevantes para MA(q) -> 1, 10 e 16
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/PACF_ChinaEastern_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose ChinaEastern 2010-2019')#lags relevantes para AR(p) -> 1, 10 e 16
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
fit10016=arima(serie_ts_2fit, order = c(10,0,16))
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

print(fit10016)

print(fit1601)


#Modelos com menores AIC e maior loglikelihood: (10,0,16); (0,0,1)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/Res(0,0,1)_ChinaEastern_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit001)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/Res(10,0,16)_ChinaEastern_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit10016)
dev.off()

#Forecasting
for001=forecast(fit001,h=158,level=c(80,95))
for10016=forecast(fit10016,h=158,level=c(80,95))

accuracy(for001,serie_2for)
accuracy(for10016,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/For(0,0,1)_ChinaEastern_10a19.jpeg",width = 658, height = 553)
plot(for001)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19FOR/For(10,0,16)_ChinaEastern_10a19.jpeg",width = 658, height = 553)
plot(for10016)
dev.off()
