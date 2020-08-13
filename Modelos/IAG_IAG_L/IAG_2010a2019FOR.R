library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L")

#Leitura dos dados 
arquivo=read.csv(file="IAG_L2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/AdjClose_IAG_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose IAG 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/LogRet_IAG_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose IAG 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/Serie_IAG_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose IAG 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/ACF_IAG_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose IAG 2010-2019')#lags relevantes para MA(q) -> 1, 11 e 24
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/PACF_IAG_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose IAG 2010-2019')#lags relevantes para AR(p) -> 1, 11 e 24
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=arima(serie_ts_2fit, order = c(1,0,0))
fit001=arima(serie_ts_2fit, order = c(0,0,1))
fit1100=arima(serie_ts_2fit, order = c(11,0,0))
fit0011=arima(serie_ts_2fit, order = c(0,0,11))
fit2400=arima(serie_ts_2fit, order = c(24,0,0))
fit0024=arima(serie_ts_2fit, order = c(0,0,24))
fit101=arima(serie_ts_2fit, order = c(1,0,1))
fit1011=arima(serie_ts_2fit, order = c(1,0,11))
fit1024=arima(serie_ts_2fit, order = c(1,0,24))
fit1101=arima(serie_ts_2fit, order = c(11,0,1))
fit11024=arima(serie_ts_2fit, order = c(11,0,24))
fit2401=arima(serie_ts_2fit, order = c(24,0,1))


#coeficientes dos modelos 
print(fit100)

print(fit001)

print(fit1100)

print(fit0011)

print(fit2400)

print(fit0024)

print(fit101)

print(fit1011)

print(fit1024)

print(fit1101)

print(fit11024)

print(fit2401)

#Modelos com menores AIC e maior loglikelihood: (11,0,24); (1,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/Res(1,0,0)_IAG_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/Res(11,0,24)_IAG_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit11024)
dev.off()

#Forecasting
for100=forecast(fit100,h=115,level=c(80,95))
for11024=forecast(fit11024,h=115,level=c(80,95))

accuracy(for100,serie_2for)
accuracy(for11024,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/For(1,0,0)_IAG_10a19.jpeg",width = 658, height = 553)
plot(for100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19FOR/For(11,0,24)_IAG_10a19.jpeg",width = 658, height = 553)
plot(for11024)
dev.off()
