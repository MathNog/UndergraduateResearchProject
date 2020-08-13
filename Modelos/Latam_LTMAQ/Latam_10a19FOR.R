library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ")

#Leitura dos dados 
arquivo=read.csv(file="LTMAQ2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/AdjClose_Latam_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Latam 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/LogRet_Latam_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Latam 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2250)]
serie_2for=serie[c(2251:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/Serie_Latam_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose Latam 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/ACF_Latam_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose Latam 2010-2019')#lags relevantes para MA(q) -> 1, 10 e 19
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/PACF_Latam_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose Latam 2010-2019')#lags relevantes para AR(p) -> 1. 10 e 19
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=arima(serie_ts_2fit, order = c(1,0,0))
fit001=arima(serie_ts_2fit, order = c(0,0,1))
fit1000=arima(serie_ts_2fit, order = c(10,0,0))
fit0010=arima(serie_ts_2fit, order = c(0,0,10))
fit1900=arima(serie_ts_2fit, order = c(19,0,0))
fit0019=arima(serie_ts_2fit, order = c(0,0,19))

fit101=arima(serie_ts_2fit, order = c(1,0,1))
fit1010=arima(serie_ts_2fit, order = c(1,0,10))
fit1019=arima(serie_ts_2fit, order = c(1,0,19))
fit1001=arima(serie_ts_2fit, order = c(10,0,1))


#coeficientes dos modelos 
print(fit100)

print(fit001)

print(fit1000)

print(fit0010)

print(fit1900)

print(fit0019)

print(fit101)

print(fit1010)

print(fit1019)

print(fit1001)

#Modelos com menores AIC e maior loglikelihood: (1,0,0); (1,0,19)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/Res(1,0,0)_Latam_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/Res(1,0,19)_Latam_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit1019)
dev.off()

#Forecasting
for100=forecast(fit100,h=153,level=c(80,95))
for1019=forecast(fit1019,h=153,level=c(80,95))

accuracy(for100,serie_2for)
accuracy(for1019,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/For(1,0,0)_Latam_10a19.jpeg",width = 658, height = 553)
plot(for100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_10a19FOR/For(1,0,19)_Latam_10a19.jpeg",width = 658, height = 553)
plot(for1019)
dev.off()
