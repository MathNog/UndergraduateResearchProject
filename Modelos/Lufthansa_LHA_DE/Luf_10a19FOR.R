library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE")

#Leitura dos dados 
arquivo=read.csv(file="LHA_DE2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/AdjClose_Lufthansa_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Lufthansa 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/LogRet_Lufthansa_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Lufthansa 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/Serie_Lufthansa_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose Lufthansa 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/ACF_Lufthansa_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose Lufthansa 2010-2019')#lags relevantes para MA(q) -> 11 e 21
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/PACF_Lufthansa_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose Lufthansa 2010-2019')#lags relevantes para AR(p) -> 11
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit1100=arima(serie_ts_2fit, order = c(11,0,0))
fit0011=arima(serie_ts_2fit, order = c(0,0,11))
fit2100=arima(serie_ts_2fit, order = c(21,0,0))


#coeficientes dos modelos 
print(fit1100)

print(fit0011)

print(fit2100)




#Modelos com menores AIC e maior loglikelihood: (11,0,0); (21,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/Res(11,0,0)_Lufthansa_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit1100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/Res(21,0,0)_Lufthansa_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit2100)
dev.off()

#Forecasting
for1100=forecast(fit1100,h=134,level=c(80,95))
for2100=forecast(fit2100,h=134,level=c(80,95))

accuracy(for1100,serie_2for)
accuracy(for2100,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/For(11,0,0)_Lufthansa_10a19.jpeg",width = 658, height = 553)
plot(for1100)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19FOR/For(21,0,0)_Lufthansa_10a19.jpeg",width = 658, height = 553)
plot(for2100)
dev.off()
