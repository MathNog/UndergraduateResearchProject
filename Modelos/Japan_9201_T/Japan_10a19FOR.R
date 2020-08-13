library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T")

#Leitura dos dados 
arquivo=read.csv(file="9201_T2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/AdjClose_Japan_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Japan 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/LogRet_Japan_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Japan 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1600)]
serie_2for=serie[c(1601:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/Serie_Japan_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose Japan 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/ACF_Japan_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose Japan 2010-2019')#lags relevantes para MA(q) -> 1,2
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/PACF_Japan_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose Japan 2010-2019')#lags relevantes para AR(p) -> 1,2
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=arima(serie_ts_2fit, order = c(1,0,0))
fit001=arima(serie_ts_2fit, order = c(0,0,1))
fit200=arima(serie_ts_2fit, order = c(2,0,0))
fit002=arima(serie_ts_2fit, order = c(0,0,2))
fit101=arima(serie_ts_2fit, order = c(1,0,1))
fit102=arima(serie_ts_2fit, order = c(1,0,2))
fit201=arima(serie_ts_2fit, order = c(2,0,1))
fit202=arima(serie_ts_2fit, order = c(2,0,2))


#coeficientes dos modelos 
print(fit100)

print(fit001)

print(fit200)

print(fit002)

print(fit101)

print(fit102)

print(fit201)

print(fit202)



#Modelos com menores AIC e maior loglikelihood: (0,0,2); (2,0,2)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/Res(0,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit002)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/Res(2,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit202)
dev.off()

#Forecasting
for002=forecast(fit002,h=188,level=c(80,95))
for202=forecast(fit202,h=188,level=c(80,95))

accuracy(for002,serie_2for)
accuracy(for202,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/For(0,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
plot(for002)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19FOR/For(2,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
plot(for202)
dev.off()
