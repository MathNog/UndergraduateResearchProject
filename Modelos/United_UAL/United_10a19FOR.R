library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL")

#Leitura dos dados 
arquivo=read.csv(file="UAL2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/AdjClose_United_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose United 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjClose_vec[t]-AdjClose_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/LogRet_United_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose United 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/Serie_United_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose United 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/ACF_United_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose United 2010-2019')#lags relevantes para MA(q) -> 23??
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/PACF_United_10a19.jpeg",width = 658, height = 553)
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

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/Res(23,0,0)_United_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit2300)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/Res(23,0,23)_United_10a19.jpeg",width = 658, height = 553)
checkresiduals(fit23023)
dev.off()

#Forecasting
for2300=forecast(fit2300,h=113,level=c(80,95))
for23023=forecast(fit23023,h=113,level=c(80,95))

accuracy(for2300,serie_2for)
accuracy(for23023,serie_2for)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/For(23,0,0)_United_10a19.jpeg",width = 658, height = 553)
plot(for2300)
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/United_UAL/Dados_10a19FOR/For(23,0,23)_United_10a19.jpeg",width = 658, height = 553)
plot(for23023)
dev.off()
