library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK")

#Leitura dos dados -> pre�os 
arquivo=read.csv(file="1055_HK2000a2020.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualiza��o dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/AdjClose_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose China Southern 2000-2020",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/LogRet_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose China Southern 2000-2020",type="l")
dev.off()

#Colocando BA_logret em uma vari�vel do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:4700)]
serie_2for=serie[c(4701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da s�rie separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/Serie_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose China Southern 2000-2020 - to fit",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/ACF_ChinaSouth_00a20.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose China Southern 2000-2020 - to fit')#lags relevantes para MA(q) -> 1, 6, 10
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/PACF_ChinaSouth_00a20.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose China Southern 2000-2020 - to fit')#lags relevantes para AR(p) -> 1, 6, 10
dev.off()


#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit600=FitARMA(serie_ts_2fit, order = c(6,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1000=FitARMA(serie_ts_2fit, order = c(10,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit006=FitARMA(serie_ts_2fit, order = c(0,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit0010=FitARMA(serie_ts_2fit, order = c(0,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit106=FitARMA(serie_ts_2fit, order = c(1,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit10010=FitARMA(serie_ts_2fit, order = c(1,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit601=FitARMA(serie_ts_2fit, order = c(6,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit600)
print(fit600)

coef(fit1000)
print(fit1000)

coef(fit001)
print(fit001)

coef(fit006)
print(fit006)

coef(fit0010)
print(fit0010)

coef(fit101)
print(fit101)

coef(fit106)
print(fit106)

coef(fit10010)
print(fit10010)

coef(fit601)
print(fit601)
#Modelos com menores AIC, BIC e maior loglikelihood: (1,0,0) ou (0,0,1); (1,0,0) ou (0,0,1); (10,0,0)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
res100=residuals.FitARMA(fit100)
res001=residuals.FitARMA(fit001)
res1000=residuals.FitARMA(fit1000)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/Res(1,0,0)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/Res(0,0,1)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(res001,main="Residuals of ARIMA(0,0,1)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/Res(10,0,0)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
plot(res1000,main="Residuals of ARIMA(10,0,0)",type="l")
dev.off()

mean(res100)
mean(res001)
mean(res1000)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/ACFRes(1,0,0)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) China Southern residuals'")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/ACFRes(0,0,1)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
Acf(res001,main="ACF for ARIMA(0,0,1) China Southern residuals'")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_00a20/ACFRes(10,0,0)_ChinaSouth_00a20.jpeg",width = 658, height = 553)
Acf(res1000,main="ACF for ARIMA(10,0,0) China Southern residuals'")#melhor
dev.off()
#M�dia pr�xima de zero, res�duos n�o correlacionados -> parece Acf(res001,main="ACF for ARIMA(4,0,4) residuals'")
