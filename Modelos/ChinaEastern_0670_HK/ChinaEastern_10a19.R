library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK")

#Leitura dos dados -> pre�os 
arquivo=read.csv(file="0670_HK2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualiza��o dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/AdjClose_ChinaEast_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose China Eastern 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/LogRet_ChinaEast_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose China Eastern 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma vari�vel do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:2300)]
serie_2for=serie[c(2301:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da s�rie separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/Serie_ChinaEast_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose China Eastern 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#ACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/ACF_ChinaEast_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose China Eastern 2010-2019')#lags relevantes para MA(q) -> 1 e 10
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/PACF_ChinaEast_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose China Eastern 2010-2019')#lags relevantes para AR(p) -> 1 e 10
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1000=FitARMA(serie_ts_2fit, order = c(10,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit0010=FitARMA(serie_ts_2fit, order = c(0,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1010=FitARMA(serie_ts_2fit, order = c(1,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit001)
print(fit001)

coef(fit101)
print(fit101)

coef(fit1000)
print(fit1000)

coef(fit0010)
print(fit0010)

coef(fit1010)
print(fit1010)
#Modelos com menores AIC, BIC e maior loglikelihood: (0,0,1); (0,0,1); (10,0,0)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
res1000=residuals.FitARMA(fit1000)
res001=residuals.FitARMA(fit001)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/Res(10,0,0)_ChinaEast_10a19.jpeg",width = 658, height = 553)
plot(res1000,main="Residuals of ARIMA(10,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/Res(0,0,1)_ChinaEast_10a19.jpeg",width = 658, height = 553)
plot(res001,main="Residuals of ARIMA(0,0,1)",type="l")
dev.off()

mean(res1000)
mean(res001)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/ACFRes(10,0,0)_ChinaEast_10a19.jpeg",width = 658, height = 553)
Acf(res1000,main="ACF for ARIMA(10,0,0) residuals'")#lag 16 relevante
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_10a19/ACFRes(0,0,1)_ChinaEast_10a19.jpeg",width = 658, height = 553)
Acf(res001,main="ACF for ARIMA(0,0,1) residuals'")#lag 16 relevante
dev.off()
#M�dia pr�xima de zero, res�duos n�o correlacionados -> parece 