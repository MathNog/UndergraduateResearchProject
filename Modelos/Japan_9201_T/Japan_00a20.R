library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T")

#Leitura dos dados -> pre�os 
arquivo=read.csv(file="9201_T2000a2020.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualiza��o dos dados originais
plot(AdjClose_vec,main="AdjClose Japan Airlines 2000-2020",type="l")

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
plot(logret,main="Log-return AdjClose Japan Airlines 2000-2020",type="l")

#Colocando BA_logret em uma vari�vel do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)
#Separando a serie em uma parte para ajuste do modelo e outra para compara��o com previs�o
serie_2fit=serie[c(1:4700)]
serie_2for=serie[c(4701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da s�rie separada para ajuste
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Japan Airlines 2000-2020 - to fit",type="l")

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Japan Airlines 2000-2020 - to fit')#lags relevantes para MA(q) -> 1, 6
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Japan Airlines 2000-2007 - to fit')#lags relevantes para AR(p) -> 1, 6

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit600=FitARMA(serie_ts_2fit, order = c(6,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit006=FitARMA(serie_ts_2fit, order = c(0,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit106=FitARMA(serie_ts_2fit, order = c(1,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit601=FitARMA(serie_ts_2fit, order = c(6,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit606=FitARMA(serie_ts_2fit, order = c(6,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


#coeficientes dos modelos 
coef(fit303)
print(fit303)

coef(fit304)
print(fit304)

coef(fit305)
print(fit305)

coef(fit403)
print(fit403)

coef(fit404)
print(fit404)

coef(fit405)
print(fit405)

coef(fit503)
print(fit503)

coef(fit504)
print(fit504)

coef(fit505)
print(fit505)
#Modelos com menores AIC, BIC e maior loglikelihood: (4,0,4); (3,0,3); (4,0,4)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
res303=residuals.FitARMA(fit303)
res404=residuals.FitARMA(fit404)

plot(res303,main="Residuals of ARIMA(3,0,3)",type="l")
plot(res403,main="Residuals of ARIMA(4,0,4)",type="l")
mean(res303)
mean(res404)
Acf(res303,main="ACF for ARIMA(3,0,3) residuals'")
Acf(res404,main="ACF for ARIMA(4,0,4) residuals'")
#M�dia pr�xima de zero, res�duos n�o correlacionados -> parece 