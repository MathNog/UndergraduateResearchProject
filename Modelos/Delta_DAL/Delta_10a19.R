library(forecast)
library(tseries)
library(FitARMA)

#Setar diret�rio correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL")

#Leitura dos dados -> pre�os BOEING
arquivo=read.csv(file="DAL2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualiza��o dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/AdjClose_Delta_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Delta 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos pre�os
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferen�a do log do pre�o atual e do log do pre�o em t-1.
#A fun��o diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, n�o o pre�o
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/LogRet_Delta_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Delta 2010-2019",type="l")
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
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/Serie_Delta_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose Delta 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo h� 1% de chance da s�rie apresentar raiz unit�ria -> a s�rie � estacion�ria -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/ACF_Delta_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose Delta 2010-2019')#lags relevantes para MA(q) -> 4,14,16
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/PACF_Delta_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose Delta 2010-2019')#lags relevantes para AR(p) -> 4,8,14
dev.off()

#Continuar ACF e PACF


#Ajustando com base nas ACF e PACF -> classe FitARMA
fit400=FitARMA(serie_ts_2fit, order = c(4,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30)
fit004=FitARMA(serie_ts_2fit, order = c(0,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30)
fit008=FitARMA(serie_ts_2fit, order = c(0,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30)
fit404=FitARMA(serie_ts_2fit, order = c(4,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30)


#coeficientes dos modelos 
coef(fit400)
print(fit400)

coef(fit004)
print(fit004)

coef(fit008)
print(fit008)

coef(fit404)
print(fit404)

#Modelos com menores AIC, BIC e maior loglikelihood: (4,0,4); (0,0,4); (4,0,4)

#Vejamos os res�duos, suas m�dias e acf dos melhores modelos 
res404=residuals.FitARMA(fit404)
res004=residuals.FitARMA(fit004)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/Res(4,0,4)_Delta_10a19.jpeg",width = 658, height = 553)
plot(res404,main="Residuals of ARIMA(4,0,4) Delta 2010-2019",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/Res(0,0,4)_Delta_10a19.jpeg",width = 658, height = 553)
plot(res004,main="Residuals of ARIMA(0,0,4) Delta 2010-2019",type="l")
dev.off()

mean(res404)
mean(res004)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/ACFRes(4,0,4)_Delta_10a19.jpeg",width = 658, height = 553)
Acf(res404,main="ACF for ARIMA(4,0,4) Delta 2010-2019 residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Delta_DAL/Dados_10a19/ACFRes(0,0,4)_Delta_10a19.jpeg",width = 658, height = 553)
Acf(res004,main="ACF for ARIMA(0,0,4) Delta 2010-2019 residuals'")#nao mt bom
dev.off()

#M�dia pr�xima de zero, res�duos n�o correlacionados -> parece 
