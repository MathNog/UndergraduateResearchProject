library(forecast)
library(ggplot2)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Japan_9201_T")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="9201_T2000a2007.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]
length(AdjClose)
head(AdjClose)
tail(AdjClose)

#Passando os dados do data frame para um vetor
AdjClose_vec=as.vector(t(AdjClose))
AdjClose_vec=as.numeric(t(AdjClose))


#Vizualização dos dados originais
plot(AdjClose_vec,main="AdjClose Japan Airlines 2000-2007",type="l")

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
plot(logret,main="Log-return AdjClose Japan Airlines 2000-2007",type="l")

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1700)]
serie_2for=serie[c(1701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Japan Airlines 2000-2007 - to fit",type="l")

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Japan Airlines 2000-2007 - to fit')#lags relevantes para MA(q) -> 
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Japan Airlines 2000-2007 - to fit')#lags relevantes para AR(p) -> 

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit303=FitARMA(serie_ts_2fit, order = c(3,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit304=FitARMA(serie_ts_2fit, order = c(3,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit305=FitARMA(serie_ts_2fit, order = c(3,0,5), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit403=FitARMA(serie_ts_2fit, order = c(4,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit404=FitARMA(serie_ts_2fit, order = c(4,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit405=FitARMA(serie_ts_2fit, order = c(4,0,5), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit503=FitARMA(serie_ts_2fit, order = c(5,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit504=FitARMA(serie_ts_2fit, order = c(5,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit505=FitARMA(serie_ts_2fit, order = c(5,0,5), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


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

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res303=residuals.FitARMA(fit303)
res404=residuals.FitARMA(fit404)

plot(res303,main="Residuals of ARIMA(3,0,3)",type="l")
plot(res403,main="Residuals of ARIMA(4,0,4)",type="l")
mean(res303)
mean(res404)
Acf(res303,main="ACF for ARIMA(3,0,3) residuals'")
Acf(res404,main="ACF for ARIMA(4,0,4) residuals'")
#Média próxima de zero, resíduos não correlacionados -> parece 