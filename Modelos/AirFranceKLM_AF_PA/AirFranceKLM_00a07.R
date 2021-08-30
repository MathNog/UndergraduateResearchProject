#Buraco entre 1200 e 1500

library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA")

#Leitura dos dados 
arquivo=read.csv(file="AF_PA2000a2007.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/AdjClose_KLM_00a07.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose AirFrance-KLM 2000-2007",type="l")
dev.off()

#Para obter o log-return a partir dos preços -> limpar NaNs
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/LogRet_KLM_00a07.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose AirFrance-KLM 2000-2007",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeS eries
serie=ts(logret)
serie=tsclean(serie)
#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1800)]
serie_2for=serie[c(1801:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/Serie_KLM_00a07.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2007",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/ACF_KLM_00a07.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2007')#lags relevantes para MA(q) -> 1, 6 e 7
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/PACF_KLM_00a07.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2007')#lags relevantes para AR(p) -> 1, 6, 7 
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit600=FitARMA(serie_ts_2fit, order = c(6,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit006=FitARMA(serie_ts_2fit, order = c(0,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit700=FitARMA(serie_ts_2fit, order = c(7,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit007=FitARMA(serie_ts_2fit, order = c(0,0,7), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit106=FitARMA(serie_ts_2fit, order = c(1,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit107=FitARMA(serie_ts_2fit, order = c(1,0,7), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit601=FitARMA(serie_ts_2fit, order = c(6,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit001)
print(fit001)

coef(fit600)
print(fit600)

coef(fit006)
print(fit006)

coef(fit700)
print(fit700)

coef(fit007)
print(fit007)

coef(fit101)
print(fit101)

coef(fit106)
print(fit106)

coef(fit107)
print(fit107)

coef(fit601)
print(fit601)
#Modelos com menores AIC, BIC e maior loglikelihood: (0,0,7); (1,0,0); (1,0,7)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res100=residuals.FitARMA(fit100)
res107=residuals.FitARMA(fit107)
res007=residuals.FitARMA(fit007)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/Res(1,0,0)_KLM_00a07.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0) AirFrance-KLM 2000-2007",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/Res(1,0,7)_KLM_00a20.jpeg",width = 658, height = 553)
plot(res107,main="Residuals of ARIMA(1,0,7) AirFrance-KLM 2000-2007",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/Res(0,0,7)_KLM_00a20.jpeg",width = 658, height = 553)
plot(res007,main="Residuals of ARIMA(0,0,7) AirFrance-KLM 2000-2007",type="l")
dev.off()

mean(res100)
mean(res107)
mean(res007)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/ACFRes(1,0,0)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) AirFrance-KLM 2000-2007 residuals")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/ACFRes(1,0,7)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res107,main="ACF for ARIMA(1,0,7) AirFrance-KLM 2000-2007 residuals")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a07/ACFRes(0,0,7)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res007,main="ACF for ARIMA(0,0,7) AirFrance-KLM 2000-2007 residuals")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 


