#Tem um buraco esquisito entre 1000 e 1400 -> limpeios NaNs e NA, além de filtrar com o tsclean

library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="AF_PA_2000a2020.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/AdjClose_KLM_00a20.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose AirFrance-KLM 2000-2020",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/LogRet_KLM_00a20.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose AirFrance-KLM 2000-2020",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)
#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:4200)]
serie_2for=serie[c(4201:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/Serie_KLM_00a20.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2020",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/ACF_KLM_00a20.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2020')#lags relevantes para MA(q) -> 1, 3, e 6
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/PACF_KLM_00a20.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose AirFrance-KLM 2000-2020')#lags relevantes para AR(p) -> 1, 3 e 6
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit300=FitARMA(serie_ts_2fit, order = c(3,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit003=FitARMA(serie_ts_2fit, order = c(0,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit600=FitARMA(serie_ts_2fit, order = c(6,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit006=FitARMA(serie_ts_2fit, order = c(0,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit103=FitARMA(serie_ts_2fit, order = c(1,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit106=FitARMA(serie_ts_2fit, order = c(1,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit301=FitARMA(serie_ts_2fit, order = c(3,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit303=FitARMA(serie_ts_2fit, order = c(3,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit306=FitARMA(serie_ts_2fit, order = c(3,0,6), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit601=FitARMA(serie_ts_2fit, order = c(6,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit603=FitARMA(serie_ts_2fit, order = c(6,0,3), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit001)
print(fit001)

coef(fit606)
print(fit606)

coef(fit003)
print(fit003)

coef(fit600)
print(fit600)

coef(fit006)
print(fit006)

coef(fit101)
print(fit101)

coef(fit103)
print(fit103)

coef(fit106)
print(fit106)

coef(fit301)
print(fit301)

coef(fit303)
print(fit303)

coef(fit306)
print(fit306)

coef(fit601)
print(fit601)

coef(fit603)
print(fit603)
#Modelos com menores AIC, BIC e maior loglikelihood: (3,0,3); (1,0,1); (6,0,3)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res303=residuals.FitARMA(fit303)
res101=residuals.FitARMA(fit101)
res603=residuals.FitARMA(fit603)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/Res(3,0,3)_KLM_00a20.jpeg",width = 658, height = 553)
plot(res303,main="Residuals of ARIMA(3,0,3) AirFrance-KLM 2000-2020",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/Res(1,0,1)_KLM_00a20.jpeg",width = 658, height = 553)
plot(res101,main="Residuals of ARIMA(1,0,1) AirFrance-KLM 2000-2020",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/Res(6,0,3)_KLM_00a20.jpeg",width = 658, height = 553)
plot(res603,main="Residuals of ARIMA(6,0,3) AirFrance-KLM 2000-2020",type="l")
dev.off()

mean(res303)
mean(res101)
mean(res603)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/ACFRes(3,0,3)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res303,main="ACF for ARIMA(3,0,3) AirFrance-KLM 2000-2020 residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/ACFRes(1,0,1)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res101,main="ACF for ARIMA(1,0,1) AirFrance-KLM 2000-2020 residuals'")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/AirFranceKLM_AF_PA/Dados_00a20/ACFRes(6,0,3)_KLM_00a20.jpeg",width = 658, height = 553)
Acf(res603,main="ACF for ARIMA(6,0,3) AirFrance-KLM 2000-2020 residuals'")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 
