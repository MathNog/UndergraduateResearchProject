#Gap esquisito logo antes de 2000 -> corrigido pelo tsclean

library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK")

#Leitura dos dados -> preços 
arquivo=read.csv(file="0670_HK2000a2007.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/AdjClose_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose China Eastern 2000 a 2007",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/LogRet_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose China Eastern 2000 a 2007",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1700)]
serie_2for=serie[c(1701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/Serie_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose China Eastern 2000 a 2007",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/ACF_ChinaEast_00a07.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose China Eastern 2000 a 2007 - to fit')#lags relevantes para MA(q) -> 2 e 8
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/PACF_ChinaEast_00a07.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose China Eastern 2000 a 2007 - to fit')#lags relevantes para AR(p) -> 2 e 8
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit200=FitARMA(serie_ts_2fit, order = c(2,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit002=FitARMA(serie_ts_2fit, order = c(0,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit008=FitARMA(serie_ts_2fit, order = c(0,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit202=FitARMA(serie_ts_2fit, order = c(2,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit208=FitARMA(serie_ts_2fit, order = c(2,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit800=FitARMA(serie_ts_2fit, order = c(8,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


#coeficientes dos modelos 
coef(fit200)
print(fit200)

coef(fit002)
print(fit002)

coef(fit008)
print(fit008)

coef(fit202)
print(fit202)

coef(fit208)
print(fit208)

coef(fit800)
print(fit800)
#Modelos com menores AIC, BIC e maior loglikelihood: (2,0,8); (0,0,8); (2,0,8) ou (8,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res008=residuals.FitARMA(fit008)
res208=residuals.FitARMA(fit208)
res800=residuals.FitARMA(fit800)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/Res(0,0,8)_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(res008,main="Residuals of ARIMA(0,0,8)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/Res(2,0,8)_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(res208,main="Residuals of ARIMA(2,0,8)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/Res(8,0,0)_ChinaEast_00a07.jpeg",width = 658, height = 553)
plot(res800,main="Residuals of ARIMA(8,0,0)",type="l")
dev.off()

mean(res008)
mean(res208)
mean(res800)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/ACFRes(0,0,8)_ChinaEast_00a07.jpeg",width = 658, height = 553)
Acf(res008,main="ACF for ARIMA(0,0,8) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/ACFRes(2,0,8)_ChinaEast_00a07.jpeg",width = 658, height = 553)
Acf(res208,main="ACF for ARIMA(2,0,8) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaEastern_0670_HK/Dados_00a07/ACFRes(8,0,0)_ChinaEast_00a07.jpeg",width = 658, height = 553)
Acf(res800,main="ACF for ARIMA(8,0,0) residuals'")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 