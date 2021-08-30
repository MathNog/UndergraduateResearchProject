library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="1055_HK2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/AdjClose_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose China Southern 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/LogRet_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose China Southern 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2300)]
serie_2for=serie[c(2301:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/Serie)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose China Southern 2010-2019",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/ACF_ChinaSouth_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose China Southern 2010-2019')#lags relevantes para MA(q) -> 1, 10 e 14
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/PACF_ChinaSouth_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose China Southern 2010-2019')#lags relevantes para AR(p) -> 1, 10 e 14
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1000=FitARMA(serie_ts_2fit, order = c(10,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit0010=FitARMA(serie_ts_2fit, order = c(0,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1010=FitARMA(serie_ts_2fit, order = c(1,0,10), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit700=FitARMA(serie_ts_2fit, order = c(7,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit1000)
print(fit1000)

coef(fit001)
print(fit001)

coef(fit0010)
print(fit0010)

coef(fit101)
print(fit101)

coef(fit1010)
print(fit1010)

coef(fit700)
print(fit700)


#Modelos com menores AIC, BIC e maior loglikelihood: (1,0,0) ou (0,0,1); (1,0,0) ou (0,0,1); (10,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res100=residuals.FitARMA(fit100)
res001=residuals.FitARMA(fit001)
res1000=residuals.FitARMA(fit1000)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/Res(1,0,0)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0) ChinaSouthern 2010-2019",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/Res(0,0,1)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(res001,main="Residuals of ARIMA(0,0,1) ChinaSouthern 2010-2019",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/Res(10,0,0)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
plot(res1000,main="Residuals of ARIMA(10,0,0) ChinaSouthern 2010-2019",type="l")
dev.off()

mean(res100)
mean(res001)
mean(res1000)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/ACFRes(1,0,0)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) ChinaSouthern 2010-2019 residuals")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/ACFRes(0,0,1)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
Acf(res001,main="ACF for ARIMA(0,0,1) ChinaSouthern 2010-2019 residuals")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/ChinaSouthern_1055_HK/Dados_10a19/Res(10,0,0)_ChinaSouth_10a19.jpeg",width = 658, height = 553)
Acf(res1000,main="ACF for ARIMA(10,0,0) ChinaSouthern 2010-2019 residuals")#um pouco melhor
dev.off()
#Média próxima de zero, resíduos não correlacionados -> não parece bom