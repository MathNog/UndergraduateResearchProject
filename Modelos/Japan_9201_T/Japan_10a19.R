library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="9201_T2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[c(150:length(AdjClose_vec))]#o inicio da serie tava esquisito

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/AdjClose_Japan_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Japan Airlines 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/LogRet_Japan_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Japan Airlines 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1400)]
serie_2for=serie[c(1401:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Serie_Japan_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Japan Airlines 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACF_Japan_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Japan Airlines 2010-2019')#lags relevantes para MA(q) -> 1 e 2
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/PACF_Japan_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Japan Airlines 2010-2019')#lags relevantes para AR(p) -> 1 e 2
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit200=FitARMA(serie_ts_2fit, order = c(2,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit002=FitARMA(serie_ts_2fit, order = c(0,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit102=FitARMA(serie_ts_2fit, order = c(1,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit201=FitARMA(serie_ts_2fit, order = c(2,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit202=FitARMA(serie_ts_2fit, order = c(2,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit200)
print(fit200)

coef(fit001)
print(fit001)

coef(fit002)
print(fit002)

coef(fit101)
print(fit101)

coef(fit102)
print(fit102)

coef(fit201)
print(fit201)

coef(fit202)
print(fit202)
#Modelos com menores AIC, BIC e maior loglikelihood: (2,0,0) ou (0,0,2); (1,0,0); (1,0,2) ou (2,0,2)


#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res100=residuals.FitARMA(fit100)
res200=residuals.FitARMA(fit200)
res002=residuals.FitARMA(fit002)
res102=residuals.FitARMA(fit102)
res202=residuals.FitARMA(fit202)


jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Res(1,0,0)_Japan_10a19.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Res(2,0,0)_Japan_10a19.jpeg",width = 658, height = 553)
plot(res200,main="Residuals of ARIMA(2,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Res(0,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
plot(res002,main="Residuals of ARIMA(0,0,2)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Res(1,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
plot(res102,main="Residuals of ARIMA(1,0,2)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/Res(2,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
plot(res202,main="Residuals of ARIMA(2,0,2)",type="l")
dev.off()

mean(res100)
mean(res200)
mean(res002)
mean(res102)
mean(res202)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACFRes(1,0,0)_Japan_10a19.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACFRes(2,0,0)_Japan_10a19.jpeg",width = 658, height = 553)
Acf(res200,main="ACF for ARIMA(2,0,0) residuals'")#bom      
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACFRes(0,0,1)_Japan_10a19.jpeg",width = 658, height = 553)
Acf(res002,main="ACF for ARIMA(0,0,2) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACFRes(1,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
Acf(res102,main="ACF for ARIMA(1,0,2) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Japan_9201_T/Dados_10a19/ACFRes(2,0,2)_Japan_10a19.jpeg",width = 658, height = 553)
Acf(res202,main="ACF for ARIMA(2,0,2) residuals'")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 