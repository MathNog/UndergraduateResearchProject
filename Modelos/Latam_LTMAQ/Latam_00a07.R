library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ")

#Leitura dos dados -> preços
arquivo=read.csv(file="LTMAQ2000a2007.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/AdjClose_Latam_00a07.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Latam 2000-2007",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/LogRet_Latam_00a07.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Latam 2000-2007",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:1700)]
serie_2for=serie[c(1701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/Serie_Latam_00a07.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Latam 2000-2007 - to fit",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/ACF_Latam_00a07.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Latam 2000-2007 - to fit')#lags relevantes para MA(q) -> 1 e 2
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/PACF_Latam_00a07.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Latam 2000-2007 - to fit')#lags relevantes para AR(p) -> 1 e 12
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit200=FitARMA(serie_ts_2fit, order = c(2,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit0012=FitARMA(serie_ts_2fit, order = c(0,0,12), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1012=FitARMA(serie_ts_2fit, order = c(1,0,12), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit201=FitARMA(serie_ts_2fit, order = c(2,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit2012=FitARMA(serie_ts_2fit, order = c(2,0,12), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit200)
print(fit200)

coef(fit001)
print(fit001)

coef(fit0012)
print(fit0012)

coef(fit101)
print(fit101)

coef(fit1012)
print(fit1012)

coef(fit201)
print(fit201)

coef(fit2012)
print(fit2012)


#Modelos com menores AIC, BIC e maior loglikelihood: (2,0,12); (1,0,0); (2,0,12)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res2012=residuals.FitARMA(fit2012)
res100=residuals.FitARMA(fit100)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/Res(2,0,12)_Latam_00a07.jpeg",width = 658, height = 553)
plot(res2012,main="Residuals of ARIMA(2,0,12)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/Res(1,0,0)_Latam_00a07.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0)",type="l")
dev.off()

mean(res2012)
mean(res100)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/ACFRes(2,0,12)_Latam_00a07.jpeg",width = 658, height = 553)
Acf(res2012,main="ACF for ARIMA(2,0,12) residuals'")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a07/ACFRes(1,0,0)_Latam_00a07.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) residuals'")#não bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 