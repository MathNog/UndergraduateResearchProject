library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L")

#Leitura dos dados 
arquivo=read.csv(file="IAG_L2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/AdjClose_IAG_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose IAG 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços -> limpar NaNs
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/LogRet_IAG_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose IAG 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeS eries
serie=ts(logret)
serie=tsclean(serie)
#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2350)]
serie_2for=serie[c(2351:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/Serie_IAG_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose IAG 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/ACF_IAG_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose IAG 2010-2019')#lags relevantes para MA(q) -> 1 e 11 
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/PACF_IAG_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjCloseIAG 2010-2019')#lags relevantes para AR(p) -> 1 e 11 
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1100=FitARMA(serie_ts_2fit, order = c(11,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit0011=FitARMA(serie_ts_2fit, order = c(0,0,11), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1011=FitARMA(serie_ts_2fit, order = c(1,0,11), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit001)
print(fit001)

coef(fit1100)
print(fit1100)

coef(fit0011)
print(fit0011)

coef(fit101)
print(fit101)

coef(fit1011)
print(fit1011)

#Modelos com menores AIC, BIC e maior loglikelihood: (0,0,1); (0,0,1); (11,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res1100=residuals.FitARMA(fit1100)
res001=residuals.FitARMA(fit001)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/Res(11,0,0)_IAG_10a19.jpeg",width = 658, height = 553)
plot(res1100,main="Residuals of ARIMA(11,0,0) IAG 2010-2019",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/Res(0,0,1)_IAG_10a19.jpeg",width = 658, height = 553)
plot(res001,main="Residuals of ARIMA(0,0,1) IAG 2010-2019",type="l")
dev.off()

mean(res1100)
mean(res001)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/ACFRes(11,0,0)_IAG_10a19.jpeg",width = 658, height = 553)
Acf(res1100,main="ACF for ARIMA(11,0,0) IAG 2010-2019 residuals")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_10a19/ACFRes(0,0,1)_IAG_10a19.jpeg",width = 658, height = 553)
Acf(res001,main="ACF for ARIMA(0,0,1) IAG 2010-2019 residuals")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 

