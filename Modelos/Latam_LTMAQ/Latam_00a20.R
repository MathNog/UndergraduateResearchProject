library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="LTMAQ2000a2020.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/AdjClose_Latam_00a20.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Latam 2000-2020",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/LogRet_Latam_00a20.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Latam 2000-2020",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:4700)]
serie_2for=serie[c(4701:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/Serie_Latam_00a20.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Latam 2000-2020 - to fit",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/ACF_Latam_00a20.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Latam 2000-2020 - to fit')#lags relevantes para MA(q) -> 1 e 8
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/PACF_Latam_00a20.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Latam 2000-2020 - to fit')#lags relevantes para AR(p) -> 1 e 8
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit800=FitARMA(serie_ts_2fit, order = c(8,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit008=FitARMA(serie_ts_2fit, order = c(0,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit108=FitARMA(serie_ts_2fit, order = c(1,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)

#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit800)
print(fit800)

coef(fit001)
print(fit001)

coef(fit008)
print(fit008)

coef(fit101)
print(fit101)

coef(fit108)
print(fit108)
#Modelos com menores AIC, BIC e maior loglikelihood: (1,0,0); (1,0,0); (1,0,8)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res100=residuals.FitARMA(fit100)
res108=residuals.FitARMA(fit108)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/Res(1,0,0)_Latam_00a20.jpeg",width = 658, height = 553)
plot(res100,main="Residuals of ARIMA(1,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/Res(1,0,8)_Latam_00a20.jpeg",width = 658, height = 553)
plot(res108,main="Residuals of ARIMA(1,0,8)",type="l")
dev.off()

mean(res100)
mean(res108)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/ACFRes(1,0,0)_Latam_00a20.jpeg",width = 658, height = 553)
Acf(res100,main="ACF for ARIMA(1,0,0) residuals'")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Latam_LTMAQ/Dados_00a20/ACFRes(1,0,8)_Latam_00a20.jpeg",width = 658, height = 553)
Acf(res108,main="ACF for ARIMA(1,0,8) residuals'")
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 