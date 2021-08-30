library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="LHA_DE2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/AdjClose_Luft_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose Lufthansa 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseserie_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/LogRet_Luft_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose Lufthansa 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2400)]
serie_2for=serie[c(2401:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/Serie_Luft_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose Lufthansa 2010-2019 - to fit",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/ACF_Luft_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose Lufthansa 2010-2019 - to fit')#lags relevantes para MA(q) -> 4 e 11
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/PACF_Luft_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose Lufthansa 2010-2019 - to fit')#lags relevantes para AR(p) -> 4 e 11
dev.off()

#Ajustando com base nas ACF e PACF -> classe FitARMA
fit400=FitARMA(serie_ts_2fit, order = c(4,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit1100=FitARMA(serie_ts_2fit, order = c(11,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit004=FitARMA(serie_ts_2fit, order = c(0,0,4), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)


#coeficientes dos modelos 
coef(fit400)
print(fit400)

coef(fit1100)
print(fit1100)

coef(fit004)
print(fit004)

#Modelos com menores AIC, BIC e maior loglikelihood: (4,0,0); (4,0,0); (11,0,0)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res400=residuals.FitARMA(fit400)
res1100=residuals.FitARMA(fit1100)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/Res(4,0,0)_Luft_10a19.jpeg",width = 658, height = 553)
plot(res400,main="Residuals of ARIMA(4,0,0)",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/Res(11,0,0)_Luft_10a19.jpeg",width = 658, height = 553)
plot(res1100,main="Residuals of ARIMA(11,0,0)",type="l")
dev.off()

mean(res400)
mean(res1100)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/ACFRes(4,0,0)_Luft_10a19.jpeg",width = 658, height = 553)
Acf(res400,main="ACF for ARIMA(4,0,0) residuals'")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/Lufthansa_LHA_DE/Dados_10a19/ACFRes(11,0,0)_Luft_10a19.jpeg",width = 658, height = 553)
Acf(res1100,main="ACF for ARIMA(11,0,0) residuals'")
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 

