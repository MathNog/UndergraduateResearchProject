library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L")

#Leitura dos dados 
arquivo=read.csv(file="IAG_L2003a2020.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]

#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/AdjClose_IAG_03a20.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose IAG 2003-2020",type="l")
dev.off()

#Para obter o log-return a partir dos preços -> limpar NaNs
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/LogRet_IAG_03a20.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose IAG 2003-2020",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeS eries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:4000)]
serie_2for=serie[c(4001:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/Serie_IAG_03a20.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Serie Temporal Log-return AdjClose IAG 2003-2020",type="l")
dev.off()

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/ACF_IAG_03a20.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF Serie Temporal Log-return AdjClose IAG 2003-2020')#lags relevantes para MA(q) -> 1, 2 e 30
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/PACF_IAG_03a20.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF Serie Temporal Log-return AdjClose IAG 2003-2020')#lags relevantes para AR(p) -> 1, 2 e 30
dev.off()


#Ajustando com base nas ACF e PACF -> classe FitARMA
fit100=FitARMA(serie_ts_2fit, order = c(1,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit001=FitARMA(serie_ts_2fit, order = c(0,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit200=FitARMA(serie_ts_2fit, order = c(2,0,0), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit002=FitARMA(serie_ts_2fit, order = c(0,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit101=FitARMA(serie_ts_2fit, order = c(1,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit102=FitARMA(serie_ts_2fit, order = c(1,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit201=FitARMA(serie_ts_2fit, order = c(2,0,1), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)
fit202=FitARMA(serie_ts_2fit, order = c(2,0,2), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30, MaxLag = 30)



#coeficientes dos modelos 
coef(fit100)
print(fit100)

coef(fit001)
print(fit001)

coef(fit200)
print(fit200)

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
#Modelos com menores AIC, BIC e maior loglikelihood: (2,0,0); (0,0,1); (2,0,2)

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res200=residuals.FitARMA(fit200)
res001=residuals.FitARMA(fit001)
res202=residuals.FitARMA(fit202)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/Res(2,0,0)_IAG_03a20.jpeg",width = 658, height = 553)
plot(res200,main="Residuals of ARIMA(1,0,0) IAG 2003-2020",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/Res(0,0,1)_IAG_03a20.jpeg",width = 658, height = 553)
plot(res001,main="Residuals of ARIMA(1,0,7) IAG 2003-2020",type="l")
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/Res(2,0,2)_IAG_03a20.jpeg",width = 658, height = 553)
plot(res202,main="Residuals of ARIMA(0,0,7) IAG 2003-2020",type="l")
dev.off()

mean(res200)
mean(res001)
mean(res202)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/ACFRes(2,0,0)_IAG_03a20.jpeg",width = 658, height = 553)
Acf(res200,main="ACF for ARIMA(1,0,0) IAG 2003-2020 residuals")#nao mt bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/ACFRes(0,0,1)_IAG_03a20.jpeg",width = 658, height = 553)
Acf(res001,main="ACF for ARIMA(1,0,7) IAG 2003-2020 residuals")#bom
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/IAG_IAG_L/Dados_03a20/ACFRes(2,0,2)_IAG_03a20.jpeg",width = 658, height = 553)
Acf(res202,main="ACF for ARIMA(0,0,7) IAG 2003-2020 residuals")#bom
dev.off()
#Média próxima de zero, resíduos não correlacionados -> parece 

