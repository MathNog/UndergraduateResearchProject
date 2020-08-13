library(forecast)
library(tseries)
library(FitARMA)

#Setar diretório correto
setwd("C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL")

#Leitura dos dados -> preços BOEING
arquivo=read.csv(file="AAL2010a2019.csv")

#Selecionando apenas o AdjClose -> podemos ignorar os outros
AdjClose=arquivo[c(6)]

#Convertendo para numeric a fim de limpar NA e NAN
AdjClose_vec=as.numeric(t(AdjClose))
AdjClose_vec=AdjClose_vec[!is.na(AdjClose_vec)]
AdjClose_vec=AdjClose_vec[!is.nan(AdjClose_vec)]


#Vizualização dos dados originais
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/AdjClose_American_10a19.jpeg",width = 658, height = 553)
plot(AdjClose_vec,main="AdjClose American 2010-2019",type="l")
dev.off()

#Para obter o log-return a partir dos preços
#Temos que, o retorno rt = ln(Pt)-ln(Pt-1), ou seja, a diferença do log do preço atual e do log do preço em t-1.
#A função diff retorna justamente AdjCloseBA_vec[t]-AdjCloseBA_vec[t-1]
logret=diff(log(AdjClose_vec),lag=1)
logret=logret[!is.na(logret)]
logret=logret[!is.nan(logret)]

#Para vizualizar o retorno, não o preço
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/LogRet_American_10a19.jpeg",width = 658, height = 553)
plot(logret,main="Log-return AdjClose American 2010-2019",type="l")
dev.off()

#Colocando BA_logret em uma variável do tipo TimeSeries
serie=ts(logret)
serie=tsclean(serie)

#Separando a serie em uma parte para ajuste do modelo e outra para comparação com previsão
serie_2fit=serie[c(1:2400)]
serie_2for=serie[c(2401:length(serie))]
serie_ts_2fit=ts(serie_2fit)
serie_ts_2for=ts(serie_2for)

#Plot da série separada para ajuste
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/Serie_American_10a19.jpeg",width = 658, height = 553)
plot(serie_ts_2fit,main="Time Serie Log Return AdjClose American 2010-2019",type="l")
dev.off()

#Tratando sazonalidade
decomp_serie=decompose(serie_ts_2fit)#nao reconhece nada

#dickey fuller test para estacionaridade
adf.test(serie_ts_2fit, alternative = "stationary")
#p-value = 0.01 -> logo há 1% de chance da série apresentar raiz unitária -> a série é estacionária -> d=0

#PACF e PACF para determinar ordem
jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/ACF_American_10a19.jpeg",width = 658, height = 553)
Acf(serie_ts_2fit,main='ACF for Log Return AdjClose American 2010-2019')#lags relevantes para MA(q) -> 8, 12, 14
dev.off()

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/PACF_American_10a19.jpeg",width = 658, height = 553)
Pacf(serie_ts_2fit,main='PACF for Log Return AdjClose American 2010-2019')#lags relevantes para AR(p) -> 8,12,14
dev.off()


#Ajustando com base nas ACF e PACF -> classe FitARMA
fit008=FitARMA(serie_ts_2fit, order = c(0,0,8), demean=TRUE, MeanMLEQ = FALSE,pApprox = 30)



#coeficientes dos modelos 
coef(fit008)
print(fit008)

#Modelos com menores AIC, BIC e maior loglikelihood: 

#Vejamos os resíduos, suas médias e acf dos melhores modelos 
res008=residuals.FitARMA(fit008)


jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/Res(0,0,8)_KLM_10a19.jpeg",width = 658, height = 553)
plot(res008,main="Residuals of ARIMA(0,0,8) American 2010-2019",type="l")
dev.off()

mean(res008)

jpeg(file="C:/Users/Matheus/Desktop/PUC/IC/CiasAereas/Modelos/American_AAL/Dados_10a19/ACFRes(0,0,8)_KLM_10a19.jpeg",width = 658, height = 553)
Acf(res008,main="ACF for ARIMA(0,0,8) American 2010-2019 residuals'")# nao mt bom -> lags perto de 15 relevantes
dev.off()

#Média próxima de zero, resíduos não correlacionados -> parece 
