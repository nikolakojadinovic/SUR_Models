#Relativo a la programación de gráficos en R

# Limpiando el Global Enviorement
rm(list=ls())

# Descargar los paquetes 
install.packages("readxl") # Importar de excel a RStudio
install.packages("lubridate") # permite trabajar con las fechas
install.packages("tseries") # fechas

# importing the data
library("readxl")
Morosidad <- read_excel("C:\\Users\\usuario\\Desktop\\libro_prog\\Stress_testing.xlsx", sheet = "Data")
# Muestra la base de datos
#View(Morosidad)

#Relativo a la programación de gráficos en R

#Generando el rezago de la variable morosidad
Morosidad$lmora<-shift(Morosidad$mora,-1)

#generate diferencias de las variables
Morosidad$dmora<-shift(Morosidad$mora,-1)

# Generango una variables en formato date
Morosidad$fecinf<-as.Date(Morosidad$fecha)

#******************************************************************************
# Función que permite ahcer rezagos en R Studio
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

View(Morosidad)

#Elgiendo determinados campos para comenzar a trabajar
data<-c(Morosidad$mora, Morosidad$pbi_ag_des, Morosidad$tcr_ag,Morosidad$unem)
mora<-c(Morosidad$mora)
pbi<-c(Morosidad$pbi_ag_des)
unem<-c(Morosidad$unem)
tc<-c(Morosidad$tcr_ag)
lmora<-c(Morosidad$lmora)

# Creando un data frame (base de datos)
data=data.frame(mora, lmora,pbi, tc,unem)
#Se coloca de esta manera porque el lmora tiene NA y no per
#mite correr el loop

data1=data.frame(mora ,pbi, tc, unem)

# Gráfico de una sola variable

ts.con<-ts(mora, frequency=12, start = c(2005,1)) # Ratio de mora de Consumo
plot(ts.con)

ts.pbi<-ts(pbi, frequency=12, start = c(2012,1)) # Tasa de crecimiento del pbi
plot(ts.pbi)

View(ts.con)

#Respecto a una variable

plot.ts(mora)
plot.ts(diff(mora)) #Primera diferencia
plot(diff(log(mora)),ylab='Cambio en logaritmos(mora)',type='l')
plot(mora,ylab=expression(Y[t]),type='o')

acf(mora)
pacf(mora)
plot(acf, type="h", xlab="lag")
plot(pacf, type="h", xlab="lag")
abline(h=0)
acf(diff(mora), 11) 
pacf(diff(mora), 11)

##########################################################################
##########################################################################

install.packages("urca")
library (urca) 

y <???ts(na.omit( mora, start =2005, end=2018, frequency =12)) 
op <??? par(no.readonly=TRUE) 
layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE) ) 
plot(y, ylab= "mora (logarithm)") 
acf(y,main='Autocorrelations',ylab=' ',ylim=c(???1,1))
pacf(y,main='PartialAutocorrelations',ylab=' ', ylim=c(???1,1)) 
par(op) 

#Mejora el gráfico previamente indicado
#Descargar previamente forecast package
install.packages("forecast")
library(forecast)

ggtsdisplay(y)

install.packages("quantmod")
library(quantmod)

##tentative ARMA(2 ,0) 

arma20 <??? arima(y,order=c(2,0,0)) 
ll <??? logLik(arma20)
aic20 <??? arma20$aic 
res20 <??? residuals(arma20) 
Box.test(res20,lag =20,type = "Ljung???Box") 
shapiro.test(res20 ) 

##ARMA(3,0) 20

arma30 <??? arima(y,order=c(3,0,0)) 
ll30 <??? logLik(arma30) 
aic30 <??? arma30$aic 
lrtest <??? as.numeric(2???(ll30???ll20)) 
chi.pval <??? pchisq(lrtest,df=1,lower.tail=FALSE) 

##ARMA( 1 , 1) 26

arma11 <??? arima(y,order=c(1,0,1)) 
ll11 <??? logLik(arma11) 
aic11 <??? arma11$aic 
tsdiag(arma11) 
res11 <??? residuals(arma11) 
Box.test(res11, lag=20, type = "Ljung???Box") 
shapiro.test( res11) 
tsdiag(arma11)

##Using auto.arima( ) 

install.packages("forecast")
library(forecast)

auto.arima(y,max.p=3,max.q=3,start.p=1,
              start.q=1,ic="aic")

#Otra forma de escoger el mejor arima segun los criterios de información

#for (i in 0:2) for (j in 0:2) {
#  fit.aic <- AIC(arima(resid(pbi), order = c(i, 0, j)))
#  if (fit.aic < best.aic) {
#    best.order <- c(i, 0, j)
#   best.arma <- arima(resid(pbi), order = best.order)
#   best.aic <- fit.aic
# }
# }


#Se puede tipear esto para encontrar el mejor orden
#best.order
#acf(resid(best.arma))

#############################################################################################
#############################################################################################
#LUEGO DE ESCOGER EL MODELO "ARMA20"

armanew.pred<???predict(arma20,n.ahead=10) 
predict<???ts(c(rep(NA,length(y)???1), y[length(y)], arma20.pred$pred), start= 2005,frequency= 12) 
upper<???ts(c(rep(NA,length(y)???1),y[length(y)], 
             armanew.pred$pred+2???armanew.pred$se),start = 2005, frequency=12)
lower<???ts(c(rep(NA,length(y)???1), y[length(y)],
                       armanew.pred$pred???2???armanew.pred$se),
                   start=2005, frequency= 12)
observed<???ts(c(y,rep(NA,10)),start=2005,
                         frequency=12) 

## Plot of atual and forecasted values 

plot(observed, type = "l", 
          ylab="Actual and predicted values", xlab = "") 
lines(predict, col = "blue", lty = 2) 
lines(lower, col = "red", lty = 5) 
lines(upper, col = "red", lty = 5) 
abline(v = 2017, col= "gray" ,lty = 3)

#####################################################################################
#####################################################################################

### fit model (iii)
mora.fit = arima(mora,order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
mora.fit # to view the results
tsdiag(mora, gof.lag=30) # diagnostics

### forecasts for the final model
morafit.pr = predict(mora.fit, n.ahead=12)
U = morafit.pr$pred + 2*morafit.pr$se
L = morafit.pr$pred - 2*morafit.pr$se
month=1:158

###plot actual forecast 
plot(month, mora[month], type="o", xlim=c(1,200), ylim=c(1,4), ylab="Mora")
lines(morafit.pr$pred, col="red", type="o")
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
abline(v=159,lty="dotted")

####################################################################################
####################################################################################
#Metodos de suavizamiento, cambiar h para saber como se comportara 

library(forecast) 

mora.msts <- msts(mora, seasonal.periods = c(12,24,36,48,60,72,84,96,108,120,132,144,156))
mora.tbats <- tbats(mora.msts)
mora.pred <- forecast(mora.tbats, h = 6)
mora.stlm <- stlm(mora.msts, s.window = "periodic", method = "ets")
#mora.stlm.pred <- forecast(mora.stlm, h = 20)
par(mfrow = c(1, 2))
plot(mora.pred, ylim = c(0, 6), xlab = "Meses", ylab = "mora mensual", main = "TBATS")
plot(mora.pred, ylim = c(0, 6), xlab = "Meses", ylab = "mora mensual", main = "TBATS")
#plot(mora.stlm.pred, ylim = c(0, 6), xlab = "Meses", ylab = "mora mensual", main = "STL + ETS")

######################################################################################
######################################################################################

install.packages("tseries")
install.packages("urdf")
install.packages("fUnitRoots")
install.packages("uroot")
install.packages("timeSeries")
install.packages("fBasics")
install.packages("timeDate")
install.packages("urca")
install.packages("adfTest")

library("tseries") 
library("fUnitRoots") 
library("uroot") 
library("adfTest") 
library("urca")

#ADF
mora_1 <??? ts(mora,start=c(2005,1),end=c(2018,2), frequency=12) 
mora_1a.ct <??? ur.df(mora_1,lags=3, type='trend') 
mora_1a.co <??? ur.df(mora_1,lags=3, type='drift') 
mora_2 <??? diff(mora_1) 
mora_2a.ct <??? ur.df(mora_2,lags=3, type=c('trend'))

summary(mora_1a.ct)
summary(mora_1a.co)
summary(mora_2a.ct)
plot(mora_1a.ct)
plot(mora_1a.co)
plot(mora_2a.ct)

#PhillipsP
mora_1<???ts(mora,start=c(2005,1),end=c(2018,2),frequency=12) 
mora_1b.ct<???ur.pp(mora_1,type='Z???tau',model='trend',lags='long') 
mora_1b.co<???ur.pp(mora_1,type='Z???tau',model='constant',lags='long') 
mora_2<??? diff(mora_1) 
mora_2b.ct<???ur.pp(mora_2,type='Z???tau',model='trend',lags='long')

summary(mora_1b.ct)
summary(mora_1b.ct)
summary(mora_1b.co)
summary(mora_2b.ct)
plot(mora_2b.ct)
plot(mora_1b.ct)
plot(mora_1b.co)
plot(mora_2b.ct)

#Elliot.R.S
mora_1c<???log(na.omit(mora)) 
mora_1c.d<???diff(mora)
mora.ct.df<???ur.ers(mora,type="DF???GLS",model="trend",lag.max=4) 
mora.ct.pt<???ur.ers(mora,type="P???test",model="trend") 
mora_1c.d.ct.df<???ur.ers(mora_1c.d,type="DF???GLS", model="trend", lag.max=4)
mora_1c.d.ct.pt<???ur.ers(mora_1c,type="P???test",model="trend")

summary(mora_1c)
summary(mora_1c.d)
summary(mora.ct.df)
summary(mora.ct.pt)
summary(mora_1c.d.ct.df)
summary(mora_1c.d.ct.pt)
plot(mora_1c)
plot(mora_1c.d)
plot(mora.ct.df)
#plot(mora.ct.pt)
plot(mora_1c.d.ct.df)
#plot(mora_1c.d.ct.pt)

#KPSS
mora_1d<???na.omit(mora) 
mora_1e<???log(na.omit(mora)) 
mora_1d.kpss<???ur.kpss(mora_1d,type="mu",use.lag=8) 
mora_1e.kpss<???ur.kpss(mora_1e,type="tau",use.lag=8)

summary(mora_1d.kpss)
summary(mora_1e.kpss)
plot(mora_1d.kpss)
plot(mora_1e.kpss)

#Extracting values from function kpss, ets, adf, pp
#names(summary(mora_1d))
#tabla=matrix(c(mora_1.ct,mora_2.ct,ncol=2))
#colnames(tabla)=c("Tests","Tests")
#rownames(tabla)=c("ADF","ADF_2")

#Loops for ADF and PP

#For ADF

nlag=NULL
testvalue=NULL#create null vectors to add results in
for (i in 1:3){
  x=ur.df(data1[,i+1], type = "drift", selectlags = "AIC")
  a=x@lags
  nlag=rbind(nlag, a)
  b=x@teststat[1]
  testvalue=rbind(testvalue,b)
}
stationary_1=cbind(colnames(data1[-1]), nlag, testvalue)#to avoid Date column added [-1]

stationary_1

nlag=NULL
testvalue_1=NULL
for (i in 1:3){
  x=ur.df(data1[,i+1], type = "trend", selectlags = "AIC")
  a=x@lags
  nlag=rbind(nlag, a)
  b=x@teststat[1]
  testvalue_1=rbind(testvalue_1,b)
}
stationary_2=cbind(colnames(data1[-1]), nlag, testvalue_1)#to avoid Date column added [-1]

stationary_2

#For PP

x=ur.pp(data1,type="z-tau", model="constant")
#x=ur.pp(data1, type= "z-tau", model="trend")
testvalue_2=NULL
for (i in 1:3){
  x=ur.pp(data1[,i+1], type = "Z-alpha", model = "constant")
  b=x@teststat[1]
  testvalue_2=rbind(testvalue_2,b)
}

stationary_3=cbind(colnames(data1[-1]), testvalue_2)
stationary_3

nlag=NULL
testvalue_3 = NULL
for(i in 1:3){
  x = ur.pp(data1[,i], type="Z-tau", model="trend")
  b=x@teststat[1]
  testvalue_3=rbind(testvalue_3,b)
}
stationary_4=cbind(colnames(data1[-1]), testvalue_3)
stationary_4

#Now cbind testvalueT to stationary1
#stationary2=cbind(colnames(data1[-1]),nlag, testvalue)
#Valores T para drift y trend 
stationary_global=cbind(stationary_1, stationary_2,stationary_3,stationary_4)
stationary_global_1=cbind(stationary_1, testvalue_1,testvalue_2,testvalue_3)
#Similarly you can add critical values, PP Test Statistics and all slots of ur.df object
#Desired output is available
#write.csv(ski, file = "ski.csv", row.names = FALSE)
stationary_global
#Este no permite ver los ultimos valores
stationary_global_1
View(stationary_global_1)
#Este si permite ver los ultimos valores de los t.

#Referente al bucle
#En este caso se propone construir un cuadro con los test de raiz unitaria

#############################################################################
#############################################################################

#Seasonal data

#Zivot y Andrews
mora.n<???log(na.omit(mora))  
za.mora.n<???ur.za(mora.n,model="intercept",lag=7)
plot(za.mora.n)
mora.r<???log(na.omit(mora))
za.mora.r<???ur.za(mora.r,model="both",lag=8)
plot(za.mora.r)
summary(za.mora.n)
summary(za.mora.r)

print(summary(za.mora.r))

#print(za.mora.n, za.mora.r)
#library("uroot") 
#require(uroot)
#moram <??? ts(mora, start= c(2005,1),end=c(2018,4), frequency =12)
#HEGY000<???HEGY.test(wts=moram,itsd=c(0,0,c(0)),selectlags=list(mode=c(1,4,5)))
#HEGY100 <??? HEGY.test(wts=moram,itsd=c(1,0,c(0)),selectlags=list(mode=c(1,4,5))) 
#HEGY110 <??? HEGY.test(wts=moram,itsd=c(1,1,c(0),selectlags=list(mode=c(1,4,5)))
#HEGY101 <??? HEGY.test(wts=moram,itsd=c(1,0,c(1,2,3)),selectlags=list(mode=c(1,4,5))
#HEGY111 <??? HEGY.test(wts=moram,itsd=c(1,1,c(1,2,3)),selectlags=list(mode=c(1,4,5))

#Creating a subsample 
#ventana_mora.ts <??? window(mora, start=c(2008,1), end=c(2010,9)) 
#tsp(ventana_mora.ts)

#######################################################################
#Seccion de gráficos

#----------------------------------
# Evolucón de la morosidad

# Evolución Gráfica de las variables
#-----------------------------------
install.packages("tseries")
install.packages("lubridate")

library(tseries)
library(ggplot2)
library(lubridate)
theme_set(theme_bw())

install.packages("ggplot2")
library(ggplot2)
require(ggplot2)

# labels and breaks for X axis text
lbls <- paste0(month.abb[month(Morosidad$fecinf)], " ", lubridate::year(Morosidad$fecinf))
brks <- Morosidad$fecinf

#Primer gráfico: aún se encuentrapendiente el eje x
ggplot(Morosidad, aes(fecinf,mora))+
  geom_line(na.rm=TRUE)+geom_point()+
  labs(title="Evolución de la Morosidad del BBVA", subtitle="En %",
       y="Morosidad (%)",x="Tiempo",
       caption="Fuente: Monitoring & Planning (By:RChD)")+
  scale_x_date(labels = lbls,breaks = brks)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank()  # Activa el modo en blanco
  )+
  theme(plot.title = element_text(hjust = 0.5))+  # Empleado para ajustar el título al centro
  theme(plot.subtitle = element_text(hjust = 0.5))+  # Empleado para ajustar el subtítulo al centro
  theme(plot.caption = element_text(hjust = 0))   # Empleado para ajustar el caption a la izquierda

#Gráfico alternativo;
ggplot(Morosidad,aes(fecinf,mora)) + geom_line() + geom_point()

#----------------------------------
# Evolucón del pbi

ggplot(Morosidad, aes(fecinf,pbi))+
  geom_line(na.rm=TRUE)+
  labs(title="Evolución del PBI (precios constantes 2007)",y="PBI",x="Tiempo",
       caption="Fuente: Monitoring & Planning (By:RChD)")+
  scale_x_date(labels = lbls,breaks = brks)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank()  # Activa el modo en blanco
  )+
  theme(plot.title = element_text(hjust = 0.5))+  # Empleado para ajustar el título al centro
  theme(plot.subtitle = element_text(hjust = 0.5))+  # Empleado para ajustar el subtítulo al centro
  theme(plot.caption = element_text(hjust = 0))   # Empleado para ajustar el caption a la izquierda

# Correlograma with graphics
#----------------------------------------------------

data=data.frame(mora,pbi, tc,unem)

install.packages("corrplot")

library(corrplot)

corrplot(data, method = "circle")

col <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
corrplot(data, type = "upper", order = "hclust", col = col)
corrplot(data, type = "upper", order = "hclust", tl.col = "darkblue", tl.srt = 45)

install.packages("ggcorrplot")
library(ggcorrplot)

corr <- round(cor(data), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

#Por alguna razón los gráficos de plot no corren, se buscará crear un cuadro de resumen
#de los diferentes tests solo para una variable

#install.packages("dplyr")
#install.packages("tidyr")
#library(tidyr)
#library(dplyr)
#library(tidyr)

#Urca package

#data1=data.frame(mora ,pbi, tc, unem)
#library("urca")
#attach(data1)
#View(data1)

#So far, ski is giving names, lags, and T-value for drift only, We need to add trend to it

#df

#pbi_1 <??? ts(pbi,start=c(2005,1),end=c(2018,2), frequency=12) 
#pbi_1a.ct <??? ur.df(pbi_1,lags=3, type='trend') 
#mora_1a.co <??? ur.df(mora_1,lags=3, type='drift') 
#mora_2 <??? diff(mora_1) 
#mora_2a.ct <??? ur.df(mora_2,lags=3, type=c('trend'))

#summary(pbi_1a.ct)
#summary(mora_1a.co)
#summary(mora_2a.ct)
#plot(pbi_1a.ct)
#plot(mora_1a.co)
#plot(mora_2a.ct)

###########################################################################################################

#Loops for ADF and PP

#For ADF

nlag=NULL
testvalue=NULL#create null vectors to add results in
for (i in 1:3){
  x=ur.df(data1[,i+1], type = "drift", selectlags = "AIC")
  a=x@lags
  nlag=rbind(nlag, a)
  b=x@teststat[1]
  testvalue=rbind(testvalue,b)
}
stationary_1=cbind(colnames(data1[-1]), nlag, testvalue)#to avoid Date column added [-1]

nlag=NULL
testvalue_1=NULL
for (i in 1:3){
  x=ur.df(data1[,i+1], type = "trend", selectlags = "AIC")
  a=x@lags
  nlag=rbind(nlag, a)
  b=x@teststat[1]
  testvalue_1=rbind(testvalue_1,b)
}
stationary_2=cbind(colnames(data1[-1]), nlag, testvalue_1)#to avoid Date column added [-1]

stationary_2

#For PP

x=ur.pp(data1,type="z-tau", model="constant")
#x=ur.pp(data1, type= "z-tau", model="trend")
testvalue_2=NULL
for (i in 1:3){
  x=ur.pp(data1[,i+1], type = "Z-alpha", model = "constant")
  b=x@teststat[1]
  testvalue_2=rbind(testvalue_2,b)
}

stationary_3=cbind(colnames(data1[-1]), testvalue_2)
stationary_3

nlag=NULL
testvalue_3 = NULL
for(i in 1:3){
x = ur.pp(data1[,i], type="Z-tau", model="trend")
b=x@teststat[1]
testvalue_3=rbind(testvalue_3,b)
}
stationary_4=cbind(colnames(data1[-1]), testvalue_3)
stationary_4

#Now cbind testvalueT to stationary1
#stationary2=cbind(colnames(data1[-1]),nlag, testvalue)
#Valores T para drift y trend 
stationary_global=cbind(stationary_1, stationary_2,stationary_3,stationary_4)
stationary_global_1=cbind(stationary_1, testvalue_1,testvalue_2,testvalue_3)
#Similarly you can add critical values, PP Test Statistics and all slots of ur.df object
#Desired output is available
#write.csv(ski, file = "ski.csv", row.names = FALSE)
stationary_global
#Este no permite ver los ultimos valores
stationary_global_1
View(stationary_global_1)
#Este si permite ver los ultimos valores de los t.

#Referente al bucle
#En este caso se propone construir un cuadro con los test de raiz unitaria


