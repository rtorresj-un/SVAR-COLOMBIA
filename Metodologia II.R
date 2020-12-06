## Spillovers ####
#Universidad Nacional De Colombia
#Metodología de la Investrigación II
#Juanita Cortes Arroyo, Raúl Esteban Torres Jimenez

## Librerías y funciones ####
library(vars);library(urca);library(ggplot2); library(tidyr)
library(ggfortify);library(gridExtra);library(dplyr); library(readr)
library(tidyverse);library(svars);library(AER); library(ggthemes)
library(dynlm);library(readr);library(tsDyn);library(VAR.etp);library(forecast)
# This R function helps to interpret the output of the urca::ur.df function.
interp_urdf <- function(urdf, level) {
  if(class(urdf) != "ur.df") stop('parameter is not of class ur.df from urca package')
  if(!(level %in% c("1pct", "5pct", "10pct") ) ) stop('parameter level is not one of 1pct, 5pct, or 10pct')
  
  cat("========================================================================\n")
  cat( paste("At the", level, "level:\n") )
  if(urdf@model == "none") {
    cat("The model is of type none\n")
    tau1_crit = urdf@cval["tau1",level]
    tau1_teststat = urdf@teststat["statistic","tau1"]
    tau1_teststat_wi_crit = tau1_teststat > tau1_crit
    if(tau1_teststat_wi_crit) {
      cat("tau1: The null hypothesis is not rejected, unit root is present\n")
    } else {
      cat("tau1: The null hypothesis is rejected, unit root is not present\n")
    }
  } else if(urdf@model == "drift") {
    cat("The model is of type drift\n")
    tau2_crit = urdf@cval["tau2",level]
    tau2_teststat = urdf@teststat["statistic","tau2"]
    tau2_teststat_wi_crit = tau2_teststat > tau2_crit
    phi1_crit = urdf@cval["phi1",level]
    phi1_teststat = urdf@teststat["statistic","phi1"]
    phi1_teststat_wi_crit = phi1_teststat < phi1_crit
    if(tau2_teststat_wi_crit) {
      # Unit root present branch
      cat("tau2: The first null hypothesis is not rejected, unit root is present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is drift.\n")
      }
    } else {
      # Unit root not present branch
      cat("tau2: The first null hypothesis is rejected, unit root is not present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
        warning("This is inconsistent with the first null hypothesis.")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there is drift.\n")
      }
    }
  } else if(urdf@model == "trend") {
    cat("The model is of type trend\n")
    tau3_crit = urdf@cval["tau3",level]
    tau3_teststat = urdf@teststat["statistic","tau3"]
    tau3_teststat_wi_crit = tau3_teststat > tau3_crit
    phi2_crit = urdf@cval["phi2",level]
    phi2_teststat = urdf@teststat["statistic","phi2"]
    phi2_teststat_wi_crit = phi2_teststat < phi2_crit
    phi3_crit = urdf@cval["phi3",level]
    phi3_teststat = urdf@teststat["statistic","phi3"]
    phi3_teststat_wi_crit = phi3_teststat < phi3_crit
    if(tau3_teststat_wi_crit) {
      # First null hypothesis is not rejected, Unit root present branch
      cat("tau3: The first null hypothesis is not rejected, unit root is present\n")
      if(phi3_teststat_wi_crit) {
        # Second null hypothesis is not rejected
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is no trend, and there is drift\n")
        }
      }
      else {
        # Second null hypothesis is rejected
        cat("phi3: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is trend, and there may or may not be drift\n")
          warning("Presence of drift is inconclusive.")
        }
      }
    } else {
      # First null hypothesis is rejected, Unit root not present branch
      cat("tau3: The first null hypothesis is rejected, unit root is not present\n")
      if(phi3_teststat_wi_crit) {
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        warning("This is inconsistent with the first null hypothesis.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there is no trend, and there is drift\n")
        }
      } else {
        cat("phi3: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there may or may not be trend\n")
        warning("Presence of trend is inconclusive.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first and second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there may or may not be trend, and there may or may not be drift\n")
          warning("Presence of trend and drift is inconclusive.")
        }
      }
    }
  } else warning('urdf model type is not one of none, drift, or trend')
  cat("========================================================================\n")
}
#Función para graficar impulso respuesta con bootstraping
irf_ggplot<-function(VAR, impulso, respuesta){
  IRF = irf(VAR, impulse=impulso ,response=respuesta,n.ahead = 15,ci=0.68, boot=T, ortho=T, runs=500) #No analizaremos respuestas ortogonales (Ahora vemos qué es eso);  
  data_irf= data.frame(IRF$irf,IRF$Lower,IRF$Upper, c(0:15))
  ggplot(data_irf, aes(x=data_irf[,4], y=data_irf[,1])) +
    geom_line() + 
    geom_ribbon(aes(ymin=data_irf[,2], ymax=data_irf[,3], fill="Bandas al 68% \n de confianza"), alpha=.3) +
    theme_minimal() + scale_color_distiller() + scale_fill_ordinal(name='') +
    ylab("Porcentaje de cambio") +
    xlab("Pasos adelante") + ggtitle(str_c('Respuesta de ',as.character(colnames(data_irf)[1]), ' ante cambios en ', impulso))
}


## Lectura de datos ####
data <- read_delim("DATA_METODOLOGIA.csv", 
                   ";", escape_double = FALSE, col_types = cols(FECHA = col_date(format = "%d/%m/%y")), 
                   trim_ws = TRUE)
head(data)
summary(data)
data1<-subset(data, FECHA>='2008-11-01')
#data <- ts(data, start = c(1998,1), frequency = 12)
autoplot(ts(data1, start = c(1998,1), frequency = 12), facets = T)
attach(data1)
##Pruebas de raíz unitaria####

X=residSVAR_exo
qplot(FECHA[], X, geom = 'line')
summary(ur.df(X, lags=8, selectlags = "AIC", type = "trend")); interp_urdf(ur.df(X,type = 'trend', lags=8),level = "5pct")
summary(ur.df(X, lags=8, selectlags = "AIC", type = "drift")); interp_urdf(ur.df(X,type = 'drift', lags=8),level = "5pct")
summary(ur.df(X, lags=8, selectlags = "AIC", type = "none")); interp_urdf(ur.df(X,type = 'none', lags=8),level = "5pct")

grid.arrange(
  ggAcf(X,lag.max=60,plot=T,lwd=2,xlab='',main='ACF del {X}', ylim=c(-1,1)),
  ggPacf(X,lag.max=60,plot=T,lwd=2,xlab='',main='PACF del {X}', ylim=c(-1,1)),
  ggAcf(diff(X),lag.max=60,plot=T,lwd=2,xlab='',main='ACF del {X} diferenciado', ylim=c(-1,1)),
  ggPacf(diff(X),lag.max=60,plot=T,lwd=2,xlab='',main='PACF del {X} diferenciado', ylim=c(-1,1))
)

summary(ur.df(diff(X), lags=8, selectlags = "AIC", type = "trend")); interp_urdf(ur.df(diff(X),type = 'trend', lags=8),level = "5pct")
summary(ur.df(diff(X), lags=8, selectlags = "AIC", type = "drift")); interp_urdf(ur.df(diff(X),type = 'drift', lags=8),level = "5pct")
summary(ur.df(diff(X), lags=8, selectlags = "AIC", type = "none")); interp_urdf(ur.df(diff(X),type = 'none', lags=8),level = "5pct")

qplot(FECHA[-1], diff(X), geom = 'line')

plot(decompose(ts(X, start = c(2008,11), frequency = 12) ))
adsdas<-decompose(ts(X, frequency = 12))
library(seasonal)
plot(seas(ts(X, start = 1998, frequency = 12), x11 =''))
fadfda<-seas(ts(X, start = 1998, frequency = 12), x11 ='')
asdadfadfasd<-fadfda[["data"]]
X<-asdadfadfasd[,'seasonaladj']
autoplot(ts(asdadfadfasd[,'irregular'], start = 1998, frequency = 12))

#X<- ts(X, start = 1998, frequency = 12)
#lin.mod <- lm(X ~ time(X))
#lin.trend <- lin.mod$fitted.values
#linear <- ts(lin.trend, start = 1998, frequency = 12)
#lin.cycle <- X - linear
#autoplot(lin.cycle)
#X<-lin.cycle

#Shocks monetarios####
X<-TOTRES- NONBOR
lin.mod <- lm(X ~ time(X))
lin.trend <- lin.mod$fitted.values
linear <- ts(lin.trend, start = c(2008,11), frequency = 12)
lin.cycle <- X - linear
autoplot(lin.cycle)
X<-lin.cycle
u_ffr<-X
u_trr<-X
u_nbr<-X
u_brr<-X

v_d<-residuals(lm(u_trr~-1+u_ffr))
v_b<-residuals(lm(u_brr~-1+u_ffr))
v_s<-residuals(lm(u_nbr~-1+v_d+v_b))
autoplot(ts(v_s, start = 1998, frequency = 12))

U<-cbind(u_trr, u_nbr, u_ffr)

b_exo<-matrix(nrow = 3, ncol = 3, 
              rbind(c( 1 , NA , 0 ), 
                    c( 1 , 1 , -1 ),
                    c( 0 , NA , 0 )))
 
VARselect(U, type = 'none')
SVAR_exo1<-SVAR(VAR(U, p = 1, ic = 'BIC'), Bmat = b_exo, estmethod = 'scoring', max.iter = 4000, maxls = 1000)
summary(SVAR_exo1)
autoplot(ts(residuals(VAR(U, p = 1, ic = 'BIC'))[,'u_nbr'], start = c(2008,11), frequency = 12))
residSVAR_exo<-t(solve(SVAR_exo[["B"]])%*%t(U))[,'u_nbr']
autoplot(ts(residSVAR_exo,start = c(2008,11),frequency = 12))
plot(irf(SVAR_exo, ortho = T))

#R<-cbind(GDP,CPI_US,BRENT,VIX,TOTRES,NONBOR,SHADOW_RATE)
#diffR<-diff(R)
#cor(R)
#a_exo<-matrix(nrow = 7, ncol = 7, 
#           rbind(c( 1 , 0 , 0 , 0 , 0 , 0, 0), 
#                 c( 0 , 1 , 0 , 0 , 0 , 0, 0),
#                 c( 0 , 0 , 1 , 0 , 0 , 0, 0),###
#                 c( 0 , 0 , 0 , 1 , 0 , 0, 0),
#                 c( 0 , 0 , 0 , 0 , 1 , 0, 0),
#                 c( 0 , 0 , 0 , 0 , 0 , 1, 0),
#                 c( NA , NA , NA , NA , NA, NA, 1)
#           ))
#
#SVAR_exo2<-SVAR(VAR(R, p = 2, ic = 'BIC'), Amat = a_exo, Bmat = NULL, estmethod = 'scoring', max.iter = 1000, maxls = 10000)
#summary(SVAR_exo2)
#residSVAR_exo2<-SVAR_exo2[["var"]][["varresult"]][["SHADOW_RATE"]][["residuals"]]
#autoplot(ts(residSVAR_exo2, start = c(2008,11), frequency = 12))

shock_m<-residuals(lm(SHADOW_RATE~BRENT+VIX+GDP+CPI_US+TOTRES+NONBOR))
autoplot(ts(shock_m, start = c(2008,11), frequency = 12))

#Variables transformadas####
lBRENT<-log(BRENT)
lCAFE<-log(CAFE)
lIPI_US<-log(IPI_US)
lCPI_US<-log(CPI_US)
lIPI_COL<-log(IPI_COL)
lIPC_COL<-log(IPC_COL)
lITCR<-log(ITCR)
lEXPORTACIONES<-log(EXPORTACIONES)
lM3_COL<-log(M3_COL)

#Modelos####
Y<-cbind(lBRENT,lCAFE, SHADOW_RATE, lIPI_US, lCPI_US, BANREP_RATE, lIPI_COL, 
         lIPC_COL, lITCR, 'XN'=EXPORTACIONES-IMPORTACIONES)

Y1<-cbind(lBRENT, SHADOW_RATE, lIPI_US, lCPI_US, BANREP_RATE, lIPI_COL, 
         lIPC_COL, lITCR)

diffY<-diff(Y1)
Y<-cbind(residSVAR_exo, diffY)
VARselect(Y)
VAR_1<-VAR(Y, p = 1, ic = 'AIC', type = 'both')
summary(VAR_1)
plot(irf(VAR_1, boot = T, ci = 0.68))

a<-matrix(nrow = 9, ncol = 9, 
              rbind(c( 1 , 0 , 0, 0, 0, 0, 0, 0, 0), 
                    c( 0 , 1 , 0, 0, 0, 0, 0, 0, 0),
                    c( NA , 0 , 1, NA , 0 , 0, 0, 0, 0),###
                    c( NA , NA , NA , 1 , 0, 0, 0, 0, 0),
                    c( NA , 0 , NA , NA , 1, 0, 0, 0, 0),
                    c( NA , NA , NA, 0, 0, 1, 0, 0, 0),
                    c( NA , NA , NA, NA, NA, NA, 1, 0, 0),
                    c( NA , NA , NA, 0, 0, NA, NA, 1, NA),
                    c( NA , NA , NA, 0, NA, NA, NA, NA, 1)
                    ))

a1<-matrix(nrow = 9, ncol = 9, 
          rbind(c( 1 , 0 , 0, 0, 0, 0, 0, 0, 0), 
                c( NA , 1 , 0, 0, 0, 0, 0, 0, 0),
                c( NA , NA , 1, 0 , 0 , 0, 0, 0, 0),###
                c( NA , NA , NA , 1 , 0, 0, 0, 0, 0),
                c( NA , NA , NA , NA , 1, 0, 0, 0, 0),
                c( NA , NA , NA, NA, NA, 1, 0, 0, 0),
                c( NA , NA , NA, NA, NA, NA, 1, 0, 0),
                c( NA , NA , NA, NA, NA, NA, NA, 1, 0),
                c( NA , NA , NA, NA, NA, NA, NA, NA, 1)
          ))

a2<-matrix(nrow = 8, ncol = 8, 
           rbind(c( 1 , 0 , 0, 0, 0, 0, 0, 0), 
                 c( NA , 1 , 0, 0, 0, 0, 0, 0),
                 c( NA , NA , 1, 0 , 0 , 0, 0, 0),###
                 c( NA , NA , NA , 1 , 0, 0, 0, 0),
                 c( NA , NA , NA , NA , 1, 0, 0, 0),
                 c( NA , NA , NA, NA, NA, 1, 0, 0),
                 c( NA , NA , NA, NA, NA, NA, 1, 0),
                 c( NA , NA , NA, NA, NA, NA, NA, 1)
           ))

b1<-diag(nrow = 9, ncol = 9)

SVAR1<-SVAR(VAR_1, Amat = a1, Bmat = NULL, max.iter = 2000, maxls = 1000)
summary(SVAR1)

lM3_USA<-log(M3_USA)
Y2<-cbind(lBRENT, BANREP_RATE, lIPI_COL, 
          lIPC_COL, lITCR)
diffY2<-diff(Y2)
diffY2<-cbind('shock_m'= shock_m[-1], diff(Y2))
VARselect(diffY2, type = 'none', lag.max = 20)
VAR2<- VAR(diffY2, p = 1, type = 'none', ic = 'BIC')
summary(VAR2)

cor(diffY2)

a4<-matrix(nrow = 6, ncol = 6, 
           rbind(c( 1 , 0 , 0 , 0, 0, 0), 
                 c( NA , 1 , 0 , 0, 0, 0),
                 c( NA , 0 , 1 , 0 , 0 , 0),###
                 c( NA , 0 , 0 , 1 , 0, 0),
                 c( NA , 0 , 0 , 0 , 1, 0),
                 c( NA , 0 , 0 , 0 , 0 , 1)
           ))

b4<-diag(nrow = 6, ncol = 6)
for (i in 1:6) {
  for (j in 1:6) {
    ifelse(b4[i,j]==1, b4[i,j]<-NA, b4[i,j]<-0)}
}

SVAR2<-SVAR(VAR2, Amat = a4, Bmat = b4, estmethod = 'scoring', max.iter = 2000, maxls = 1000)
summary(SVAR2)
plot(irf(SVAR2, impulse = 'shock_m', ortho = T, cumulative = F, boot = T, n.ahead = 5, ci = .68, runs = 100))

autoplot(ts(data, start = 1998, frequency = 12))


Y3<-cbind(TOTRES, NONBOR, FEDERAL_RATE, BANREP_RATE, lIPI_COL, 
          lIPC_COL, lITCR)
diffY3<-diff(Y3)
diffY3<-cbind(residSVAR_exo, diff(Y3))
VARselect(diffY3, type = 'none', lag.max = 20)
VAR3<- VAR(diffY3, p = 1, type = 'none', ic = 'BIC')
summary(VAR3)

cor(diffY3)

a5<-matrix(nrow = 7, ncol = 7, 
           rbind(c( 1 , 0 , 0 , 0 , 0 , 0, 0), 
                 c( 0 , 1 , 0 , 0 , 0 , 0, 0),
                 c( 0 , 0 , 1 , 0 , 0 , 0, 0),###
                 c( 0 , NA , NA , 1 , 0 , 0, 0),
                 c( 0 , NA , NA , 0 , 1 , 0, 0),
                 c( 0 , NA , NA , 0 , 0 , 1, 0),
                 c( 0 , NA , NA , 0 , 0 , 0, 1)
           ))

b5<-matrix(nrow = 7, ncol = 7, 
           rbind(c( 1 , NA , 0 , 0 , 0 , 0, 0), 
                 c( 1 , 1 , -1 , 0 , 0 , 0, 0),
                 c( 0 , NA , 0 , 0 , 0 , 0, 0),###
                 c( 0 , 0 , 0 , NA , 0 , 0, 0),
                 c( 0 , 0 , 0 , 0 , NA , 0, 0),
                 c( 0 , 0 , 0 , 0 , 0 , NA, 0),
                 c( 0 , 0 , 0 , 0 , 0 , 0, NA)
           ))

SVAR3<-SVAR(VAR3, Amat = a5, Bmat = b5, estmethod = 'scoring', max.iter = 2000, maxls = 5000)
summary(SVAR3)
plot(irf(SVAR3, impulse = 'FEDERAL_RATE', ortho = T, cumulative = F, boot = T, n.ahead = 5, ci = .68, runs = 100))

#Salió algo ####
shock_m<-residuals(lm(SHADOW_RATE~IPI_US+CPI_US+lBRENT+u_trr+u_nbr))
autoplot(ts(shock_m, start = 1998, frequency = 12))

Y4<-cbind(u_nbr, BANREP_RATE, lIPI_COL, lIPC_COL)
diffY4<-diff(Y4)
diffY4<-cbind(residSVAR_exo, diff(Y4))
VARselect(diffY4, type = 'none', lag.max = 10)
VAR4<- VAR(diffY4, p = 5, type = 'none', ic = 'BIC')
summary(VAR4)

a6<-matrix(nrow = 4, ncol = 4, 
           rbind(c( 1 , 0 , 0 , 0), 
                 c( NA , 1 , 0 , 0),
                 c( NA , NA , 1 , 0),###
                 c( NA , NA , 0 , 1)
           ))

b6<-diag(nrow = 4, ncol = 4)
for (i in 1:4) {
  for (j in 1:4) {
    ifelse(b6[i,j]==1, b6[i,j]<-NA, b6[i,j]<-0)}
}

SVAR4<-SVAR(VAR4, Amat = a6, Bmat = b6, estmethod = 'scoring', max.iter = 2000, maxls = 1000)
summary(SVAR4)
plot(irf(SVAR4, impulse = c('BANREP_RATE','shock_m'), response = 'lIPC_COL', ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR4, impulse = c('BANREP_RATE','shock_m'), response = 'lIPI_COL', ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))

P.62=serial.test(VAR4, lags.pt = 75, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR4, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR4, lags.pt = 20, type = "PT.asymptotic");P.20 #No rechazo, se cumple el supuesto

plot(P.30) #Bien comportados, salvo por los residuales al cuadrado
plot(P.62)
plot(VAR4)
#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR4, lags.multi = 75) 
arch.test(VAR4, lags.multi = 50) #Rechazo, no se cumple el supuesto
arch.test(VAR4, lags.multi = 20) #Rechazo, no se cumple el supuesto

##Test Jarque-Bera multivariado
plot(normality.test(VAR4, multivariate.only = T)) #Rechazo, no se cumple el supuesto. 
#Estabilidad del VAR
roots(VAR4)
stability(VAR4); plot(stability(VAR4))
autoplot(ts(lIPC_COL, start = 1, frequency = 12))

asfgsfddhha<-cbind(lM3_USA, shock_m, v_s, SHADOW_RATE, residSVAR_exo, IPC_COL, IPI_COL, ITCR, lM3_COL, BANREP_RATE, lBRENT, lCAFE)
cor(asfgsfddhha)

Y5<-cbind(shock_m, BANREP_RATE, lIPI_COL, lIPC_COL, lITCR)
diffY5<-diff(Y5)
diffY5<-cbind(residSVAR_exo, diff(Y5))
VARselect(diffY5, type = 'none', lag.max = 10)
VAR5<- VAR(diffY5, p = 10, type = 'none', ic = 'BIC', exogen = d08[-1])
summary(VAR5)

a7<-matrix(nrow = 5, ncol = 5, 
           rbind(c( 1 , 0 , 0 , 0, 0), 
                 c( NA , 1 , 0 , 0, 0),
                 c( NA , NA , 1 , 0, 0),###
                 c( NA , NA , 0 , 1, 0),
                 c( NA , NA , 0 , NA, 1)
           ))

b7<-diag(nrow = 5, ncol = 5)
for (i in 1:5) {
  for (j in 1:5) {
    ifelse(b7[i,j]==1, b7[i,j]<-NA, b7[i,j]<-0)}
}

SVAR5<-SVAR(VAR5, Amat = a7, Bmat = b7, estmethod = 'scoring', max.iter = 2000, maxls = 1000)
summary(SVAR5)
plot(irf(SVAR5, impulse = c('BANREP_RATE','shock_m'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR5, impulse = c('BANREP_RATE','shock_m'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR5, impulse = c('BANREP_RATE','shock_m'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))

P.62=serial.test(VAR5, lags.pt = 75, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR5, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR5, lags.pt = 20, type = "PT.asymptotic");P.20 #No rechazo, se cumple el supuesto

plot(P.30) #Bien comportados, salvo por los residuales al cuadrado
plot(P.62)
plot(VAR5)
#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR5, lags.multi = 75) 
arch.test(VAR5, lags.multi = 50) #Rechazo, no se cumple el supuesto
arch.test(VAR5, lags.multi = 20) #Rechazo, no se cumple el supuesto

##Test Jarque-Bera multivariado
plot(normality.test(VAR5, multivariate.only = T)) #Rechazo, no se cumple el supuesto. 
#Estabilidad del VAR
roots(VAR5)
stability(VAR5); plot(stability(VAR5))

d08<-rep(1,264)
d08[1:129]<-0

Y6<-cbind(residSVAR_exo, lBRENT, lIPI_US, BANREP_RATE, lIPI_COL, lIPC_COL, lITCR, lM3_COL)
diffY6<-diff(Y6)
diffY6<-cbind(residSVAR_exo, diff(Y6))
VARselect(diffY6, type = 'none', lag.max = 10)
VAR6<- VAR(diffY6, p = 3, type = 'none', ic = 'BIC')
summary(VAR6)

a8<-matrix(nrow = 8, ncol = 8, 
           rbind(c( 1 , 0 , 0 , 0 , 0 , 0, 0, 0), 
                 c( 0 , 1 , 0 , 0 , 0 , 0, 0, 0),
                 c( NA , NA , 1 , 0 , 0 , 0, 0, 0), ###
                 c( 0 , 0 , 0 , 1 , NA , 0, 0, NA),
                 c( 0 , NA , NA , 0 , 1 , 0, 0, 0),
                 c( 0 , 0 , 0 , 0 , NA , 1, 0, 0), ###
                 c( NA , NA , NA , NA , NA , NA, 1, 0),
                 c( 0 , 0 , 0 , NA , NA , -1, 0, 1)
           ))

b8<-diag(nrow = 8, ncol = 8)
for (i in 1:8) {
  for (j in 1:8) {
    ifelse(b8[i,j]==1, b8[i,j]<-NA, b8[i,j]<-0)}
}

SVAR6<-SVAR(VAR6, Amat = a8, Bmat = b8, estmethod = 'scoring', max.iter = 999, maxls = 1000)
summary(SVAR6)
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lITCR'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lITCR'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lBRENT'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))

P.62=serial.test(VAR6, lags.pt = 75, type = "PT.asymptotic");P.62 #No rechazo, se cumple el supuesto
P.50=serial.test(VAR6, lags.pt = 50, type = "PT.asymptotic");P.50 #No rechazo, se cumple el supuesto
P.20=serial.test(VAR6, lags.pt = 20, type = "PT.asymptotic");P.20 #No rechazo, se cumple el supuesto

plot(P.20) #Bien comportados, salvo por los residuales al cuadrado
plot(P.62)
plot(VAR6)
#Homocedasticidad: Test tipo ARCH multivariado
arch.test(VAR6, lags.multi = 75) 
arch.test(VAR6, lags.multi = 50) #Rechazo, no se cumple el supuesto
arch.test(VAR6, lags.multi = 20) #Rechazo, no se cumple el supuesto

##Test Jarque-Bera multivariado
normality.test(VAR6, multivariate.only = T) #Rechazo, no se cumple el supuesto. 
#Estabilidad del VAR
roots(VAR6)
stability(VAR6); plot(stability(VAR6))

grid.arrange(ncol=3, 
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lIPI_COL'),
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lIPC_COL'),
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lITCR'),
             irf_ggplot(VAR = SVAR6, impulso = 'lIPI_US', respuesta = 'lIPI_COL'),
             irf_ggplot(VAR = SVAR6, impulso = 'lIPI_US', respuesta = 'lIPC_COL'),
             irf_ggplot(VAR = SVAR6, impulso = 'lIPI_US', respuesta = 'lITCR')
)
 stargazer::stargazer(SVAR6[["var"]][["varresult"]], type = 'text')
stargazer::stargazer(summary(VAR6))

print(xtable::xtable(jfhafsakdsajdka[["varresult"]]))

