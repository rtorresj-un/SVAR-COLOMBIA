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
  IRF = irf(VAR, impulse=impulso ,response=respuesta,n.ahead = 30,ci=0.68, boot=T, ortho=T, runs=200)  
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
X<-TOTRES - NONBOR
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
residSVAR_exo<-t(solve(SVAR_exo1[["B"]])%*%t(U))[,'u_nbr']
ggplot(data1,aes(FECHA,residSVAR_exo)) + 
  geom_line(aes(y= residSVAR_exo)) +
  geom_ribbon(aes(ymin = 0, ymax = residSVAR_exo), alpha = .3) + 
  theme_minimal() + xlab('') + ylab('') + 
  theme(legend.title = element_blank(), legend.position="bottom")

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
modshock_m<-lm(SHADOW_RATE~BRENT+VIX+GDP+CPI_US+TOTRES+NONBOR)
shock_m<-residuals(modshock_m)
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

# Modelos finales ####

Y6<-cbind(residSVAR_exo, lBRENT, lIPI_US, BANREP_RATE, lIPI_COL, lIPC_COL, lITCR, lM3_COL)
diffY6<-diff(Y6)
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
                 c( NA , NA , NA , NA , NA , NA, 1, NA),
                 c( 0 , 0 , 0 , NA , NA , -1, 0, 1)
           ))

b8<-diag(nrow = 8, ncol = 8)
for (i in 1:8) {
  for (j in 1:8) {
    ifelse(b8[i,j]==1, b8[i,j]<-NA, b8[i,j]<-0)}
}

SVAR6<-SVAR(VAR6, Amat = a8, Bmat = b8, estmethod = 'scoring', max.iter = 1000, maxls = 1000)
summary(SVAR6)
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('BANREP_RATE'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lITCR'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lITCR'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('BANREP_RATE'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('residSVAR_exo'), response = c('lM3_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lBRENT'), response = c('lITCR'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lM3_COL'), response = c('lIPI_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))
plot(irf(SVAR6, impulse = c('lM3_COL'), response = c('lIPC_COL'), ortho = T, cumulative = F, boot = T, n.ahead = 10, ci = .68, runs = 100))

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

             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lIPI_COL')
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lIPC_COL')
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'BANREP_RATE')
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lITCR')
             irf_ggplot(VAR = SVAR6, impulso = 'residSVAR_exo', respuesta = 'lM3_COL')
             irf_ggplot(VAR = SVAR6, impulso = 'BANREP_RATE', respuesta = 'lIPI_COL')
             irf_ggplot(VAR = SVAR6, impulso = 'BANREP_RATE', respuesta = 'lIPC_COL')
             irf_ggplot(VAR = SVAR6, impulso = 'BANREP_RATE', respuesta = 'lM3_COL')
             
             
Y7<-cbind(shock_m, lBRENT, lIPI_US, BANREP_RATE, lIPI_COL, lIPC_COL, lITCR, lM3_COL)
diffY7<-diff(Y7)
VARselect(diffY7, type = 'none', lag.max = 10)
VAR6<- VAR(diffY7, p = 3, type = 'none', ic = 'BIC')
summary(VAR7)
             
SVAR7<-SVAR(VAR7, Amat = a8, Bmat = b8, estmethod = 'scoring', max.iter = 1000, maxls = 1000)
summary(SVAR7)
             
             
 stargazer::stargazer(SVAR6[["var"]][["varresult"]], 
                      type = 'latex', 
                      title            = "Estimación del modelo de choques monetarios para Colombia con metodología \textit{agnostic} SVAR-SOE.",
                      covariate.labels = c("s_m_{t-1}", "log(r)_{t-1}", "Shock m_{t-2}", "log(r)_{t-2}",'Shock m_{t-3}', "log(r)_{t-3}"),
                      omit = c('lBRENT', 'lIPI_US', 'lIPI_COL', 'lIPC_COL', 'lITCR', 'lM3_COL'),
                      dep.var.caption  = "Variables dependientes:",
                      dep.var.labels   = "y_t asdf y_t^*",
                      keep.stat=c("rsq", 'f', 'll', 'bic', 'sigma2'),
                      column.labels = c("s_m", "log(BRENT)", "log(y^*)", 'r', 'log(y)', 'log(p)', 'log(ITCR)', 'log(M3)'))

 stargazer::stargazer(SVAR7[["var"]][["varresult"]], 
                      type = 'latex', 
                      title            = "Estimación del modelo de choques monetarios para Colombia con metodología \textit{agnostic} SVAR-SOE.",
                      covariate.labels = c("s_m_{t-1}", "log(r)_{t-1}", "Shock m_{t-2}", "log(r)_{t-2}",'Shock m_{t-3}', "log(r)_{t-3}"),
                      omit = c('lBRENT', 'lIPI_US', 'lIPI_COL', 'lIPC_COL', 'lITCR', 'lM3_COL'),
                      dep.var.caption  = "Variables dependientes:",
                      dep.var.labels   = "y_t asdf y_t^*",
                      keep.stat=c("rsq", 'f', 'll', 'bic', 'sigma2'),
                      column.labels = c("s_m", "log(BRENT)", "log(y^*)", 'r', 'log(y)', 'log(p)', 'log(ITCR)', 'log(M3)'))
 
 
 stargazer::stargazer(SVAR_exo1[["var"]][["varresult"]], type = 'latex',
                      dep.var.labels   = "U",
                      keep.stat=c("rsq", 'f', 'll', 'bic', 'sigma2'),
                      covariate.labels = c('u_{ffr}', 'u_{nbr}', 'u_{ttr}'),
                      column.labels = c('u_{ffr}', 'u_{nbr}', 'u_{ttr}')
                      )
 
 stargazer::stargazer(modshock_m, type = 'latex',
                      dep.var.labels   = "Wu & Xia interest rate",
                      covariate.labels = c('BRENT', 'VIX', 'y^*', 'p^*', 'RES_{tt}', 'RES_{nb}'), 
                      omit.stat='adj.rsq'
                      )
 
 
 
 
print(xtable::xtable(jfhafsakdsajdka[["varresult"]]))