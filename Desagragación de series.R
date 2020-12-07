install.packages('tempdisagg')
library(tseries)
library(tempdisagg)
IED<-data.frame(IED)

m1 <- td(IED$Inversiones.de.cartera..entrada.neta.de.capital..balanza.de.pagos..US..a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversiones.de.cartera..entrada.neta.de.capital..balanza.de.pagos..US..a.precios.actuales.)
plot(predict(m1))

s1<-ts(predict(m1), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s1)

m2 <- td(IED$Inversiones.de.cartera..bonos..PPG..PNG...NFL..US..a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversiones.de.cartera..bonos..PPG..PNG...NFL..US..a.precios.actuales.)
plot(predict(m2))

s2<-ts(predict(m2), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s2)

m3 <- td(IED$Inversiones.de.cartera..neta..balanza.de.pagos..US..a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversiones.de.cartera..neta..balanza.de.pagos..US..a.precios.actuales.)
plot(predict(m3))

s3 <-ts(predict(m3), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s3)

m4 <- td(IED$Inversión.extranjera.directa..entrada.neta.de.capital..balanza.de.pagos..US..a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversión.extranjera.directa..entrada.neta.de.capital..balanza.de.pagos..US..a.precios.actuales.)
plot(predict(m4))

s4<-ts(predict(m4), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s4)

m5 <- td(IED$Inversión.extranjera.directa..neta..balanza.de.pagos..US..a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversión.extranjera.directa..neta..balanza.de.pagos..US..a.precios.actuales.)
plot(predict(m5))

s5<-ts(predict(m5), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s5)

m6 <- td(IED$Inversión.extranjera.directa..salidas.netas..BDP..USD.a.precios.actuales. ~ 1, 
         to = 12, method = "denton-cholette")
plot(IED$Inversión.extranjera.directa..salidas.netas..BDP..USD.a.precios.actuales.)
plot(predict(m6))

s6<-ts(predict(m6), start = c(1994,1), frequency = 12)
ggplot2::autoplot(s6)


IED.da<-cbind(s1, s2, s3, s4, s5, s6)
IED.da<-ts(IED.da, start = c(1994,1), frequency = 12)
write.csv(IED.da, file = 'IED.da.csv')


GDP <- td(GDP$GDP ~ 1, to = 3, method = "denton-cholette")
plot(predict(GDP))
s7<-ts(predict(GDP), start = c(1998,1), frequency = 12)
length(s7)
View(s7)
write.csv(predict(GDP), file = 'GDP.csv')


PIB <- td(PIB$PIB ~ 1, to = 3, method = "denton-cholette")
plot(predict(PIB))
s8<-ts(predict(PIB), start = c(1998,1), frequency = 12)
length(s8)
View(s8)
write.csv(predict(PIB), file = 'PIB.csv')

PIB_2 <- td(PIB2$Colombia ~ 1, to = 12, method = "denton-cholette")
plot(predict(PIB_2))
s8<-ts(predict(PIB), start = c(1998,1), frequency = 12)
length(s8)
View(s8)
write.csv(predict(PIB_2), file = 'PIB_COMPLETO.csv')

