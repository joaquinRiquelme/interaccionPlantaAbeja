library(tidyverse)

base_datos <- readRDS("F:/Github/interaccionPlantaAbeja/00baseDatos/base_datos.RDS")


ml <- base_datos$matriz_comp_larga
especie <- base_datos$especies
DIT <- base_datos$DIT


head(ml)

m2 <- merge(ml, especie)

Grado <- ml |> group_by(ID, especie) |>
  mutate(Grado=sum(interaccion!=0))
Grado

Z_score <- Grado |> group_by(ID) |>
  mutate(Z_Grado=scale(Grado))


GradoA <- subset(m2, orden=="Hymenoptera")|> group_by(ID, especie) |>
  mutate(Grado=sum(interaccion!=0))
GradoA

Z_scoreA <- GradoA |> group_by(ID) |>
  mutate(Z_Grado=scale(Grado))


DIT$Abeja <- gsub(pattern = "_",replacement = ".",x = DIT$Abeja)
rasgos2 <- merge(Z_scoreA, DIT, by.x=c("ID","especie"), by.y=c("ID","especies"))
head(rasgos2)

m1 <- lm(Z_Grado~sqrt(DIT), rasgos2)
plot(Z_Grado~sqrt(DIT), rasgos2)
abline(m1, col="red")
summary(m1)


cuadratica <- function(x,b0,b1,b2){
  y <- b0+b1*x+b2*x^2
  y
}


plot(1:100,cuadratica(1:100,100,100,-1))

data1 <- data.frame(x=1:100,
                    y=cuadratica(1:100,100,100,-1)+rnorm(100,100,1000))

plot(data1)

m2 <- lm(y ~ x,
          data=data1)

m1 <- nls(y ~ cuadratica(x,b0,b1,b2),
    data=data1,
    start = c(b0=100,b1=100,b2=-1))
m1
lines(x=1:100, y=fitted(m1)-200, col="blue")
lines(x=1:100, y=fitted(m1)+300, col="red")
lines(x=1:100, y=fitted(m2))

summary(m1)
summary(m2)

residuals(m1)
residuals(m2)

histogram(~sqrt(rasgos2$Z_Grado))
shapiro.test(log(rasgos2$Z_Grado))

library(tseries)
a <- sqrt(rasgos2$Z_Grado+.00001)
a <- a[!is.na(a)]
jarque.bera.test(a)

nlme(height ~ SSasymp(age, Asym, R0, lrc),
     data = Loblolly,
     fixed = Asym + R0 + lrc ~ 1,
     random = Asym ~ 1,
     start = c(Asym = 103, R0 = -8.5, lrc = -3.3))




