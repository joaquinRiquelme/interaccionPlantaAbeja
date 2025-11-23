library(tidyverse)

base_datos <- readRDS("../00baseDatos/base_datos.RDS")

ml <- base_datos$matriz_comp_larga
ml <- subset(ml, interaccion > 0)
especie <- base_datos$especies

# Grado 
Grado <- ml |> group_by(ID, especie) |>
  mutate(Grado=sum(interaccion>0)) |>
  select(c(-plantas,-interaccion)) |> 
  unique()

# Calculo de grado relativo y zscore
Grados <- Grado |> group_by(ID) |>
  mutate(Grado_max=max(Grado)) |>
  mutate(Grado_relativo=Grado/Grado_max) |>
  mutate(Z_Grado=scale(Grado))

write.csv(x = Grados, file = "../02salidas/Grados.csv", row.names = FALSE)

# histograma de grado
for(i in unique(Grados$ID)){
  print(i)
  Grado_i <- subset(Grados, ID==i)
  
  png(filename = paste0("../03figuras/Grado/histograma_grado_",i,".png"))
  hist(Grado_i$Grado, main=paste("Histograma grado absoluto \n red",i),
       ylab="Frecuencia",
       xlab="Grado", las=1)
  dev.off()
  
  png(filename = paste0("../03figuras/Grado/histograma_gradoRelativo_",i,".png"))
  hist(Grado_i$Grado_relativo, main=paste("Histograma grado relativo \n red",i),
       ylab="Frecuencia",
       xlab="Grado relativo", las=1)
  dev.off()
  
  png(filename = paste0("../03figuras/Grado/histograma_Z_score_",i,".png"))
  hist(Grado_i$Z_Grado, main=paste("Histograma z score \n red",i),
       ylab="Frecuencia",
       xlab="Z score", las=1)
  dev.off()
}






  interaccion_i = subset(interaccion, ID==i)
  matrix_i = pivot_wider(interaccion_i, 
                         id_cols = c(ID, plantas),
                         names_from = especie,
                         values_from = interaccion, values_fn = unique)

d  BD=nx.Graph()
  nodos_A=label_plant
  BD.add_nodes_from(nodos_A,bipartite=0)
  nodos_B=label_spe
  BD.add_nodes_from(nodos_B,bipartite=1)
  
  for k in range(len(list_df)):
    if label_edge[k] == 1: 
    BD.add_edge(label_plant[k],label_spe[k])
}
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




