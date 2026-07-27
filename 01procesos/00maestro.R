# Codigo maestro de flujo de trabajo IPA
# ubicacion  "../GitHub/interaccionPlantaAbeja/01procesos/00maestro.R"

# Definición de directorios de trabajo

dir.base <- paste(getwd(),"/..", sep="")
dir.datos <- paste(dir.base,"00baseDatos", sep="/")
dir.procesos <- paste(dir.base,"01procesos", sep="/")
dir.salidas <- paste(dir.base,"02salidas", sep="/")
dir.figuras <- paste(dir.base,"03figuras", sep="/")


### Definicion de variables de latitud

variable.latitud <- "Latitud"#numerica
n.categorias.latitud <- 2

if(n.categorias.latitud==2){
  niveles.latitud <- c(1,2)
  valores.latitud <- c("Baja","Media")
}


### Definicion de variables grado de generalizacion trofica ----

variable.grado <- "Grado"
# variable.grado <- "Grado_z" # logica de distruciones
# variable.grado <- "Grado_log"
# variable.grado <- "Grado_familia"
# variable.grado <- "Diversidad_familia"

# n.categorias.grado <- 1
n.categorias.grado <- 2

if(n.categorias.grado==2){
  niveles.grado <- c(1,2)
  valores.grado <- c("Especialista","Generalista")
}

if(n.categorias.grado==3){
  niveles.grado <- c(1,2,3)
  valores.grado <- c("Especialista","Medio","Generalista")
  }

if(n.categorias.grado==4){
  niveles.grado <- seq(from=1, to=n.categorias.grado)
  valores.grado <- c("Hiper especialista","Especialista","Generalista","Hiper generalista")
}

if(n.categorias.grado>4){
  niveles.grado <- seq(from=1, to=n.categorias.grado)
  valores.grado <- c("Especialista",rep("",n.categorias.grado-2),"Generalista")
}


### Definicion de variables de tamaño (DIT) ----
variable.dit <- "DIT"
# variable.dit <- "DIT_z"
# variable.dit <- "DIT_log"
n.categorias.dit <- 1

if(n.categorias.dit==3){
  niveles.dit <- c(1,2,3)
  valores.dit <- c("Pequeña","Mediana","Grande")
}

# directorio para compilaciones

directorio.comp <- file.path(dir.salidas, 
                             paste(variable.latitud, n.categorias.latitud,
                                   "_",variable.grado, n.categorias.grado,
                                   "_",variable.dit, n.categorias.dit, sep=""))


if (!file.exists(directorio.comp)) {
  dir.create(directorio.comp)
  message("Directorio creado exitosamente.")
} else {
  message("El directorio ya existe.")
}

# Correr codigos ----

## 01 Mapa de ubicacion de datos ----
source("01mapaUbicacion.R")
# detalles de que matrices estamos trabajando

## 02 Armonizacion de nombres de plantas ----
source("02taxonomiaPlantas.R")

## 03 Armonizacion de nombres de especies de abejas ----
source("03armonizacionEspecies.R")

## 04 Filtros por grupo ----
source("04filtroAbejas.R")

## 05 Calculo de grado de generalizacion trofica ----
source("05calculoGrado.R")

## 06 Combinacion de bases de datos ----
source("06unionBaseDatos.R")

## 07 asignacion de categorias ----
source("07asignacionCategorias.R")

## 08 Estadistica descriptiva de variables ----
source("08familias.R")

## 09 Analisis de datos Grado vs DIT vs Latitud ----
source("09analisisDITGrado.R")


