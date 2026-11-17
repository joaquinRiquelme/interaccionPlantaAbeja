calcular_baselga_focal <- function(plantas_R1, plantas_R2) {

  # Vectores de nombres de plantas utilizadas por la misma abeja
  plantas_R1 <- unique(plantas_R1)
  plantas_R2 <- unique(plantas_R2)

  # Excluir transiciones sin interacciones en alguna fecha
  if (length(plantas_R1) == 0 || length(plantas_R2) == 0) {
    return(data.frame(
      a = NA_integer_, b = NA_integer_, c = NA_integer_,
      BW = NA_real_, BSIM = NA_real_, BSNE = NA_real_
    ))
  }

  a <- length(intersect(plantas_R1, plantas_R2))
  b <- length(setdiff(plantas_R1, plantas_R2))
  c <- length(setdiff(plantas_R2, plantas_R1))

  # Whittaker / Sorensen binario
  BW <- (b + c) / (2 * a + b + c)

  # Baselga: componente de reemplazo
  BSIM <- min(b, c) / (a + min(b, c))

  # Baselga: componente de anidamiento resultante
  BSNE <- BW - BSIM

  data.frame(a = a, b = b, c = c,
             BW = BW, BSIM = BSIM, BSNE = BSNE)
}

# Ejemplo: abeja que utiliza P1, P2 y P3 en R1;
# P1 y P4 en R2
resultado <- calcular_baselga_focal(
  plantas_R1 = c("P1", "P2", "P3"),
  plantas_R2 = c("P1", "P4")
)
print(resultado)
# Esperado: a=1, b=2, c=1, BW=0.6, BSIM=0.5, BSNE=0.1
