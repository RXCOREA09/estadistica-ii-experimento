set.seed(123)  # Cambia la semilla si quieres otra aleatorización

n_total <- 55

# ---- Variables categóricas (balance por bloques) ----
cat_factores <- c("Iluminacion", "PosPiezas", "Guantes", "PosCorporal")

n_bloques <- ceiling(n_total / 4)
tam_bloques <- c(rep(4, n_bloques - 1), n_total - 4 * (n_bloques - 1))

gen_bloque <- function(k) {
  unos <- floor(k / 2)
  ceros <- k - unos
  sample(c(rep(0, ceros), rep(1, unos)))
}

matriz <- data.frame(Corrida = 1:n_total)
for (f in cat_factores) {
  bloques <- lapply(tam_bloques, gen_bloque)
  matriz[[f]] <- unlist(bloques)
}

# ---- Variables cuantitativas (asignación aleatoria) ----
matriz$NumPiezas <- sample(4:8, n_total, replace = TRUE)   # enteros de 4 a 8
matriz$Distancia <- sample(10:60, n_total, replace = TRUE) # enteros de 10 a 60 cm
matriz$Ruido     <- sample(40:80, n_total, replace = TRUE) # enteros de 40 a 80%

# ---- Resultado ----
print(matriz)

# Exportar a Excel (.xlsx)
library(writexl)
write_xlsx(matriz, "plan_55_corridas.xlsx")

# Alternativa: exportar a CSV
# write.csv(matriz, "plan_55_corridas.csv", row.names = FALSE)

