# ============================================================
# ESTADISTICA II
# ANALISIS ESTADISTICO DEL EXPERIMENTO DE ENSAMBLAJE
# 55 CORRIDAS EXPERIMENTALES
# ============================================================

# Seleccione el archivo Excel original cuando se solicite.
# Los archivos de salida se guardan en el directorio de trabajo de R.

# 1. CARGA Y VERIFICACION DE PAQUETES
paquetes <- c(
  "readxl", "dplyr", "ggplot2", "tidyr", "car",
  "lmtest", "sandwich", "psych", "corrplot", "openxlsx"
)

paquetes_faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(lmtest)
library(sandwich)
library(psych)
library(corrplot)
library(openxlsx)

# 2. IMPORTACION Y PREPARACION DE LOS DATOS
datos <- read_excel(file.choose())
datos <- as.data.frame(datos)

# Renombrar la variable de respuesta
names(datos)[names(datos) == "Tiempo Promedio"] <- "Tiempo"

# Eliminar columnas completamente vacias
datos <- datos[, colSums(!is.na(datos)) > 0, drop = FALSE]

cat("\nESTRUCTURA DE LA BASE DE DATOS\n")
str(datos)
cat("\nNOMBRES DE LAS VARIABLES\n")
print(names(datos))
cat("\nPRIMERAS FILAS\n")
print(head(datos))
cat("\nVALORES FALTANTES POR VARIABLE\n")
print(colSums(is.na(datos)))

variables_requeridas <- c(
  "Corrida", "PosCorporal", "Distancia", "NumPiezas",
  "PosPiezas", "Guantes", "Iluminacion", "Ruido", "Tiempo"
)
faltan_variables <- setdiff(variables_requeridas, names(datos))
if (length(faltan_variables) > 0) {
  stop(paste(
    "Faltan variables en el archivo seleccionado:",
    paste(faltan_variables, collapse = ", ")
  ))
}
if (nrow(datos) != 55) {
  warning("La base seleccionada no contiene 55 observaciones.")
}

# 3. ESTADISTICA DESCRIPTIVA
cat("\nRESUMEN GENERAL DE LAS VARIABLES\n")
print(summary(datos))

estadisticos_tiempo <- data.frame(
  Estadistico = c(
    "Media", "Mediana", "Desviacion estandar", "Minimo",
    "Maximo", "Primer cuartil", "Tercer cuartil"
  ),
  Valor = c(
    mean(datos$Tiempo, na.rm = TRUE),
    median(datos$Tiempo, na.rm = TRUE),
    sd(datos$Tiempo, na.rm = TRUE),
    min(datos$Tiempo, na.rm = TRUE),
    max(datos$Tiempo, na.rm = TRUE),
    quantile(datos$Tiempo, 0.25, na.rm = TRUE),
    quantile(datos$Tiempo, 0.75, na.rm = TRUE)
  )
)
cat("\nESTADISTICOS DESCRIPTIVOS DE TIEMPO\n")
print(estadisticos_tiempo)

descripcion <- psych::describe(datos)
cat("\nDESCRIPCION DE LAS VARIABLES\n")
print(descripcion)

# 4. ANALISIS EXPLORATORIO Y GRAFICOS
grafico1 <- ggplot(datos, aes(x = Tiempo)) +
  geom_histogram(bins = 10, color = "black") +
  labs(
    title = "Distribucion del tiempo de ensamblaje",
    x = "Tiempo (segundos)", y = "Frecuencia"
  ) +
  theme_minimal()
print(grafico1)
ggsave("Grafico1_Histograma.png", grafico1, width = 8, height = 5)

grafico2 <- ggplot(datos, aes(x = Distancia, y = Tiempo)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Distancia y tiempo de ensamblaje",
    x = "Distancia (cm)", y = "Tiempo (segundos)"
  ) +
  theme_minimal()
print(grafico2)
ggsave("Grafico2_Distancia_Tiempo.png", grafico2, width = 8, height = 5)

grafico3 <- ggplot(datos, aes(x = NumPiezas, y = Tiempo)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Numero de piezas y tiempo de ensamblaje",
    x = "Numero de piezas", y = "Tiempo (segundos)"
  ) +
  theme_minimal()
print(grafico3)
ggsave("Grafico3_Piezas_Tiempo.png", grafico3, width = 8, height = 5)

grafico4 <- ggplot(datos, aes(x = factor(Guantes), y = Tiempo)) +
  geom_boxplot() +
  labs(
    title = "Tiempo de ensamblaje segun uso de guantes",
    x = "Guantes (0 = No, 1 = Si)", y = "Tiempo (segundos)"
  ) +
  theme_minimal()
print(grafico4)
ggsave("Grafico4_Guantes_Tiempo.png", grafico4, width = 8, height = 5)

# 5. MATRIZ DE CORRELACIONES DE PEARSON
variables_cor <- datos %>%
  select(Distancia, NumPiezas, Ruido, Tiempo)
matriz_cor <- cor(
  variables_cor, method = "pearson", use = "complete.obs"
)
cat("\nMATRIZ DE CORRELACIONES DE PEARSON\n")
print(matriz_cor)

png("Grafico9_Matriz_Correlaciones.png", width = 1000, height = 800)
corrplot::corrplot(
  matriz_cor, method = "color", addCoef.col = "black",
  tl.col = "black", number.cex = 0.8
)
dev.off()

# 6. MODELO DE REGRESION LINEAL COMPLETO
modelo_completo <- lm(
  Tiempo ~ PosCorporal + Distancia + NumPiezas +
    PosPiezas + Guantes + Iluminacion + Ruido,
  data = datos
)
cat("\nRESUMEN DEL MODELO COMPLETO\n")
print(summary(modelo_completo))

# 7. MODELO DE REGRESION LINEAL REDUCIDO
modelo_reducido <- lm(
  Tiempo ~ Distancia + NumPiezas + Guantes,
  data = datos
)
resumen_reducido <- summary(modelo_reducido)
cat("\nRESUMEN DEL MODELO REDUCIDO\n")
print(resumen_reducido)

# 8. COMPARACION DE MODELOS
comparacion_modelos <- anova(modelo_reducido, modelo_completo)
cat("\nCOMPARACION ANOVA DE LOS MODELOS ANIDADOS\n")
print(comparacion_modelos)

aic_modelos <- data.frame(
  Modelo = c("Completo", "Reducido"),
  AIC = c(AIC(modelo_completo), AIC(modelo_reducido))
)
cat("\nCRITERIOS AIC\n")
print(aic_modelos)

# 9. DIAGNOSTICO DE LOS SUPUESTOS
prueba_shapiro <- shapiro.test(residuals(modelo_reducido))
cat("\nPRUEBA DE NORMALIDAD DE SHAPIRO-WILK\n")
print(prueba_shapiro)

prueba_bp <- lmtest::bptest(modelo_reducido)
cat("\nPRUEBA DE HOMOCEDASTICIDAD DE BREUSCH-PAGAN\n")
print(prueba_bp)

prueba_dw <- lmtest::dwtest(modelo_reducido)
cat("\nPRUEBA DE AUTOCORRELACION DE DURBIN-WATSON\n")
print(prueba_dw)

valores_vif <- car::vif(modelo_reducido)
cat("\nFACTORES DE INFLACION DE VARIANZA (VIF)\n")
print(valores_vif)

# 10. GRAFICO DE RESIDUOS POR CORRIDA
residuos <- data.frame(
  Corrida = datos$Corrida,
  Residuos = residuals(modelo_reducido)
)
grafico_residuos <- ggplot(residuos, aes(x = Corrida, y = Residuos)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Residuos del modelo por corrida experimental",
    x = "Corrida", y = "Residuos"
  ) +
  theme_minimal()
print(grafico_residuos)
ggsave(
  "Grafico11_Residuos_Corridas.png",
  grafico_residuos, width = 8, height = 5
)

# 11. ERRORES ESTANDAR ROBUSTOS HC3
resultados_HC3 <- lmtest::coeftest(
  modelo_reducido,
  vcov. = sandwich::vcovHC(modelo_reducido, type = "HC3")
)
cat("\nRESULTADOS CON ERRORES ESTANDAR ROBUSTOS HC3\n")
print(resultados_HC3)

# 12. ERRORES ESTANDAR ROBUSTOS NEWEY-WEST
# Se conserva la configuracion predeterminada usada en el analisis.
resultados_NeweyWest <- lmtest::coeftest(
  modelo_reducido,
  vcov. = sandwich::NeweyWest(modelo_reducido)
)
cat("\nRESULTADOS CON ERRORES ESTANDAR NEWEY-WEST\n")
print(resultados_NeweyWest)

# 13. ANALISIS DE VARIANZA: SUMAS DE CUADRADOS TIPO I
anova_tipo_I <- anova(modelo_reducido)
cat("\nANOVA DEL MODELO REDUCIDO - TIPO I SECUENCIAL\n")
print(anova_tipo_I)

# 14. PREDICCIONES DEL MODELO REDUCIDO
nuevos_datos <- data.frame(
  Distancia = c(20, 20, 40, 40),
  NumPiezas = c(5, 5, 8, 8),
  Guantes = c(0, 1, 0, 1)
)
predicciones <- predict(modelo_reducido, newdata = nuevos_datos)
tabla_predicciones <- cbind(
  nuevos_datos,
  Tiempo_estimado = as.numeric(predicciones)
)
cat("\nPREDICCIONES DEL MODELO REDUCIDO\n")
print(tabla_predicciones)

# 15. EXPORTACION DE RESULTADOS
tabla_HC3 <- as.data.frame(resultados_HC3)
tabla_NeweyWest <- as.data.frame(resultados_NeweyWest)
tabla_coeficientes <- as.data.frame(resumen_reducido$coefficients)

tabla_ajuste <- data.frame(
  R_cuadrado = resumen_reducido$r.squared,
  R_cuadrado_ajustado = resumen_reducido$adj.r.squared,
  Error_estandar_residual = resumen_reducido$sigma,
  Estadistico_F = unname(resumen_reducido$fstatistic[1]),
  gl_numerador = unname(resumen_reducido$fstatistic[2]),
  gl_denominador = unname(resumen_reducido$fstatistic[3]),
  Valor_p_F = pf(
    resumen_reducido$fstatistic[1],
    resumen_reducido$fstatistic[2],
    resumen_reducido$fstatistic[3],
    lower.tail = FALSE
  )
)

openxlsx::write.xlsx(
  list(
    Datos_limpios = datos,
    Estadisticos_Tiempo = estadisticos_tiempo,
    Descripcion = descripcion,
    Correlaciones = as.data.frame(matriz_cor),
    Coeficientes_reducido = tabla_coeficientes,
    Ajuste_modelo = tabla_ajuste,
    Comparacion_modelos = as.data.frame(comparacion_modelos),
    AIC = aic_modelos,
    HC3 = tabla_HC3,
    Newey_West = tabla_NeweyWest,
    ANOVA_Tipo_I = as.data.frame(anova_tipo_I),
    Predicciones = tabla_predicciones
  ),
  file = "Resultados_analisis_estadistico.xlsx",
  rowNames = TRUE
)

write.csv(matriz_cor, "Matriz_correlaciones.csv", row.names = TRUE)
write.csv(tabla_predicciones, "Tabla_predicciones.csv", row.names = FALSE)
write.csv(datos, "Base_datos_limpia.csv", row.names = FALSE)

cat("\nANALISIS ESTADISTICO FINALIZADO.\n")
cat("Se genero Resultados_analisis_estadistico.xlsx\n")
