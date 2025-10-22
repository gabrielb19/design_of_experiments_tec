
Data <- read.csv("path/a/archivo/de/datos/limpios.csv")
Data$Objetos <- as.factor(Data$Objetos)
Data$Arquitectura <- as.factor(Data$Arquitectura)
Data$Efectos <- as.factor(Data$Efectos)
Data$Resolucion <- as.factor(Data$Resolucion)

# ==============================================================
# CANTIDAD DE REPETICIONES POR EXPERIMENTO
# ==============================================================
# Se cuenta la cantidad de veces que aparece cada combinación de factores y se nota que se repite el 5.

Data %>%
  select(-Runtime) %>%                              
  group_by(across(everything())) %>%                
  summarise(Frecuencia = n(), .groups = "drop") %>% 
  arrange(desc(Frecuencia))                         

# Se verifica que todos se repiten 5 veces.
Data %>%
  select(-Runtime) %>%                             
  group_by(across(everything())) %>%               
  summarise(Frecuencia = n(), .groups = "drop") %>%
  summarise(ValoresUnicos = n_distinct(Frecuencia))

# ==============================================================
# ANÁLISIS CON DATOS SIN PROCESAR
# ==============================================================

model = lm(Runtime ~ Objetos * Arquitectura * Efectos * Resolucion,
           data = Data)

x = residuals(model)

library(rcompanion)
plotNormalHistogram(x)

library(car)
leveneTest(Runtime ~ Objetos * Arquitectura * Efectos * Resolucion,
           data = Data)

plot(model)

# ==============================================================
# TRANSFORMACIÓN SQRT
# ==============================================================

Data$T_sqrt <- sqrt(Data$Runtime)

model <- lm(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion,
            data = Data)

x = residuals(model)

library(rcompanion)
plotNormalHistogram(x)

plot(fitted(model), residuals(model))

library(car)
leveneTest(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion,
           data = Data)

plot(model)

# ==============================================================
# EJECUCIÓN DE ANOVA CON DATOS TRANSFORMADOS
# ==============================================================

library(car)
Anova(model, type = "II")

# ==============================================================
# PAIRED T-TEST: COMPARAR ARQUITECTURAS DENTRO DE CADA RESOLUCIÓN
#    (Ejemplo: CPU:1920x1080 vs GPU:1920x1080)
# ==============================================================

resultados_arquitectura_resolucion <- list()

for (res in levels(Data$Resolucion)) {
  cat("\n--- Resolución:", res, "---\n")
  subset_res <- filter(Data, Resolucion == res)
  
  test_res <- pairwise.t.test(
    subset_res$T_sqrt,
    subset_res$Arquitectura,
    p.adjust.method = "holm"
  )
  
  print(test_res)
  resultados_arquitectura_resolucion[[res]] <- test_res
}

# ==============================================================
# PAIRED T-TEST: COMPARAR ARQUITECTURAS DENTRO DE CADA CANTIDAD DE OBJETOS
#    (Ejemplo: APU:16000objetos vs CPU:16000objetos)
# ==============================================================

resultados_arquitectura_objetos <- list()

for (obj in levels(Data$Objetos)) {
  cat("\n--- Objetos:", obj, "---\n")
  subset_obj <- filter(Data, Objetos == obj)
  
  test_obj <- pairwise.t.test(
    subset_obj$T_sqrt,
    subset_obj$Arquitectura,
    p.adjust.method = "holm"
  )
  
  print(test_obj)
  resultados_arquitectura_objetos[[obj]] <- test_obj
}

# ==============================================================
# PAIRED T-TEST: COMPARAR RESOLUCIONES DENTRO DE CADA ARQUITECTURA
#    (Ejemplo: CPU:720p vs CPU:1920x1080)
# ==============================================================

resultados_resolucion_por_arquitectura <- list()

for (arc in levels(Data$Arquitectura)) {
  cat("\n--- Arquitectura:", arc, "---\n")
  subset_arc <- filter(Data, Arquitectura == arc)
  
  test_arc <- pairwise.t.test(
    subset_arc$T_sqrt,
    subset_arc$Resolucion,
    p.adjust.method = "holm"
  )
  
  print(test_arc)
  resultados_resolucion_por_arquitectura[[arc]] <- test_arc
}

# ==============================================================
# PAIRED T-TEST: COMPARAR EFECTOS DENTRO DE CADA ARQUITECTURA (opcional)
# ==============================================================

resultados_efectos_por_arquitectura <- list()

for (arc in levels(Data$Arquitectura)) {
  cat("\n--- Arquitectura:", arc, "---\n")
  subset_arc <- filter(Data, Arquitectura == arc)
  
  test_arc <- pairwise.t.test(
    subset_arc$T_sqrt,
    subset_arc$Efectos,
    p.adjust.method = "holm"
  )
  
  print(test_arc)
  resultados_efectos_por_arquitectura[[arc]] <- test_arc
}


# ==============================================================
# GRÁFICO DE MEDIAS Y BIGOTES (Arquitectura + Resolución)
# ==============================================================

library(FSA)       # Para calcular medias, sd y n
library(ggplot2)   # Para graficar

Sum <- Summarize(T_sqrt ~ Arquitectura + Resolucion, data = Data, digits = 3)

Sum$se <- Sum$sd / sqrt(Sum$n)

Sum$mean_original <- Sum$mean^2
Sum$se_original   <- (Sum$se)^2


pd <- position_dodge(0.2)  # separación horizontal entre grupos

ggplot(Sum, aes(x = Resolucion, y = mean_original, color = Arquitectura)) +
  geom_errorbar(aes(ymin = mean_original - se_original,
                    ymax = mean_original + se_original),
                width = 0.2, size = 0.8, position = pd) +
  geom_point(shape = 15, size = 4, position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold")) +
  ylab("Tiempo medio de sintetizado (s)") +
  xlab("Resolución") +
  scale_colour_manual(values = c("black", "red", "blue")) +
  ggtitle("Medias de tiempo por Arquitectura y Resolución (detransformadas)")

# ==============================================================
# GRÁFICO DE MEDIAS Y BIGOTES (Arquitectura + Objetos)
# ==============================================================
Sum <- Summarize(T_sqrt ~ Arquitectura + Objetos, data = Data, digits = 3)

Sum$se <- Sum$sd / sqrt(Sum$n)

Sum$mean_original <- Sum$mean^2
Sum$se_original   <- (Sum$se)^2


pd <- position_dodge(0.2)  # separación horizontal entre grupos

ggplot(Sum, aes(x = Objetos, y = mean_original, color = Arquitectura)) +
  geom_errorbar(aes(ymin = mean_original - se_original,
                    ymax = mean_original + se_original),
                width = 0.2, size = 0.8, position = pd) +
  geom_point(shape = 15, size = 4, position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold")) +
  ylab("Tiempo medio de sintetizado (s)") +
  xlab("Objetos") +
  scale_colour_manual(values = c("black", "red", "blue")) +
  ggtitle("Medias de tiempo por Arquitectura y Objetos (detransformadas)")

# ==============================================================
# GRÁFICO DE MEDIAS Y BIGOTES (Arquitectura + Efectos)
# ==============================================================
Sum <- Summarize(T_sqrt ~ Arquitectura + Efectos, data = Data, digits = 3)

Sum$se <- Sum$sd / sqrt(Sum$n)

Sum$mean_original <- Sum$mean^2
Sum$se_original   <- (Sum$se)^2


pd <- position_dodge(0.2)  # separación horizontal entre grupos

ggplot(Sum, aes(x = Efectos, y = mean_original, color = Arquitectura)) +
  geom_errorbar(aes(ymin = mean_original - se_original,
                    ymax = mean_original + se_original),
                width = 0.2, size = 0.8, position = pd) +
  geom_point(shape = 15, size = 4, position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold")) +
  ylab("Tiempo medio de sintetizado (s)") +
  xlab("Efectos") +
  scale_colour_manual(values = c("black", "red", "blue")) +
  ggtitle("Medias de tiempo por Arquitectura y Efectos (detransformadas)")

# ==============================================================
# GRÁFICO DE MEDIAS Y BIGOTES (Arquitectura)
# ==============================================================

library(FSA)
library(ggplot2)

Sum <- Summarize(T_sqrt ~ Arquitectura, data = Data, digits = 3)

Sum$se <- Sum$sd / sqrt(Sum$n)

Sum$mean_original <- Sum$mean^2
Sum$se_original   <- (Sum$se)^2

ggplot(Sum, aes(x = Arquitectura, y = mean_original)) +
  geom_errorbar(aes(ymin = mean_original - se_original,
                    ymax = mean_original + se_original),
                width = 0.2, size = 0.8) +
  geom_point(shape = 15, size = 4, color = "steelblue") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size = 12)) +
  ylab("Tiempo medio de sintetizado (s)") +
  xlab("Arquitectura") +
  ggtitle("Medias de tiempo por Arquitectura (detransformadas)")

library(dplyr)

# ============================================================
# METRÍCA 1: Porcentaje de mejora de APU frente a CPU y GPU
#     Usando la variable transformada T_sqrt
# ============================================================

# Calcular el tiempo promedio (transformado) por arquitectura
mean_times <- Data %>%
  group_by(Arquitectura) %>%
  summarise(mean_T = mean(T_sqrt, na.rm = TRUE))

# Extraer los promedios por arquitectura
mean_apu <- mean_times$mean_T[mean_times$Arquitectura == "APU"]
mean_cpu <- mean_times$mean_T[mean_times$Arquitectura == "CPU"]
mean_gpu <- mean_times$mean_T[mean_times$Arquitectura == "GPU"]

# Calcular el porcentaje de mejora (positiva si APU es más rápida)
mejora_cpu <- (1 - mean_apu / mean_cpu) * 100
mejora_gpu <- (1 - mean_apu / mean_gpu) * 100

cat("Mejora % (APU vs CPU):", round(mejora_cpu, 2), "%\n")
cat("Mejora % (APU vs GPU):", round(mejora_gpu, 2), "%\n")

# ============================================================
# MÉTRICA 2: Estabilidad del rendimiento por arquitectura
#     Basada en el CV promedio entre combinaciones de factores
# ============================================================

cv_por_escenario <- Data %>%
  group_by(Arquitectura, Resolucion, Objetos, Efectos) %>%
  summarise(
    mean_T = mean(T_sqrt, na.rm = TRUE),
    sd_T = sd(T_sqrt, na.rm = TRUE),
    cv_escenario = (sd_T / mean_T) * 100,
    .groups = "drop"
  )

estabilidad <- cv_por_escenario %>%
  group_by(Arquitectura) %>%
  summarise(
    cv_promedio = mean(cv_escenario, na.rm = TRUE),
    estabilidad = 100 - cv_promedio
  )


cat("\nEstabilidad del rendimiento (datos transformados):\n")
print(estabilidad)







