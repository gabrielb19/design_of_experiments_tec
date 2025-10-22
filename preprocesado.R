# Leer el archivo
lines <- readLines("path/a/archivo/de/datos/originales.txt")

# Filtrar líneas innecesarias
cleaned <- lines[!grepl("^(user|sys)", lines) & !grepl("image written", lines)]

# Limpiar las líneas no 'real': quitar antes de '_' y la extensión .sc
cleaned <- ifelse(
  grepl("^real", cleaned),
  cleaned,
  sub("\\.sc$", "", sub("^[^_]*_", "", cleaned))
)

# Extraer índices de las líneas "real"
real_idx <- grep("^real", cleaned)

# Crear un data frame vacío
data <- data.frame(Objetos=character(), Arquitectura=character(), 
                   Efectos=character(), Resolucion=character(), 
                   Runtime=numeric(), stringsAsFactors=FALSE)

# Recorrer las líneas "real" y emparejarlas con la siguiente línea
for (i in real_idx) {
  runtime <- as.numeric(gsub(",", ".", sub("real ", "", cleaned[i])))  # convertir coma a punto
  config <- cleaned[i + 1]
  
  # Separar partes: (Objetos)(Arquitectura)(Efectos)-(Resolucion)
  matches <- regmatches(config, regexec("^([0-9]+)([A-Za-z]+)([0-9]+)-(.+)$", config))[[1]]
  if (length(matches) == 5) {
    data <- rbind(data, data.frame(
      Objetos = matches[2],
      Arquitectura = matches[3],
      Efectos = matches[4],
      Resolucion = matches[5],
      Runtime = runtime,
      stringsAsFactors = FALSE
    ))
  }
}

# Guardar como CSV
write.csv(data, "path/a/archivo/de/datos/limpios.csv", row.names = FALSE)
