#Funcion general que genera limpia los datos por confirmados

prepara_datos <- function(name) {
  if (missing(name)) name <- "sisa.csv"

  d_prov <- read.csv("sisa.csv")
  # renombro tags para poder usar las mismas expresiones (ver F001)
  colnames(d_prov)[which(colnames(d_prov) == "Fecha1rSintoma")] <- "fecha_fis"
  colnames(d_prov)[which(colnames(d_prov) == "PROVINCIA_CARGA")] <- "provincia_carga"
  colnames(d_prov)[which(colnames(d_prov) == "CLASIF_RESUMEN")] <- "clasificacion_resumen"
  d_prov$fecha_fis <- as.Date(d_prov$fecha_fis, format = "%d/%m/%Y")
  # N001
  for (w in which(is.na(d_prov$fecha_fis))) {
    d_prov$fecha_fis[w] <- as.Date(substring(d_prov$Toma_MUESTRA[w], 1, 10), format = "%d-%m-%Y")
  }
  # N001
  # provincia_carga NO SERIA NECESARIO, pero por las dudas
  d_prov <- subset(d_prov, clasificacion_resumen == "Confirmado" & provincia_carga == "Córdoba")
  return(d_prov)
}

#Funcion para sacar solo los datos de los fallecidos.... devuelve los datos en forma de data.frame
fallecidos <- function(data) {
  if (missing(data)) data <- prepara_datos("sisa.csv")


  Ninf <- length(data$fecha_fis)
  # ///////////////////
  falle <- subset(data, FALLECIDO == "SI")
  TasaDMuerte <- length(falle$fecha_fis) / Ninf

  F_fis <- falle$fecha_fis
  F_int <- as.Date(falle$FECHA_INTERNACION, format = "%d/%m/%Y")
  w0 <- which(is.na(F_int))
  F_int[w0] <- F_fis[w0]
  F_cui <- as.Date(falle$FECHA_CUI_INTENSIVOS, format = "%d/%m/%Y")
  w1 <- which(is.na(F_cui))
  F_cui[w1] <- F_int[w1]

  F_mue <- as.Date(falle$FECHA_FALLECIMIENTO, format = "%d/%m/%Y")
  if (length(which(is.na(F_mue))) != 0) print("MUERTO SIN FECHA!!!!")

  D_int <- as.numeric(F_mue - F_int)
  D_cui <- as.numeric(F_mue - F_cui)
  D_fis <- as.numeric(F_mue - F_fis)


  print(c("Summario:"))
  print(c("Nfallecidos:", length(D_fis), "Tasa de Mortalidad (mu):", TasaDMuerte))
  print("<Tiempos Medios>")
  print(c("Tiempo desde FIS:", mean(D_fis)))
  print(c("Tiempo desde Internacion:", mean(D_int)))
  print(c("Tiempo desde CUI:", mean(D_cui)))

  return(data.frame(
    Nfat = length(D_fis), TasaDeMue = TasaDMuerte,
    Dfis = mean(D_fis), Dcui = mean(D_cui), Dint = mean(D_int),
    Edades = falle$EDAD_ACTUAL,
    D_fis = D_fis, D_cui = D_cui, D_int = D_int,
    F_mue = F_mue, F_int = F_int, F_cui = F_cui
  ))
}
