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

  ei0=which(data$EDAD_ACTUAL < 60)
  ei1=which(data$EDAD_ACTUAL >= 60)
  Ninf <- c(length(data$fecha_fis),length(data$fecha_fis[ei0]),length(data$fecha_fis[ei1]) )

  # ///////////////////
  falle <- subset(data, FALLECIDO == "SI")

  ###N002: faltan cargar algunas fechas intermedias, repito fis
  F_fis <- falle$fecha_fis
  F_int <- as.Date(falle$FECHA_INTERNACION, format = "%d/%m/%Y")
  w0 <- which(is.na(F_int))
  F_int[w0] <- F_fis[w0]
  #NOTA: SI NO CUI, puede decir que no entró a cuidados intensivos
  # tons si F_CUI==F_INT, no entró.
  F_cui <- as.Date(falle$FECHA_CUI_INTENSIVOS, format = "%d/%m/%Y")
  w1 <- which(is.na(F_cui))
  F_cui[w1] <- F_int[w1]
  ###N002/
  F_mue <- as.Date(falle$FECHA_FALLECIMIENTO, format = "%d/%m/%Y")
  if (length(which(is.na(F_mue))) != 0) print("MUERTO SIN FECHA!!!!")

  D_int <- as.numeric(F_mue - F_int)
  D_cui <- as.numeric(F_mue - F_cui)
  D_fis <- as.numeric(F_mue - F_fis)

  e0=which(falle$EDAD_ACTUAL < 60)
  e1=which(falle$EDAD_ACTUAL >= 60)
  tau_fis=c(median(D_fis),median(D_fis[e0]),median(D_fis[e1]))
  tau_int=c(median(D_int),median(D_int[e0]),median(D_int[e1]))
  tau_cui=c(median(D_cui),median(D_cui[e0]),median(D_cui[e1]))
  #esto podria ser el lenght e1 o e0
  Nfat  = c(length(falle$fecha_fis),length(falle$fecha_fis[e0]),length(falle$fecha_fis[e1]))
  TasaDMuerte <- Nfat/Ninf

  print(c("Summario:"))
  print(c("Nfallecidos:", Nfat, "Tasa de Mortalidad (mu):", TasaDMuerte))
  print("<Tiempos Medios>")
  print(c("Tiempo desde FIS:", tau_fis))
  print(c("Tiempo desde Internacion:", tau_int))
  print(c("Tiempo desde CUI:", tau_cui))

  return(list(
    Nfat = Nfat, TasaDeMue = TasaDMuerte,
    tfis = tau_fis, tcui = tau_cui, tint = tau_int,
    Edades = falle$EDAD_ACTUAL,
    D_fis = D_fis, D_cui = D_cui, D_int = D_int,
    F_mue = F_mue, F_int = F_int, F_cui = F_cui, F_fis = F_fis
  ))
}

recuperados <- function(data) {
  if (missing(data)) data <- prepara_datos("sisa.csv")

  ei0=which(data$EDAD_ACTUAL < 60)
  ei1=which(data$EDAD_ACTUAL >= 60)
  Ninf <- c(length(data$fecha_fis),length(data$fecha_fis[ei0]),length(data$fecha_fis[ei1]) )
  # //////////////////
  ClasSel=grepl("no activo",tolower(data$CLASIFICACION))
  
  recu <- subset(data, ClasSel)

  ###N002:
  R_fis <- recu$fecha_fis
  R_int <- as.Date(recu$FECHA_INTERNACION, format = "%d/%m/%Y")
  w0 <- which(is.na(R_int))
  R_int[w0] <- R_fis[w0]
  R_cui <- as.Date(recu$FECHA_CUI_INTENSIVOS, format = "%d/%m/%Y")
  w1 <- which(is.na(R_cui))
  R_cui[w1] <- R_int[w1]
  ##N003: Pilar usar fecha de modificacion de diagsnotico!
  R_alta <- as.Date(recu$FECHA_MOD_DIAG, format = "%d/%m/%Y")
  if(length(which(is.na(R_alta)))!= 0) print("TODO MAL CON LA FECHA DE ALTA")

  D_int <- as.numeric(R_alta - R_int)
  D_cui <- as.numeric(R_alta - R_cui)
  D_fis <- as.numeric(R_alta - R_fis)
  
  #divido por edades
  e0=which(recu$EDAD_ACTUAL < 60)
  e1=which(recu$EDAD_ACTUAL >= 60)
  tau_fis=c(median(D_fis),median(D_fis[e0]),median(D_fis[e1]))
  tau_int=c(median(D_int),median(D_int[e0]),median(D_int[e1]))
  tau_cui=c(median(D_cui),median(D_cui[e0]),median(D_cui[e1]))
  
  Nrec  = length(recu$fecha_fis)
  Nrec0 = length(recu$fecha_fis[e0])
  Nrec1 = length(recu$fecha_fis[e1])
  Nrec  = c(Nrec,Nrec0,Nrec1)
  TasaDRecu <- Nrec/Ninf
  
  print(c("Summario:"))
  print(c("NRecuperados:", Nrec, "Tasa de recuperabilidad (gamma):", TasaDRecu))
  print("<Tiempos Medios>")
  print(c("Tiempo desde FIS (<60,>60):",tau_fis))
  print(c("Tiempo desde Internacion(<60,>60):",tau_int))
  print(c("Tiempo desde CUI(<60,>60):", tau_cui))

  return(list(
    Nrec = Nrec, TasaDeRecu = TasaDRecu,
    tfis = tau_fis, tcui = tau_cui, tint = tau_int,
    Edades = recu$EDAD_ACTUAL,
    D_fis = D_fis, D_cui = D_cui, D_int = D_int,
    R_alta = R_alta, R_int = R_int, R_cui = R_cui, R_fis=R_fis
  ))
}