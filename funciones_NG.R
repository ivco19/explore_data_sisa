# Funcion general que genera limpia los datos por confirmados
r_date <-function(charin) # Origuinal idea de Dante
 {
	 for (i in 1:length(charin))
	 {
	       	 a=strsplit(charin[i],"/")
		 if(length(a[[1]])>0)
		 {
			 if(nchar(a[[1]][3])==2)
			 {
			     charin[i]=paste0(paste(a[[1]][1],a[[1]][2],sep="/"),"/2020")
			 }
		 }
	 }
	 return(charin)
 }


prepara_datos <- function(name, type) {
  if (missing(name)) name <- "sisa.20.05.12.csv"
  if (missing(type)) type <- "sisa"
  #TODO chequear el formato ingresado
  if (type == "sisa") {
    d_prov <- read.csv(name)
    # renombro tags para poder usar las mismas expresiones (ver F001)
    colnames(d_prov)[which(colnames(d_prov) == "Fecha1rSintoma")] <- "fecha_fis"
    colnames(d_prov)[which(colnames(d_prov) == "PROVINCIA_CARGA")] <- "provincia_carga"
    colnames(d_prov)[which(colnames(d_prov) == "CLASIF_RESUMEN")] <- "clasificacion_resumen"
    # provincia_carga NO SERIA NECESARIO, pero por las dudas
    d_prov <- subset(d_prov, clasificacion_resumen == "Confirmado" & provincia_carga == "Córdoba")
    fecha_fis <- as.Date(d_prov$fecha_fis, format = "%d/%m/%Y")
    # N001
    w <- which(is.na(fecha_fis))
    if ( length(w) != 0 ) fecha_fis[w] <- as.Date(substring(d_prov$Toma_MUESTRA[w], 1, 10), format = "%d-%m-%Y")
    # N001
    fecha_int  = as.Date(d_prov$FECHA_INTERNACION, format = "%d/%m/%Y")
    fecha_cui  = as.Date(d_prov$FECHA_CUI_INTENSIVOS, format = "%d/%m/%Y")
    fecha_mue  = as.Date(d_prov$FECHA_FALLECIMIENTO, format = "%d/%m/%Y")
    fecha_alta = as.Date(d_prov$FECHA_MOD_DIAG, format = "%d/%m/%Y")
    #####
    VecRecu <- grepl("no activo", tolower(d_prov$CLASIFICACION))
    VecFall <- which(d_prov$FALLECIDO == "SI")
    ClassSel <- gsub("Confirmado", "Activo", d_prov$clasificacion_resumen)
    ClassSel[VecRecu] <- "Recuperado"
    ClassSel[VecFall] <- "Fallecido"
    #### Posibles valores de ClassSel = [Fallecido, Recuperado, Activo]
  ##### QUIERO LA MISMA DATA ASI QUE LIMPIO Y USO LO QUE NECESITO
    dataout <- data.frame(
      edad = d_prov$EDAD_ACTUAL, fecha_fis = fecha_fis,
      fecha_int = fecha_int, fecha_cui = fecha_cui,
      fecha_mue = fecha_mue, ClassSel = ClassSel, 
      fecha_alta = fecha_alta
    )
  } else if(type == "BM") {
    d_prov <- read.csv(name)
    d_prov =subset(d_prov,F_CONF!="")
    
    fecha_fis=as.character(d_prov$FIS)
    fecha_fis=as.Date(r_date(fecha_fis),format="%d/%m/%Y")
    w=which(is.na(fecha_fis))
    F_tmp=as.Date(r_date(as.character(d_prov$F_CONF)),format="%d/%m/%Y")
    if (length(w) != 0) fecha_fis[w]=F_tmp[w]
    F_MUERTE=as.Date(r_date(as.character(d_prov$F_MUERTE)),format="%d/%m/%Y")
    F_ALTA_M=as.Date(r_date(as.character(d_prov$F_ALTA_M)),format="%d/%m/%Y")
    F_INT   =as.Date(r_date(as.character(d_prov$F_INT)),format="%d/%m/%Y")
    print("CUIDADO BASE MADRE NO DISCRIMINA CUIDADOS INTENSIVOS, USO Internacion")
#####
    VecRecu <- which(!is.na(F_ALTA_M))
    VecFall <- which(!is.na(F_MUERTE))
    ClassSel <- gsub("SI", "Activo", d_prov$ACTIVO)
    ClassSel[VecRecu] <- "Recuperado"
    ClassSel[VecFall] <- "Fallecido"
    w <- which(ClassSel == "NO" )
    if (length(w) != 0) { 
    	print("OJO< NO ACTIVO PERO NO RECUPERADO NI FALLECIDO")
    } 
    
##### QUIERO LA MISMA DATA ASI QUE LIMPIO Y USO LO QUE NECESITO
    dataout <- data.frame(
      edad = d_prov$EDAD, fecha_fis = fecha_fis,
      fecha_int = F_INT   , fecha_cui = F_INT,
      fecha_mue = F_MUERTE, ClassSel = ClassSel, fecha_alta = F_ALTA_M
    )
  } else {
    print(c(type, "NO IMPLEMENTADO"))
    return(NULL)
  }
  
  return(dataout)
}

# Funcion para sacar solo los datos de los fallecidos.... devuelve los datos en forma de data.frame
fallecidos <- function(data) {
  if (missing(data)) data <- prepara_datos()

  ei0 <- which(data$edad < 60)
  ei1 <- which(data$edad >= 60)
  Ninf <- c(length(data$fecha_fis), length(data$fecha_fis[ei0]), length(data$fecha_fis[ei1]))

  # ///////////////////
  falle <- subset(data, ClassSel == "Fallecido")

  ### N002: faltan cargar algunas fechas intermedias, repito fis
  F_fis <- falle$fecha_fis
  #F_int <- as.Date(falle$fecha_int, format = "%d/%m/%Y")
  F_int <- falle$fecha_int
  w0 <- which(is.na(F_int))
  F_int[w0] <- F_fis[w0]
  # NOTA: SI NO CUI, puede decir que no entró a cuidados intensivos
  # tons si F_CUI==F_INT, no entró.
  #F_cui <- as.Date(falle$fecha_cui, format = "%d/%m/%Y")
  F_cui <- falle$fecha_cui
  w1 <- which(is.na(F_cui))
  F_cui[w1] <- F_int[w1]
  ### N002/
  #F_mue <- as.Date(falle$fecha_mue, format = "%d/%m/%Y")
  F_mue <- falle$fecha_mue
  if (length(which(is.na(F_mue))) != 0) print("MUERTO SIN FECHA!!!!")

  D_int <- as.numeric(F_mue - F_int)
  D_cui <- as.numeric(F_mue - F_cui)
  D_fis <- as.numeric(F_mue - F_fis)

  e0 <- which(falle$edad < 60)
  e1 <- which(falle$edad >= 60)
  tau_fis <- c(median(D_fis), median(D_fis[e0]), median(D_fis[e1]))
  tau_int <- c(median(D_int), median(D_int[e0]), median(D_int[e1]))
  tau_cui <- c(median(D_cui), median(D_cui[e0]), median(D_cui[e1]))
  # esto podria ser el lenght e1 o e0
  Nfat <- c(length(falle$fecha_fis), length(falle$fecha_fis[e0]), length(falle$fecha_fis[e1]))
  TasaDMuerte <- Nfat / Ninf

  print(c("Summario:"))
  print(c("Nfallecidos:", Nfat, "Tasa de Mortalidad (mu):", TasaDMuerte))
  print("<Tiempos Medios>")
  print(c("Tiempo desde FIS:", tau_fis))
  print(c("Tiempo desde Internacion:", tau_int))
  print(c("Tiempo desde CUI:", tau_cui))

  return(list(
    Nfat = Nfat, TasaDeMue = TasaDMuerte,
    tfis = tau_fis, tcui = tau_cui, tint = tau_int,
    Edades = falle$edad,
    D_fis = D_fis, D_cui = D_cui, D_int = D_int,
    F_mue = F_mue, F_int = F_int, F_cui = F_cui, F_fis = F_fis
  ))
}

recuperados <- function(data) {
  if (missing(data)) data <- prepara_datos()

  ei0 <- which(data$edad < 60)
  ei1 <- which(data$edad >= 60)
  Ninf <- c(length(data$fecha_fis), length(data$fecha_fis[ei0]), length(data$fecha_fis[ei1]))
  # //////////////////
  ClasSel <- grepl("recuperado", tolower(data$ClassSel))

  recu <- subset(data, ClasSel)

  ### N002:
  R_fis <- recu$fecha_fis
  #R_int <- as.Date(recu$fecha_int, format = "%d/%m/%Y")
  R_int <- recu$fecha_int
  w0 <- which(is.na(R_int))
  R_int[w0] <- R_fis[w0]
  #R_cui <- as.Date(recu$fecha_cui, format = "%d/%m/%Y")
  R_cui <- recu$fecha_cui
  w1 <- which(is.na(R_cui))
  R_cui[w1] <- R_int[w1]
  ## N003: Pilar usar fecha de modificacion de diagsnotico!
  ##R_alta <- as.Date(recu$fecha_alta, format = "%d/%m/%Y")
  R_alta <- recu$fecha_alta
  if (length(which(is.na(R_alta))) != 0) print("TODO MAL CON LA FECHA DE ALTA")

  D_int <- as.numeric(R_alta - R_int)
  D_cui <- as.numeric(R_alta - R_cui)
  D_fis <- as.numeric(R_alta - R_fis)

  # divido por edades
  e0 <- which(recu$edad < 60)
  e1 <- which(recu$edad >= 60)
  tau_fis <- c(median(D_fis), median(D_fis[e0]), median(D_fis[e1]))
  tau_int <- c(median(D_int), median(D_int[e0]), median(D_int[e1]))
  tau_cui <- c(median(D_cui), median(D_cui[e0]), median(D_cui[e1]))

  Nrec <- length(recu$fecha_fis)
  Nrec0 <- length(recu$fecha_fis[e0])
  Nrec1 <- length(recu$fecha_fis[e1])
  Nrec <- c(Nrec, Nrec0, Nrec1)
  TasaDRecu <- Nrec / Ninf

  print(c("Summario:"))
  print(c("NRecuperados:", Nrec, "Tasa de recuperabilidad (gamma):", TasaDRecu))
  print("<Tiempos Medios>")
  print(c("Tiempo desde FIS (<60,>60):", tau_fis))
  print(c("Tiempo desde Internacion(<60,>60):", tau_int))
  print(c("Tiempo desde CUI(<60,>60):", tau_cui))

  return(list(
    Nrec = Nrec, TasaDeRecu = TasaDRecu,
    tfis = tau_fis, tcui = tau_cui, tint = tau_int,
    Edades = recu$edad,
    D_fis = D_fis, D_cui = D_cui, D_int = D_int,
    R_alta = R_alta, R_int = R_int, R_cui = R_cui, R_fis = R_fis
  ))
}
