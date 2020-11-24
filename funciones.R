 plot_casos <-function(data,fecha_var,fecha_min,breaks,title=title,...)
 {
      p=acumula_serie(data,fecha_var,fecha_min,breaks)

      #ytitle=paste("Número de casos",data$clasificacion_resumen[1])
      ytitle="Número de casos"
      xtitle=paste("Dias desde",p[["fecha_min"]])
      plot(p[["dias"]],p[["cum"]],log="y",type="l",main=title,xlab=xtitle,ylab=ytitle,...)
 }
 oplot_casos <-function(data,fecha_var,fecha_min,breaks,col=col)
 {
      p=acumula_serie(data,fecha_var,fecha_min,breaks)
      lines(p[["dias"]],p[["cum"]],col=col)
 }
 acumula_serie <- function(data,fecha_var,fecha_min,breaks)
 {
       c=as.Date(data[[fecha_var]])
       c=c[!is.na(c)]
       c = (as.numeric(c-fecha_min))
       h=hist(c,breaks,plot=FALSE)
       cum=cumsum(h$counts)
       dias=h$breaks[1:(length(h$breaks)-1)]
 
       return(list(dias=h$mids,cum=cum,his=h$counts,fecha_min=fecha_min))
 }
 repair_date <-function(char)
 { 
	 charo=char
	 for(i in 1:length(char))
	 {
	       	 a=strsplit(char[i],"/")
		 if(length(a[[1]])>0)
		 {
			 if(nchar(a[[1]][3])==2)
			 {
			     charo[i]=paste0(paste(a[[1]][1],a[[1]][2],sep="/"),"/2020")
			 }
		 }
	 }
	 return(charo)
 }

 load_data_sisa <-function()
 {
     #cargo datos provinciales confidenciales
     #d_prov <- read.csv("BM_2020.05.15.csv")
     d_prov <- read.csv2("SISA_13_07_08_35-Pilar.csv")
     #renombro tags para poder usar las mismas expresiones
     #colnames(d_prov)[which(colnames(d_prov)=="Fecha1rSintoma")]="fis"
     colnames(d_prov)[which(colnames(d_prov)=="FECHA_APERTURA")]="fis"
     colnames(d_prov)[which(colnames(d_prov)=="PROVINCIA_CARGA")]="provincia_carga"
     colnames(d_prov)[which(colnames(d_prov)=="CLASIF_RESUMEN")]="clasificacion_resumen"
     #acomodo el formato de la fecha que no es ISO
     d_prov$fis=as.Date(d_prov$fis,format="%d/%m/%Y")
     #a los asintomáticos les asigno la fecha de inicio de sintomas 
     #que está tambien en otro formato
     for(w in which(is.na(d_prov$fis)))
     {
             d_prov$fis[w]=as.Date(substring(d_prov$Toma_MUESTRA[w],1,10),format="%d-%m-%Y")
     }
    
     d_prov$FECHA_MOD_DIAG=as.Date(d_prov$FECHA_MOD_DIAG,format="%d/%m/%Y")
     d_prov$FECHA_FALLECIMIENTO=as.Date(d_prov$FECHA_FALLECIMIENTO,format="%d/%m/%Y")
     return(d_prov)
 }
 load_data_nac <-function()
 { 
     #los datos nacionales bajados de la red estan en 
     #file -i covid_19_casos.csv
     #covid_19_casos.csv: application/csv; charset=utf-16le
     #por eso pesan el doble que el utf-8 y por alguna razon
     #me cuesta leerlo en mi distrib de R, lo estoy transformando 
     #a utf-8 con libreoffice

     data <- read.csv2("covid_19_casos_n.csv")

     #continuan cambiando constantemente el formato de los datos
     data$fis=data$fecha_inicio_sintomas
     data$provincia_carga=data$carga_provincia_nombre
     data$fis                 = as.Date(data$fis,format="%Y-%m-%d")
     data$fecha_diagnostico   = as.Date(data$fecha_diagnostico,format="%Y-%m-%d")
     data$fecha_fallecimiento = as.Date(data$fecha_fallecimiento,format="%Y-%m-%d")

     data$F_CASO=data$fis 
     ii=is.na(data$fis)
     data$F_CASO[ii]=data$fecha_diagnostico[ii]

     return(data)
 }

 load_data_prov <-function()
 {
     #d_prov <- read.csv("SISA-12-05_pilar.csv")
     #d_prov <- read.csv("BM_2020.05.15.csv")
     #d_prov <- read.csv2("SISA_13_07_08_35-Pilar.csv")
     #d_prov <- read.csv2("SISA_22_07_12_59.csv")
     #d_prov <- read.csv2("SISA_27_08.csv")
     #d_prov <- read.csv2("SISA-10-09-2020_13-46-02.csv")
     #d_prov <- read.csv2("CORDOBA-21-09-2020_13-46-16.csv")
     #d_prov <- read.csv2("CORDOBA-23-09-2020_16-46-12.csv")
     #d_prov <- read.csv2("CORDOBA-03-10-2020_07-46-01.csv")
     #d_prov <- read.csv2("CORDOBA-13-10-2020_09-46-44.csv")
     #d_prov <- read.csv("CORDOBA-28-10-2020_16-46-36.csv")
#     d_prov <- read.csv("CORDOBA-12-11-20_15-01.csv")
     d_prov <- read.csv("CORDOBA-22-11-2020_15-01-51.csv")
     #d_prov =subset(d_prov,F_CONF!="")
     d_prov <- subset(d_prov,CLASIF_RESUMEN=="Confirmado")

     fisname="FECHA_APERTURA"
     faltaname="FECHA_ALTA_MEDICA"
     fmuertename="FECHA_FALLECIMIENTO"
     
     d_prov[[fisname]]=as.Date(repair_date(d_prov[[fisname]]),format="%d/%m/%Y")
     #d_prov[[fisname]]=as.Date(repair_date(d_prov[[fisname]]),format="%m/%d/%Y")
     #a los asintomáticos les asigno la fecha de inicio de sintomas 
     #que está tambien en otro formato
     
     F_CASO=d_prov[[fisname]]

     ###  for(w in which(is.na(d_prov$FIS)))
     ###  {
     ###       ##   print(w)
     ###       ##   arr=c( d_prov$PCR_1[w],d_prov$PCR_2[w],
     ###       ##          d_prov$PCR_3[w],d_prov$PCR_4[w],
     ###       ##          d_prov$PCR_5[w],d_prov$PCR_5[w],
     ###       ##          d_prov$PCR_6[w])

     ###       ##   #que diga no
     ###       ##   l1=grepl("no",arr,ignore.case=TRUE)
     ###       ##   #que diga detectable
     ###       ##   l2=grepl("detectable",arr,ignore.case=TRUE)
     ###       ##   #el primero que diga detectable (sin un no)
     ###       ##   pcr_positivo=min(which((!l1)&l2))
     ###       ##   tag=paste0("F_PCR_",as.character(pcr_positivo))

     ###          F_CASO[w]=as.Date(repair_date(d_prov$F_CONF[w]),format="%d/%m/%Y")
     ###  }

     d_prov$F_CASO=F_CASO
     d_prov$F_ALTA_M=as.Date(repair_date(d_prov[[faltaname]]),format="%d/%m/%Y")
     d_prov$F_MUERTE=as.Date(repair_date(d_prov[[fmuertename]]),format="%d/%m/%Y")
     #d_prov$F_ALTA_M=as.Date(repair_date(d_prov[[faltaname]]),format="%m/%d/%Y")
     #d_prov$F_MUERTE=as.Date(repair_date(d_prov[[fmuertename]]),format="%m/%d/%Y")

     ii=d_prov$FECHA_INTERNACION!=""
     jj=( d_prov$FECHA_ALTA_MEDICA[ii])!=""

     entrada=as.Date(repair_date((d_prov$FECHA_INTERNACION[ii])[jj]),format="%d/%m/%Y")
     salida=as.Date(repair_date((d_prov$FECHA_ALTA_MEDICA[ii])[jj]),format="%d/%m/%Y")
     #entrada=as.Date(repair_date((d_prov$FECHA_INTERNACION[ii])[jj]),format="%m/%d/%Y")
     #salida=as.Date(repair_date((d_prov$FECHA_ALTA_MEDICA[ii])[jj]),format="%m/%d/%Y")

     diff=as.numeric(salida-entrada)
     diff=diff[diff <100]
     mean(diff)



     return(d_prov)
 }
 split_cases <- function(data)
 {

	 #clasificacion previa a septiembre
       #"Caso confirmado - No Activo por criterio de laboratorio"           ,# [1]
       #"Caso confirmado - No activo (por tiempo de evolución)"             ,# [2]
       #"Caso confirmado - Activo "                                         ,# [3]
       #"Caso confirmado - Activo Internado"                                ,# [4]
       #"Caso confirmado - Fallecido"                                       )# [5]

	 #desde el 10 de septiembre
     clasif=c(
       "Caso confirmado por laboratorio - No activo (por tiempo de evolución)",# [1]                     
       "Caso confirmado por criterio clínico-epidemiológico  - No activo (por tiempo de evolución)",# [2]
       "Caso confirmado por laboratorio - Activo",# [3]                                                  
       "Caso confirmado por laboratorio - Activo Internado",# [4]                                        
       "Caso confirmado por laboratorio - Fallecido",#[5]                                               
        #categorias nuevas luego del 10 de sept.
       "Caso confirmado por laboratorio - No Activo por criterio de laboratorio",#[6]                   
       "Caso confirmado por criterio clinico-epidemiológico - Activo ")#[7]


     confirmados=subset(data,CLASIF_RESUMEN=="Confirmado")
     recuperados=subset(data,
            	   (CLASIFICACION==clasif[1] |
            	   CLASIFICACION==clasif[2] |
		   CLASIFICACION==clasif[6]
		   )
                    )
     activos=subset(data,
            	    CLASIFICACION==clasif[3] |
            	    CLASIFICACION==clasif[4] |
                    CLASIFICACION==clasif[7])
     
     fallecidos=subset(data,
            	    CLASIFICACION==clasif[5] )

     return(list(confirmados=confirmados,activos=activos,recuperados=recuperados,fallecidos=fallecidos))
 }

 split_cases_bm <- function(data)
 {
     confirmados =data
     recuperados = subset(data,!is.na(F_ALTA_M))
     activos     = subset(data,ACTIVO =="SI")
     fallecidos  = subset(data,!is.na(F_MUERTE))

     return(list(confirmados=confirmados,activos=activos,recuperados=recuperados,fallecidos=fallecidos))
 }
