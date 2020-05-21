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
 load_data <-function()
 {
     d_nac <- read.csv("covid19casos.csv")
    
     #cargo datos provinciales confidenciales
     d_prov <- read.csv("SISA-12-05_pilar.csv")
     #renombro tags para poder usar las mismas expresiones
     colnames(d_prov)[which(colnames(d_prov)=="Fecha1rSintoma")]="fecha_fis"
     colnames(d_prov)[which(colnames(d_prov)=="PROVINCIA_CARGA")]="provincia_carga"
     colnames(d_prov)[which(colnames(d_prov)=="CLASIF_RESUMEN")]="clasificacion_resumen"
     #acomodo el formato de la fecha que no es ISO
     d_prov$fecha_fis=as.Date(d_prov$fecha_fis,format="%d/%m/%Y")
     #a los asintomáticos les asigno la fecha de inicio de sintomas 
     #que está tambien en otro formato
     for(w in which(is.na(d_prov$fecha_fis)))
     {
             d_prov$fecha_fis[w]=as.Date(substring(d_prov$Toma_MUESTRA[w],1,10),format="%d-%m-%Y")
     }
    
     d_prov$FECHA_MOD_DIAG=as.Date(d_prov$FECHA_MOD_DIAG,format="%d/%m/%Y")
     d_prov$FECHA_FALLECIMIENTO=as.Date(d_prov$FECHA_FALLECIMIENTO,format="%d/%m/%Y")
     return(list(nac=d_nac,prov=d_prov))
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


 load_data_prov <-function()
 {
     #d_prov <- read.csv("SISA-12-05_pilar.csv")
     d_prov <- read.csv("BM_2020.05.15.csv")
     d_prov =subset(d_prov,F_CONF!="")
     
     
     d_prov$FIS=as.Date(repair_date(d_prov$FIS),format="%d/%m/%Y")
     #a los asintomáticos les asigno la fecha de inicio de sintomas 
     #que está tambien en otro formato
     
     F_CASO=d_prov$FIS

     for(w in which(is.na(d_prov$FIS)))
     {
	  ##   print(w)
	  ##   arr=c( d_prov$PCR_1[w],d_prov$PCR_2[w],
	  ##          d_prov$PCR_3[w],d_prov$PCR_4[w],
	  ##          d_prov$PCR_5[w],d_prov$PCR_5[w],
	  ##          d_prov$PCR_6[w])

	  ##   #que diga no
	  ##   l1=grepl("no",arr,ignore.case=TRUE)
          ##   #que diga detectable
	  ##   l2=grepl("detectable",arr,ignore.case=TRUE)
          ##   #el primero que diga detectable (sin un no)
	  ##   pcr_positivo=min(which((!l1)&l2))
	  ##   tag=paste0("F_PCR_",as.character(pcr_positivo))

             F_CASO[w]=as.Date(repair_date(d_prov$F_CONF[w]),format="%d/%m/%Y")
     }
     d_prov$F_CASO=F_CASO
     d_prov$F_ALTA_M=as.Date(repair_date(d_prov$F_ALTA_M),format="%d/%m/%Y")
     d_prov$F_MUERTE=as.Date(repair_date(d_prov$F_MUERTE),format="%d/%m/%Y")
     return(d_prov)
 }
 split_cases <- function(data)
 {
     clasif=c(
      "Caso confirmado - No Activo por criterio de laboratorio"            ,# [1]
      "Caso confirmado - No activo (por tiempo de evolución)"              ,# [2]
      "Caso confirmado - Activo "                                          ,# [3]
      "Caso confirmado - Activo Internado"                                 ,# [4]
      "Caso confirmado - Fallecido"                                        ,# [5]
      "Caso confirmado - No activo (por laboratorio y tiempo de evolución)")# [6]
     confirmados=subset(data,clasificacion_resumen=="Confirmado")
     recuperados=subset(data,
            	   (CLASIFICACION==clasif[1] |
            	   CLASIFICACION==clasif[2] |
            	   CLASIFICACION==clasif[6])
                    )
     activos=subset(data,
            	    CLASIFICACION==clasif[3] |
            	    CLASIFICACION==clasif[4] )
     
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
