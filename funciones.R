 plot_casos <-function(data,fecha_min,title=title)
 {
      p=acumula_serie(data,fecha_min)

      #ytitle=paste("Número de casos",data$clasificacion_resumen[1])
      ytitle="Número de casos"
      xtitle=paste("Dias desde",p[["fecha_min"]])
      plot(p[["dias"]],p[["cum"]],log="y",type="l",main=title,xlab=xtitle,ylab=ytitle)
 }
 oplot_casos <-function(data,fecha_min,col=col)
 {
      p=acumula_serie(data,fecha_min)
      lines(p[["dias"]],p[["cum"]],col=col)
 }
 acumula_serie <- function(data,fecha_min)
 {
       c=as.Date(data$fecha_fis)
       c=c[!is.na(c)]
       c = (as.numeric(c-fecha_min))
       h=hist(c,breaks=max(c),plot=FALSE)
       cum=cumsum(h$counts)
       dias=h$breaks[1:(length(h$breaks)-1)]
 
       return(list(dias=dias,cum=cum,fecha_min=fecha_min))
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
    
      return(list(nac=d_nac,prov=d_prov))
 }

