 plot_casos <-function(data,...)
 {
      data_set=subset(data,...)
      p=acumula_serie(data_set)

      if(length(unique(data_set$provincia_carga))==1) {
        title=paste("Acumulado",data_set$provincia_carga[1])
      } else {
	      title="Acumulado Nacional"
      }

      ytitle=paste("Número de casos",data_set$clasificacion_resumen[1])
      xtitle=paste("Dias desde",p[["fecha_min"]])
      plot(p[["dias"]],p[["cum"]],log="y",type="l",main=title,xlab=xtitle,ylab=ytitle)
 }

 acumula_serie <- function(data)
 {
       c=as.Date(data$fecha_fis)
       #extrañamente los datos de la base nacional siempre hay una fecha fis
       #pero en la base sisa de pilar muchos casos no tiene fis... me parece
       #que es por los asintomáticos, a nivel nacional algo estan haciendo
       #por ejemplo estan poniendo la fecha de isopado
       c=c[!is.na(c)]
       fecha_min=min(c)
       c = (as.numeric(c-fecha_min))
       h=hist(c,breaks=max(c),plot=FALSE)
       cum=cumsum(h$counts)
       dias=h$breaks[1:(length(h$breaks)-1)]
 
       return(list(dias=dias,cum=cum,fecha_min=fecha_min))
 }

 #cargo datos nacionales publicos
 d_nac <- read.csv("covid19casos.csv")

 pdf("acumulado_confirmados_datos_publicos.pdf")
 plot_casos(d_nac,clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba")
 dev.off()

 #cargo datos provinciales confidenciales
 d_prov <- read.csv("SISA-12-05_pilar.csv")
 #renombro tags para poder usar las mismas expresiones
 colnames(d_prov)[which(colnames(d_prov)=="Fecha1rSintoma")]="fecha_fis"
 colnames(d_prov)[which(colnames(d_prov)=="PROVINCIA_CARGA")]="provincia_carga"
 colnames(d_prov)[which(colnames(d_prov)=="CLASIF_RESUMEN")]="clasificacion_resumen"
 #acomodo el formato de la fecha que no es ISO
 d_prov$fecha_fis=as.Date(d_prov$fecha_fis,format="%d/%m/%Y")
 #which(is.na(d_prov$fecha_fis))


 pdf("acumulado_confirmados_datos_confid.pdf")
 plot_casos(d_prov,clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba")
 dev.off()
