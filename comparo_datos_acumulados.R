 source("funciones.R")
 #cargo datos nacionales publicos
 #http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina

 #comparo los datos provinciales con los nacionales, esto parece confirmar que en los
 #datos nacionales se carga para los asintomáticos la fecha de inicio de síntomas como
 #la fecha del hisopado
 d_prov=load_data_prov()
 d_nac=load_data_nac()

 
 c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba"))
 c_prov=d_prov #subset(d_prov,(clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba"))
 c=c_nac$F_CASO
 fecha_min=min(c)
 fecha_max=max(c)
 diff=as.numeric(fecha_max-fecha_min)
 breaks=seq(-0.5,diff+0.5,by=1)
 pdf("acumulado_confirmados_datos_vs.pdf")
 plot_casos(c_nac,"F_CASO",fecha_min,breaks,title="Acumulado Córdoba")
 oplot_casos(c_prov,"F_CASO",fecha_min,breaks,col="red")
 dev.off()

