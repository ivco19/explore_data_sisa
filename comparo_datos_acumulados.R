 source("funciones.R")
 #cargo datos nacionales publicos
 #http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina

 #comparo los datos provinciales con los nacionales, esto parece confirmar que en los
 #datos nacionales se carga para los asintomáticos la fecha de inicio de síntomas como
 #la fecha del hisopado
 d_prov=load_data_prov()
 d_nac=load_data_nac()

 
 c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba"))
 c_prov=subset(d_prov,(clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba"))
 c=as.Date(c_nac$fecha_fis)
 fecha_min=min(c)
 cc_prov=subset(d_prov,clasificacion_resumen=="Confirmado")
 pdf("acumulado_confirmados_datos_vs.pdf")
 plot_casos(c_nac,fecha_min,title="Acumulado Córdoba")
 oplot_casos(c_prov,col="red",fecha_min)
 oplot_casos(cc_prov,col="blue",fecha_min)
 dev.off()
