source("funciones.R")

dd=load_data()
d_nac=dd[["nac"]]
d_prov=dd[["prov"]]

#para la base de datos nacional vamos a ver recuperados usando
#clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA"
# rec_nac=subset(d_nac,
#		clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA" &
#		clasificacion_resumen=="Confirmado" & 
#		provincia_carga=="Córdoba")



 ss=split_cases(d_prov)

 c_prov=ss[["confirmados"]]
 a_prov=ss[["activos"]]
 r_prov=ss[["recuperados"]]
 f_prov=ss[["fallecidos"]]

 c=as.Date(c_prov$fecha_fis)
 fecha_min=min(c)

 pdf("casos_covid19_cba.pdf")
 plot_casos(c_prov,"fecha_fis",fecha_min,title="Acumulado Córdoba, Pop Gral",ylim=c(1,300))
 oplot_casos(a_prov,"fecha_fis",fecha_min,col="red")
 oplot_casos(r_prov,"FECHA_MOD_DIAG",fecha_min,col="blue")
 oplot_casos(f_prov,"FECHA_FALLECIMIENTO",fecha_min,col="green")
 
 legend("bottomright",col=c("black","blue","red","green"),
        legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

 c_prov=subset(ss[["confirmados"]], EDAD_ACTUAL>=60)
 a_prov=subset(ss[["activos"]], EDAD_ACTUAL>=60)
 r_prov=subset(ss[["recuperados"]], EDAD_ACTUAL>=60)
 f_prov=subset(ss[["fallecidos"]], EDAD_ACTUAL>=60)

 plot_casos(c_prov,"fecha_fis",fecha_min,title="Acumulado Córdoba, Mayores de 60",ylim=c(1,300))
 oplot_casos(a_prov,"fecha_fis",fecha_min,col="red")
 oplot_casos(r_prov,"FECHA_MOD_DIAG",fecha_min,col="blue")
 oplot_casos(f_prov,"FECHA_FALLECIMIENTO",fecha_min,col="green")
 
 legend("bottomright",col=c("black","blue","red","green"),
        legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

 c_prov=subset(ss[["confirmados"]], EDAD_ACTUAL<60)
 a_prov=subset(ss[["activos"]], EDAD_ACTUAL<60)
 r_prov=subset(ss[["recuperados"]], EDAD_ACTUAL<60)
 #f_prov=subset(ss[["fallecidos"]], EDAD_ACTUAL<60)

 plot_casos(c_prov,"fecha_fis",fecha_min,title="Acumulado Córdoba, Menores de 60",ylim=c(1,300))
 oplot_casos(a_prov,"fecha_fis",fecha_min,col="red")
 oplot_casos(r_prov,"FECHA_MOD_DIAG",fecha_min,col="blue")
 #oplot_casos(f_prov,"FECHA_FALLECIMIENTO",fecha_min,col="green")
 
 legend("bottomright",col=c("black","blue","red","green"),
        legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

 dev.off()


	
		