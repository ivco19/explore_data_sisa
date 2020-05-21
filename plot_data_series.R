source("funciones.R")

 #dd=load_data()
 #d_nac=dd[["nac"]]
 #d_prov=dd[["prov"]]
 d_prov=load_data_prov()

 #para la base de datos nacional vamos a ver recuperados usando
 #clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA"
 # rec_nac=subset(d_nac,
 #		clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA" &
 #		clasificacion_resumen=="Confirmado" & 
 #		provincia_carga=="Córdoba")

 #ss=split_cases(d_prov)
 ss=split_cases_bm(d_prov)
 c_prov=ss[["confirmados"]]
 a_prov=ss[["activos"]]
 r_prov=ss[["recuperados"]]
 f_prov=ss[["fallecidos"]]
 
 #tfis="fecha_fis"
 tfis="F_CASO"
 #trec="FECHA_MOD_DIAG"
 trec="F_ALTA_M"
 #tdead=FECHA_FALLECIMIENTO
 tdead="F_MUERTE"


 c=as.Date(c_prov[[tfis]])
 fecha_min=min(c)
 ff=d_prov[[tfis]]
 max_fis=max(as.numeric(ff[!is.na(ff)]-fecha_min))
 ff=d_prov[[trec]]
 max_mod=max(as.numeric(ff[!is.na(ff)]-fecha_min))
 ff=d_prov[[tdead]]
 max_fall=max(as.numeric(ff[!is.na(ff)]-fecha_min))

 maxf=max(c(max_fis,max_mod,max_fall))
 breaks=seq(-0.5,maxf+0.5,by=1)

 pdf("casos_covid19_cba.pdf")
     plot_casos(c_prov,tfis,fecha_min,breaks,title="Acumulado Córdoba, Pop Gral",ylim=c(1,400))
     oplot_casos(a_prov,tfis,fecha_min,breaks,col="red")
     oplot_casos(r_prov,trec,fecha_min,breaks,col="blue")
     oplot_casos(f_prov,tdead,fecha_min,breaks,col="green")
     
     legend("bottomright",col=c("black","blue","red","green"),
            legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

     c_prov=subset(ss[["confirmados"]], EDAD>=60)
     a_prov=subset(ss[["activos"]], EDAD>=60)
     r_prov=subset(ss[["recuperados"]], EDAD>=60)
     f_prov=subset(ss[["fallecidos"]], EDAD>=60)

     plot_casos(c_prov,tfis,fecha_min,breaks,title="Acumulado Córdoba, Mayores de 60",ylim=c(1,400))
     oplot_casos(a_prov,tfis,fecha_min,breaks,col="red")
     oplot_casos(r_prov,trec,fecha_min,breaks,col="blue")
     oplot_casos(f_prov,tdead,fecha_min,breaks,col="green")
     
     legend("bottomright",col=c("black","blue","red","green"),
            legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

     c_prov=subset(ss[["confirmados"]], EDAD<60)
     a_prov=subset(ss[["activos"]], EDAD<60)
     r_prov=subset(ss[["recuperados"]], EDAD<60)
     #f_prov=subset(ss[["fallecidos"]], EDAD<60)

     plot_casos(c_prov,tfis,fecha_min,breaks,title="Acumulado Córdoba, Menores de 60",ylim=c(1,400))
     oplot_casos(a_prov,tfis,fecha_min,breaks,col="red")
     oplot_casos(r_prov,trec,fecha_min,breaks,col="blue")
     #oplot_casos(f_prov,tdead,fecha_min,breaks,col="green")
     
     legend("bottomright",col=c("black","blue","red","green"),
            legend=c("Confirmados","Recuperados","Activos","Fallecidos"),lty=1,bty="n")

 dev.off()


	
		
