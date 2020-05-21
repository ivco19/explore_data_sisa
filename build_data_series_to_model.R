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

 c_prov=subset(ss[["confirmados"]], EDAD>=60)
 a_prov=subset(ss[["activos"]], EDAD>=60)
 r_prov=subset(ss[["recuperados"]], EDAD>=60)
 f_prov=subset(ss[["fallecidos"]], EDAD>=60)
     
 conf_myrs = acumula_serie(c_prov,tfis,fecha_min,breaks)
 act_myrs  = acumula_serie(a_prov,tfis,fecha_min,breaks)
 rec_myrs  = acumula_serie(r_prov,trec,fecha_min,breaks)
 fall_myrs = acumula_serie(f_prov,tdead,fecha_min,breaks)

 c_prov=subset(ss[["confirmados"]], EDAD<60)
 a_prov=subset(ss[["activos"]], EDAD<60)
 r_prov=subset(ss[["recuperados"]], EDAD<60)
 f_prov=subset(ss[["fallecidos"]], EDAD<60)

 conf_men = acumula_serie(c_prov,tfis,fecha_min,breaks)
 act_men  = acumula_serie(a_prov,tfis,fecha_min,breaks)
 rec_men  = acumula_serie(r_prov,trec,fecha_min,breaks)
 fall_men = acumula_serie(f_prov,tdead,fecha_min,breaks)


 to_fit <- data.frame(
      fecha=as.Date(0:maxf+fecha_min),
      dia = conf_men$dias,
      confirmados_menores_diarios   = conf_men$his,
      confirmados_menores_acumulados= conf_men$cum,
      activos_menores_diarios       = act_men$his,
      activos_menores_acumulados    = act_men$cum,
      recuperados_menores_diarios   = rec_men$his,
      recuperados_menores_acumulados= rec_men$cum,
      fallecidos_menores_diarios    = fall_men$his,
      fallecidos_menores_acumulados = fall_men$cum,
      confirmados_mayores_diarios   = conf_myrs$his,
      confirmados_mayores_acumulados= conf_myrs$cum,
      activos_mayores_diarios       = act_myrs$his,
      activos_mayores_acumulados    = act_myrs$cum,
      recuperados_mayores_diarios   = rec_myrs$his,
      recuperados_mayores_acumulados= rec_myrs$cum,
      fallecidos_mayores_diarios    = fall_myrs$his,
      fallecidos_mayores_acumulados = fall_myrs$cum
 )

 #write.csv(to_fit,file="data_to_fit.csv",row.names = FALSE)
 write.csv(to_fit,file="data_to_fit_BM.csv",row.names = FALSE)



