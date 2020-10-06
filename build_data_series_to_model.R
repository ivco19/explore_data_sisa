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
 ss=split_cases(d_prov)
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

 ##datos sin separar por edad #####################################################################################


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

 conf = acumula_serie(c_prov,tfis,fecha_min,breaks)
 act  = acumula_serie(a_prov,tfis,fecha_min,breaks)
 rec  = acumula_serie(r_prov,trec,fecha_min,breaks)
 fall = acumula_serie(f_prov,tdead,fecha_min,breaks)

 to_fit <- data.frame(
      fecha=as.Date(0:maxf+fecha_min),
      dia = conf$dias,
      confirmados_diarios   = conf$his,
      confirmados_acumulados= conf$cum,
      activos_diarios       = act$his,
      activos_acumulados    = act$cum,
      recuperados_diarios   = rec$his,
      recuperados_acumulados= rec$cum,
      fallecidos_diarios    = fall$his,
      fallecidos_acumulados = fall$cum
 )

 print("dia 220:")
 print(as.Date(220+11+fecha_min)) #11 viene de write_js.R en la calculadora
 #write.csv(to_fit,file="data_to_fit.csv",row.names = FALSE)
 write.csv(to_fit,file="data_to_fit_BM-sin_edad.csv",row.names = FALSE)

 ii=d_prov$FECHA_INTERNACION!=""
 jj=( d_prov$FECHA_ALTA_MEDICA[ii])!=""

 entrada=as.Date(repair_date((d_prov$FECHA_INTERNACION[ii])[jj]),format="%d/%m/%Y")
 salida =as.Date(repair_date((d_prov$FECHA_ALTA_MEDICA[ii])[jj]),format="%d/%m/%Y")

 diff=as.numeric(salida-entrada)
 diff=diff[diff <100]
 print("permanencia media en el hospital:")
 print(mean(diff))


 c=as.Date(c_prov[[tfis]])
 fecha_max=max(c)

 fintern=as.Date(repair_date((d_prov$FECHA_INTERNACION[ii])),format="%d/%m/%Y")
 fmin=min(fintern)

 print("rate de hospitalizacion:")
 print(length(fintern)/as.numeric(fecha_max - fmin)/(conf$his[length(conf$his)]))


 ##################################################################################################################

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

 c_prov=subset(ss[["confirmados"]], EDAD_ACTUAL>=60)
 a_prov=subset(ss[["activos"]], EDAD_ACTUAL>=60)
 r_prov=subset(ss[["recuperados"]], EDAD_ACTUAL>=60)
 f_prov=subset(ss[["fallecidos"]], EDAD_ACTUAL>=60)
     
 conf_myrs = acumula_serie(c_prov,tfis,fecha_min,breaks)
 act_myrs  = acumula_serie(a_prov,tfis,fecha_min,breaks)
 rec_myrs  = acumula_serie(r_prov,trec,fecha_min,breaks)
 fall_myrs = acumula_serie(f_prov,tdead,fecha_min,breaks)

 c_prov=subset(ss[["confirmados"]], EDAD_ACTUAL<60)
 a_prov=subset(ss[["activos"]], EDAD_ACTUAL<60)
 r_prov=subset(ss[["recuperados"]], EDAD_ACTUAL<60)
 f_prov=subset(ss[["fallecidos"]], EDAD_ACTUAL<60)

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


 d_nac=load_data_nac()
 c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado"))

 c=c_nac$F_CASO
 fecha_min=min(c)
 fecha_max=max(c)
 diff=as.numeric(fecha_max-fecha_min)
 breaks=seq(-0.5,diff+0.5,by=1)
 f_nac=subset(d_nac,(fallecido=="SI"))
 
 conf_nac = acumula_serie(c_nac,"F_CASO",fecha_min,breaks)
 fall_nac = acumula_serie(f_nac,"fecha_fallecimiento",fecha_min,breaks)

 to_fit <- data.frame(
      fecha=as.Date(0:diff+fecha_min),
      dia = conf_nac$dias,
      confirmados_dia  = conf_nac$his,
      confirmados_acum  = conf_nac$cum,
      fallecidos_dia   = fall_nac$his,
      fallecidos_acum   = fall_nac$cum
 )
 
 write.csv(to_fit,file="data_to_fit_nacional.csv",row.names = FALSE)

 d_nac=load_data_nac()
 #c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado"))
 c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado" & provincia_carga=="Córdoba"))

 c=c_nac$F_CASO
 fecha_min=min(c)
 fecha_max=max(c)
 diff=as.numeric(fecha_max-fecha_min)
 breaks=seq(-0.5,diff+0.5,by=1)
 f_nac=subset(d_nac,(fallecido=="SI" & provincia_carga=="Córdoba"))
 
 conf_nac = acumula_serie(c_nac,"F_CASO",fecha_min,breaks)
 fall_nac = acumula_serie(f_nac,"fecha_fallecimiento",fecha_min,breaks)

 to_fit <- data.frame(
      fecha=as.Date(0:diff+fecha_min),
      dia = conf_nac$dias,
      confirmados_dia  = conf_nac$his,
      confirmados_acum  = conf_nac$cum,
      fallecidos_dia   = fall_nac$his,
      fallecidos_acum   = fall_nac$cum
 )
 
 write.csv(to_fit,file="data_to_fit_cba_public.csv",row.names = FALSE)

 d_nac=load_data_nac()
 #c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado"))
 c_nac=subset(d_nac,(clasificacion_resumen=="Confirmado" & provincia_carga=="CABA"))

 c=c_nac$F_CASO
 fecha_min=min(c)
 fecha_max=max(c)
 diff=as.numeric(fecha_max-fecha_min)
 breaks=seq(-0.5,diff+0.5,by=1)
 f_nac=subset(d_nac,(fallecido=="SI" & provincia_carga=="CABA"))
 
 conf_nac = acumula_serie(c_nac,"F_CASO",fecha_min,breaks)
 fall_nac = acumula_serie(f_nac,"fecha_fallecimiento",fecha_min,breaks)

 to_fit <- data.frame(
      fecha=as.Date(0:diff+fecha_min),
      dia = conf_nac$dias,
      confirmados_dia  = conf_nac$his,
      confirmados_acum  = conf_nac$cum,
      fallecidos_dia   = fall_nac$his,
      fallecidos_acum   = fall_nac$cum
 )
 
 write.csv(to_fit,file="data_to_fit_caba_public.csv",row.names = FALSE)

