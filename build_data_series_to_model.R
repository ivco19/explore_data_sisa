source("funciones.R")

dd=load_data()
d_nac=dd[["nac"]]
d_prov=dd[["prov"]]

#para la base de datos nacional vamos a ver recuperados usando
#clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA"
# "fecha_fallecimiento" "fallecido"
#para la base de datos de cordoba vamos a ver recuperados usando
#d_prov$CURADO=="SI" o "FECHA_ALTA_MEDICA" o "FECHA_FALLECIMIENTO"

clasif=c(
 "Caso confirmado - No Activo por criterio de laboratorio"            ,# [1]
 "Caso confirmado - No activo (por tiempo de evolución)"              ,# [2]
 "Caso confirmado - Activo "                                          ,# [3]
 "Caso confirmado - Activo Internado"                                 ,# [4]
 "Caso confirmado - Fallecido"                                        ,# [5]
 "Caso confirmado - No activo (por laboratorio y tiempo de evolución)")# [6]

fecha_fis
FECHA_INTERNACION
FECHA_FALLECIMIENTO
#FECHA_MOD_CLASIF
FECHA_MOD_DIAG

 rec_nac=subset(d_nac,
		clasificacion_manual=="Caso confirmado con criterio laboratorial para ALTA" & 
		clasificacion_resumen=="Confirmado" & 
		provincia_carga=="Córdoba")
 c_prov=subset(d_prov,clasificacion_resumen=="Confirmado")
 r_prov=subset(d_prov,
		   (CLASIFICACION==clasif[1] |
		   CLASIFICACION==clasif[2] |
		   CLASIFICACION==clasif[6])
                )
 a_prov=subset(d_prov,
		    CLASIFICACION==clasif[3] |
		    CLASIFICACION==clasif[4] )

 f_prov=subset(d_prov,
		    CLASIFICACION==clasif[5] )



 c=as.Date(c_prov$fecha_fis)
 fecha_min=min(c)
 plot_casos(a_prov,fecha_min,title="Acumulado Córdoba")


		
