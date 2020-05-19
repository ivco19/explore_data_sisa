#leemos los datos
datos=prepara_datos()

rec=recuperados(datos)
fal=fallecidos(datos)

e0=which(rec$Edades  < 60)
e1=which(rec$Edades >= 60)

h0=hist(rec$D_int[e0])
h1=hist(rec$D_int[e1])
h2=hist(fal$D_int)

pdf('histogramas.pdf')
plot( h0, col=rgb(0,0,1,1/4), main='Histograma de dias de internacion de recuperados',xlab="Nro de Dias") 
plot( h1, col=rgb(1,0,0,1/4), add=T)
legend("topleft",c('<60','>=60'),fill=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))
plot( h2, col=rgb(0,0,1,1/4), main='Histograma de dias de internacion de fallecidos',xlab="Nro de Dias") 
legend("topleft",c('>=60'),fill=c(rgb(0,0,1,1/4)))
dev.off()
