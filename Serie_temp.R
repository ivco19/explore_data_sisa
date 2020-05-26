do_pl <- function()
{
source('funciones_NG.R')
datos=prepara_datos('bm_20.05.15-V2.csv','BM')
rec=recuperados(datos,verb=FALSE)
fal=fallecidos(datos,verb=FALSE)
tod=todos(datos,verb=FALSE)

f0=min(rec$fis,fal$fis)
f1=max(rec$fis,fal$fis)
Ndias=6
NN=as.numeric(f1-f0)
Nbins=NN/Ndias
dates=as.Date(c(1:Nbins),'1/1/2020',format='%d/%m/%Y')
Ninf=c(1:Nbins)
Nrec=c(1:Nbins)
Nmue=c(1:Nbins)
for (i in c(1:Nbins)){
   tbin0=f0+(i-1)*Ndias
   tbin1=f0+(i)*Ndias
   dates[i]=tbin0+as.integer(Ndias/2)
   print(c(tbin0,tbin1))
   
   w0=which(rec$alta > tbin0)
   w1=which(rec$alta[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nrec[i]=length(w)
   
   w0=which(fal$mue > tbin0)
   w1=which(fal$mue[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nmue[i]=length(w)
   
   w0=which(tod$FisInf > tbin0)
   w1=which(tod$FisInf[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Ninf[i]=length(w)
   
}

gam=Nrec/Ninf
mu =Nmue/Ninf
print(dates)
print(gam)
print(mu)

return(data.frame(d = dates, mu = mu, gam =gam))
}
