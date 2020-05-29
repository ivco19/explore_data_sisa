do_pl <- function()
{
source('funciones_NG.R')
datos=prepara_datos('BM.csv','BM')
rec=recuperados(datos,verb=FALSE)
fal=fallecidos(datos,verb=FALSE)
tod=todos(datos,verb=FALSE)

f0=min(tod$FInf)
f1=max(tod$FInf)
Ndias=6
NN=as.numeric(f1-f0)
Nbins=ceiling(NN/Ndias)
dates=as.Date(c(1:Nbins),'1/1/2020',format='%d/%m/%Y')
Ninf=c(1:Nbins)*0
Nrec=c(1:Nbins)*0
Nmue=c(1:Nbins)*0
dd=c(1:Nbins)*Ndias

for (i in c(1:Nbins)){
   tbin0=f0+(i-1)*Ndias
   tbin1=f0+(i)*Ndias
   dates[i]=tbin0+as.integer(Ndias/2)
   print(c(tbin0,tbin1))
   
   w0=which(rec$alta >= tbin0)
   w1=which(rec$alta[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nrec[i]=length(w)
   
   w0=which(fal$mue >= tbin0)
   w1=which(fal$mue[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nmue[i]=length(w)
   
   w0=which(tod$FInf >= tbin0)
   w1=which(tod$FInf[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Ninf[i]=length(w)
   
}
crec=cumsum(Nrec)
cinf=cumsum(Ninf)
cmue=cumsum(Nmue)
Npob=3.5E6
gam=crec/cinf
mu =cmue/cinf
a=1./5.
bet=Ninf/Ndias*a
bet=cinf*Npob/(Npob-cinf-crec-cmue)/dd*a
bet=cumsum(Ninf)/dd/Ndias
#print(Nmue)
#print(cumsum(Nmue))
#print(Nrec)
#print(cumsum(Nrec))
#print(Ninf)
#print(cumsum(Ninf))
#R0 = a*bet*No/((mu+a)(mu+gam))
print('################## R0 R0')
print(bet*a/((a+mu)*(gam+mu)))
print('##################')
print(dates)
print('################## Betta')
print(bet)
print('################## Gam')
print(gam)
print('################## Mu ')
print(mu)

return(data.frame(d = dates, mu = mu, gam =gam, bet=bet))
}
