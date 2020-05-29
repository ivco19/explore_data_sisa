do_pl <- function(sset)
{
if(missing(sset)) {
sset = 0
print('Recordar args: 0 todos, 1 < 60 años, 2 > 60 años')
}

source('funciones_NG.R')
datos=prepara_datos('BM.csv','BM')
rec=recuperados(datos,verb=FALSE)
fal=fallecidos(datos,verb=FALSE)
tod=todos(datos,verb=FALSE)
##Cuidado esto no esta en las definiciones
if(sset == 0){
  alta=rec$alta
  fmue=fal$mue
  finf=tod$FInf
}else if(sset == 1){
  print("Estadistica de < 60")
  w=which(rec$Edades < 60)
  alta=rec$alta[w]
  w=which(fal$Edades < 60)
  fmue=fal$mue[w]
  w=which(tod$EdadInf < 60)
  finf=tod$FInf[w]
} else if (sset == 2)
{
  print("Estadistica de >= 60")
  w=which(rec$Edades >= 60)
  alta=rec$alta[w]
  w=which(fal$Edades >= 60)
  fmue=fal$mue[w]
  w=which(tod$EdadInf >= 60)
  finf=tod$FInf[w]
}

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
   
   w0=which(alta >= tbin0)
   w1=which(alta[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nrec[i]=length(w)
   
   w0=which(fmue >= tbin0)
   w1=which(fmue[w0] < tbin1)
   w=w0[w1] #   print(length(w))
   Nmue[i]=length(w)
   
   w0=which(finf >= tbin0)
   w1=which(finf[w0] < tbin1)
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
