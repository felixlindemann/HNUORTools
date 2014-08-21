

## Definieren der Funktionen
Andler.KL<-function(x,kl,p)
{
  0.5*x*kl*p
}

Andler.KB<-function(x,kb,M)
{
  kb*M/x
}

Andler.KG<-function(x,kb,kl,p,M)
{
  Andler.KL(x,kl,p) + Andler.KB(x,kb,M) 
}

Andler.KG.EV<-function(x,kb,kl,p,M)
{
  Andler.KG(x,kb,kl,p,M) + M*p
}

Andler.bopt<-function(kb,kl,p,M)
{
  sqrt(2*kb*M/kl/p)
}

Andler.Kmin<-function(kb,kl,p,M)
{
  sqrt(2*kb*kl*M*p)
}

## Definieren der Parameter
M<-150000
kb<-700
kl<-0.05
p<-c(7,6,4.5)
Bounds<-c(25000,100000)
x<-seq(0,M*1.1,length=400)


#Ausgabe
# excl.Ev
plot(x,Andler.KG(x,kb,kl,p[1],M),type="l",lty=2,col=1,
     ylim=c(0,1250000),
     xlim=c(0,M),
     xlab="Lotsize [ME]",ylab="Gesamtkosten [€]")
lines(x,Andler.KG(x,kb,kl,p[2],M),lty=2,col=2)
lines(x,Andler.KG(x,kb,kl,p[3],M),lty=2,col=3)

# incl. EV.
lines(x,Andler.KG.EV(x,kb,kl,p[1],M),lty=2,col=1)
lines(x,Andler.KG.EV(x,kb,kl,p[2],M),lty=2,col=2)
lines(x,Andler.KG.EV(x,kb,kl,p[3],M),lty=2,col=3)

# gültige Bereiche:
x<-seq(0,Bounds[1],length=100)
lines(x,Andler.KG(x,kb,kl,p[1],M),lty=1,col=1,lwd=2)
lines(x,Andler.KG.EV(x,kb,kl,p[1],M),col=1,lwd=2)

x<-seq(Bounds[1],Bounds[2],length=100)
lines(x,Andler.KG(x,kb,kl,p[2],M),lty=1,col=2,lwd=2)
lines(x,Andler.KG.EV(x,kb,kl,p[2],M),col=2,lwd=2)

x<-seq(Bounds[2],M,length=100)
lines(x,Andler.KG(x,kb,kl,p[3],M),lty=1,col=3,lwd=2) 
lines(x,Andler.KG.EV(x,kb,kl,p[3],M),col=3,lwd=2)


abline(v=Bounds,lty=1,col=2:3,lwd=2)

b.opt<-Andler.bopt(kb,kl,p,M)
K.Min<-Andler.Kmin(kb,kl,p,M)

abline(v=b.opt,col=1:3,lty=1)
points(b.opt,K.Min,col=1:3,pch=20)

# Ergebnisausgabe in R
b.opt.EV 
K.Min 

