library(poweRlaw)
library(ineq)

Lp = function(p,alpha) {1-(1-p)^((alpha-1)/alpha)}
p = seq(0,1,0.01)
plot(p,p,type="l", xlab="", ylab="", main="Pareto Katsayýlarý ve Lorenz Eðrileri")
lines(p,Lp(p,1.2),col=2)
lines(p,Lp(p,1.6),col=3)
lines(p,Lp(p,2.2),col=4)
lines(p,Lp(p,3.2),col=5)
text(0.8,0.15,"alpha=1.2",col=2)
text(0.8,0.35,"alpha=1.6",col=3)
text(0.8,0.48,"alpha=2.2",col=4)
text(0.8,0.58,"alpha=3.2",col=5)

####
library(ineq)
x = seq(0,4,0.01)
y1 = dlnorm(x,meanlog=0,sdlog=0.25)
y2 = dlnorm(x,meanlog=0,sdlog=1.0)
plot(x,y1,type="l")
lines(x,y2,type="l",col="red")
text(1.8,1,"sigma = 0.25")
text(3,0.20,"sigma = 1")

library(ineq)
p = seq(0,1,0.01)
plot(p,Lc.lognorm(p, parameter=0.25),type="l",col="brown")
lines(p,Lc.lognorm(p, parameter=0.5),col="red")
lines(p,Lc.lognorm(p, parameter=1.0),col="blue")
lines(p,Lc.lognorm(p, parameter=1.5),col="green")
lines(p,p)
# text(0.42,0.5,"45degree line")
text(0.8,0.68,"0.25")
text(0.8,0.58,"0.50")
text(0.8,0.40,"1.00")
text(0.8,0.20,"1.50")


#########
LCgen <- function(p,alpha,beta){
  smlc <- (1-(1-p)^(1-1/alpha))^beta
  smlc}
p = seq(0,1,0.01)
plot(p,LCgen(p, 1.5,1),type="l")
text(0.93,0.45,"1.5, 1.0")
lines(p,LCgen(p,3,1.5),type="l",col="red")
text(0.7,0.50,"3.0, 1.5")
lines(p,LCgen(p,4,2),type="l",col="blue")
text(0.5,0.10,"4.0, 2.0")
lines(p,p)
text(0.42,0.5,"45 degree line")