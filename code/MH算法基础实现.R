a=2.7
b=6.3
Nsim <- 5000#Ä£Äâ´ÎÊý
X=rep(runif(1),Nsim)
for(i in 2:Nsim){
  Y=runif(1)
  rho <- dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i] <- X[i-1]+(Y-X[i-1])*(runif(1)<rho)
}
ks.test(jitter(X),rbeta(5000,a,b))
ks.test(X,rbeta(5000,a,b)) 