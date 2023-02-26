n <- 5000
burn.in <- 2500
X <- matrix(0,n,2)
mu1 <- 1
mu2 <- -1
sigma1 <- 1
sigma2 <- 2
rho <- .5
s1.c <- sqrt(1-rho^2)*sigma1
s2.c <- sqrt(1-rho^2)*sigma2
X[1,] <- c(mu1,mu2)
for(i in 2:n){
  x2 <- X[i-1,2]
  m1.c <- mu1+rho*(x2-mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1,m1.c,s1.c)
  x1 <- X[i,1]
  m2.c <- mu2+rho*(x1-mu1)*sigma2/sigma1
  X[i,2] <- rnorm(1,m2.c,s2.c)
}
b <- burn.in+1
x.mcmc <- X[b:n,]
library(MASS)
MVN.kdensity <- kde2d(x.mcmc[,1],x.mcmc[,2],h=5)
plot(x.mcmc,col="blue",xlab="X1",ylab="X2")
contour(MVN.kdensity,add=TRUE)
hist(x.mcmc[,1])
library(stats)
acf(x.mcmc[,1])
index <- 1:n
plot(index,X[,1],type="l")