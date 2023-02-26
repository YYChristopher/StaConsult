N <- 5000
burn <- 1000
X <- matrix(0,N,2)
rho <- -.75
mu1 <- 0
mu2 <- 2
sigma1 <- 1
sigma2 <- .5
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
X[1,] <- c(mu1,mu2)
for(i in 2:N){
  x2 <- X[i-1,2]
  m1 <- mu1+rho*(x2-mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1,m1,s1)
  x1 <- X[i,1]
  m2 <- mu2+rho*(x1-mu1)*sigma2/sigma1
  X[i,2] <- rnorm(1,m2,s2)
}
b <- burn+1
x <- X[b:N,]
colMeans(x)
cov(x)
cor(x)
plot(x,main="",cex=.5,xlab = bquote(X[1]),ylab = bquote(X[2]),ylim = range(x[,2]))