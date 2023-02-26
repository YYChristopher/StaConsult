sdq <- seq(from=0.1,to = 10,by= 0.1)
meanres <- rep(0,length(sdq))
sdres <- rep(0,length(sdq))
meanmse <- rep(0,length(sdq))
sdmse <- rep(0,length(sdq))
M <- 100

for(i in 1:length(sdq)){
  y <- rnorm(100,mean = 100,sd=sdq[i])
  bary <- mean(y)
  iterations <- 10000
  mu_0 <- 0
  s_0 <- 10
  a_0 <- 0.001
  b_0 <- 0.001
  theta <- matrix(nrow = iterations,ncol = 2)
  cur.mu <- 0
  cur.tau <- 2
  cur.s <- sqrt(1/cur.tau)
  for(t in 1:iterations){
    w <- s_0^2/(cur.s^2/n+s_0^2)
    m <- w*bary+(1-w)*mu_0
    s <- sqrt(w/n)*cur.s
    cur.mu <- rnorm(1,m,s)
    a <- a_0+0.5*n
    b <- b_0+0.5*sum((y-cur.mu)^2)
    cur.tau <- rgamma(1,a,b)
    cur.s <- sqrt(1/cur.tau)
    theta[t,] <- c(cur.mu,cur.s)
  }
  meanres[i] <- mean(theta[,1])
  sdres[i] <- mean(theta[,2])
}

for(j in 1:length(meanres)){
  meanmse[j] <- (meanres[j]-M)^2
  sdmse[j] <- (sdres[j]-sdq[j])^2
}

plot(sdq,sdmse,col=2,type = "l")
plot(sdq,meanmse,col=2,type = "l")