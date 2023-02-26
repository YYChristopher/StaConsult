path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source('批均值法计算Monte Carlo误差.R')
y_vec1 <- seq(from = 50, to = 950, by = 10)
y_vec2 <- seq(from = 50, to = 950, by = 10)
N <- 1000
iterations <- 2500
acc.problist1 <- rep(0,length(y_vec))
acc.problist2 <- rep(0,length(y_vec))
sdplist1 <- rep(0,length(y_vec))
sdplist2 <- rep(0,length(y_vec))
sdthetalist1 <- rep(0,length(y_vec))
sdthetalist2 <- rep(0,length(y_vec))
mcerrorplist1 <- rep(0,length(y_vec))
mcerrorplist2 <- rep(0,length(y_vec))
mcerrorthetalist1 <- rep(0,length(y_vec))
mcerrorthetalist2 <- rep(0,length(y_vec))
for(y in y_vec1){
  mu.theta <- 0
  s.theta <- 100
  prop.s <- 0.35
  theta <- numeric(iterations)
  current.theta <- 0
  acc.prob <- 0
  for(t in 1:iterations){
    prop.theta <- rnorm(1,current.theta,prop.s)
    loga <- ((prop.theta*y-N*log(1+exp(prop.theta)))
             -(current.theta*y-N*log(1+exp(current.theta)))
             +dnorm(prop.theta,mu.theta,s.theta,log=TRUE)
             -dnorm(current.theta,mu.theta,s.theta,log = TRUE))
    u <- runif(1)
    u <- log(u)
    if(u<loga){
      current.theta <- prop.theta
      acc.prob <- acc.prob+1
    }
    theta[t] <- current.theta
  }
  p <- exp(theta)/(1+exp(theta))
  acc.problist1[round((y-50)/10)] <- acc.prob
  sdplist1[round((y-50)/10)] <- sd(p)
  sdthetalist1[round((y-50)/10)] <- sd(theta)
  mcerrorplist1[round((y-50)/10)] <- mcerror.batch(p,batches = 50)
  mcerrorthetalist1[round((y-50)/10)] <- mcerror.batch(theta,batches = 50)
}
for(y in y_vec2){
  mu.theta <- 0
  s.theta <- 100
  prop.mu <- log(y/(N-y))
  mle.var <- 1/y+1/(N-y)
  w <- 1/(1+mle.var/s.theta^2)
  prop.s <- sqrt(mle.var*w)
  theta <- numeric(iterations)
  acc.prob <- 0
  current.theta <- 0
  for(t in 1:iterations){
    prop.theta <- rnorm(1,prop.mu,prop.s)
    loga <- ((prop.theta*y-N*log(1+exp(prop.theta)))
             -(current.theta*y-N*log(1+exp(current.theta)))
             +dnorm(prop.theta,mu.theta,s.theta,log = TRUE)
             -dnorm(current.theta,mu.theta,s.theta,log = TRUE)
             +dnorm(current.theta,prop.mu,prop.s,log = TRUE)
             -dnorm(prop.theta,prop.mu,prop.s,log = TRUE))
    u <- runif(1)
    u <- log(u)
    if(u<loga){
      current.theta <- prop.theta
      acc.prob <- acc.prob+1
    }
    theta[t] <- current.theta
  }
  p <- exp(theta)/(1+exp(theta))
  acc.problist2[round((y-50)/10)] <- acc.prob
  sdplist2[round((y-50)/10)] <- sd(p)
  sdthetalist2[round((y-50)/10)] <- sd(theta)
  mcerrorplist2[round((y-50)/10)] <- mcerror.batch(p,batches = 50)
  mcerrorthetalist2[round((y-50)/10)] <- mcerror.batch(theta,batches = 50)
}
plot(y_vec1,acc.problist1,type="l",xlab = "原假定的y值（胜场值）",ylab = "抽样接受次数",col="red")
lines(y_vec2,acc.problist2,type="l",xlab = "原假定的y值（胜场值）",ylab = "抽样接受次数",col="blue")
plot(y_vec1,sdplist1,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟胜率标准差",col="red")
lines(y_vec2,sdplist2,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟胜率标准差",col="blue")
plot(y_vec1,sdthetalist1,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟theta标准差",col="red")
lines(y_vec2,sdthetalist2,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟theta标准差",col="blue")
plot(y_vec1,mcerrorplist1,type="l",xlab = "原假定的y值（胜场值）",ylab = "胜率的MC误差",col="red")
lines(y_vec2,mcerrorplist2,type="l",xlab = "原假定的y值（胜场值）",ylab = "胜率的MC误差",col="blue")
plot(y_vec1,mcerrorthetalist1,type="l",xlab = "原假定的y值（胜场值）",ylab = "theta的MC误差",col="red")
lines(y_vec2,mcerrorthetalist2,type="l",xlab = "原假定的y值（胜场值）",ylab = "theta的MC误差",col="blue")