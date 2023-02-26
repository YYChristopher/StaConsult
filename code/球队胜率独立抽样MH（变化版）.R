path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source('批均值法计算Monte Carlo误差.R')
y_vec <- seq(from = 50, to = 950, by = 10)
acc.problist <- rep(0,length(y_vec))
sdplist <- rep(0,length(y_vec))
sdthetalist <- rep(0,length(y_vec))
mcerrorplist <- rep(0,length(y_vec))
mcerrorthetalist <- rep(0,length(y_vec))
N <- 1000
iterations <- 1500
for(y in y_vec){
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
  acc.problist[round((y-50)/10)] <- acc.prob
  sdplist[round((y-50)/10)] <- sd(p)
  sdthetalist[round((y-50)/10)] <- sd(theta)
  mcerrorplist[round((y-50)/10)] <- mcerror.batch(p,batches = 50)
  mcerrorthetalist[round((y-50)/10)] <- mcerror.batch(theta,batches = 50)
}
plot(y_vec,acc.problist,type="l",xlab = "原假定的y值（胜场值）",ylab = "抽样接受次数")
plot(y_vec,sdplist,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟胜率标准差")
plot(y_vec,sdthetalist,type="l",xlab = "原假定的y值（胜场值）",ylab = "模拟theta标准差")
plot(y_vec,mcerrorplist,type="l",xlab = "原假定的y值（胜场值）",ylab = "胜率的MC误差")
plot(y_vec,mcerrorthetalist,type="l",xlab = "原假定的y值（胜场值）",ylab = "theta的MC误差")