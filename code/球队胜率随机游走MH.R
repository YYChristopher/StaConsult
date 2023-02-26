path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source('批均值法计算Monte Carlo误差.R')
y_vec <- seq(from = 50, to = 950, by = 10)
N <- 1000
iterations <- 2500
acc.problist <- rep(0,length(y_vec))
sdplist <- rep(0,length(y_vec))
sdthetalist <- rep(0,length(y_vec))
mcerrorplist <- rep(0,length(y_vec))
mcerrorthetalist <- rep(0,length(y_vec))
for(y in y_vec){
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