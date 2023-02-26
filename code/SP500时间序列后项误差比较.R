library(bsts)
path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source("Makeplots函数.R")
#数据导入
data<-read.csv("D:/Rsoftware/时间序列与金融统计/非线性模型作业/data/S&P.csv")
d<-ts(data,frequency = 53,start = c(2016))
#数据处理
#figure1:导入收盘价数据
plot(d[,5])
x <- d[,5]
#figure2:转对数收益率
logd<-diff(log(d[,5]))
plot(logd,col="red",xlab="时间",ylab="对数收益率")
y_ori <- logd
y_range <- as.vector(seq(from = 145, to=245, by = 1))
pre_error1 <- rep(0,length(y_range))
pre_error2 <- rep(0,length(y_range))
for(i in y_range){
  y <- y_ori[1:i]
  ss <- AddLocalLinearTrend(list(), y)
  model <- bsts(y, state.specification = ss, niter = 500)
  pred <- predict(model, horizon = 12, burn = 100)
  error <- dist(rbind(pred$mean,y_ori[i+1:i+12]),method = "euclidean")/12
  pre_error1[i-144] <- error
}
pre_error1
for(i in y_range){
  y <- y_ori[1:i]
  pre <- forecast(y,12)
  error <- dist(rbind(pre$mean,y_ori[i+1:i+12]),method = "euclidean")/12
  pre_error2[i-144] <- error
}
pre_error2
plot(y_range,pre_error1,xlab="时间变动范围",ylab="horizon=12预测误差",col="red",type="l")
lines(y_range,pre_error2,xlab="时间变动范围",ylab="horizon=12预测误差",col="blue",type="l")