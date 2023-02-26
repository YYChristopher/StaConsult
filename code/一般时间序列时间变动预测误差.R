#SP500(R)
library(quantmod)
library(TSA)
library(tseries)
library(forecast)
library(fGarch)
library(rugarch)
library(forecast)
#数据导入
data<-read.csv("D:/Rsoftware/时间序列与金融统计/非线性模型作业/data/S&P.csv")
d<-ts(data,frequency = 53,start = c(2016))
#数据处理
#figure1:导入收盘价数据
plot(d[,5])
#figure2:转对数收益率
logd<-diff(log(d[,5]))
x_ori <- logd
plot(logd,col="red",xlab="时间",ylab="对数收益率")
abline(h=0,col="blue")
title("S&P500对数收益率曲线图")
#figure3:单位根稳定性检验
adf.test(logd)
#figure4:ARMA定阶
a<-acf(logd,plot = T)
pa<-pacf(logd,plot = T)
#figure5:eacf定阶
eacf(logd)
x<-logd
#figure6:GARCH效应(对数收益率相关性)检验
Box.test(x^2,lag = 12,type = 'Ljung-Box')
#GARCH模型拟合
garch.model<-garch(x,order = c(1,1))
summary(garch.model)
r<-residuals(garch.model,standardize=TRUE)
#标准残差正态性QQ图检验
plot(r,type='h',ylab='标准化残差')
title('拟合GARCH(1,1)模型残差')
#figure7:对数收益率作为残差的QQ图
qqnorm(x);qqline(x)
#figure8:标准残差正态性统计量检验
shapiro.test(r)
jarqueberaTest(r)
#figure9:对数收益率平方定阶
a<-acf((logd)^2,plot = T)
pa<-pacf((logd)^2,plot = T)
#标准残差服从t分布
#figure10:GARCH(1,1) figure21:取其中残差检验
garch.model1<-garchFit(~garch(1,1),data = x,cond.dist='std',trace = F)
summary(garch.model1)
pre <- forecast(x,12)
x_range <- as.vector(seq(from = 145, to=245, by = 1))
pre_error <- rep(0,length(x_range))
for(i in x_range){
  x <- x_ori[1:i]
  pre <- forecast(x,12)
  error <- dist(rbind(pre$mean,x_ori[i+1:i+12]),method = "euclidean")/12
  pre_error[i-144] <- error
}
pre_error
plot(x_range,pre_error,xlab="时间变动范围",ylab="horizon=12预测误差",col="red",type="l")