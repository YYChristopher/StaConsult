#SP500(R)
library(quantmod)
library(TSA)
library(tseries)
library(forecast)
library(fGarch)
library(rugarch)
#数据导入
data<-read.csv("D:/Rsoftware/时间序列与金融统计/非线性模型作业/data/S&P.csv")
d<-ts(data,frequency = 53,start = c(2016))
#数据处理
#figure1:导入收盘价数据
plot(d[,5])
#figure2:转对数收益率
logd<-diff(log(d[,5]))
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
#figure11:GARCH(1,2)
garch.model2<-garchFit(~garch(1,2),data = x,cond.dist='std',trace = F)
summary(garch.model2)
#figure12:GARCH(2,1)
garch.model3<-garchFit(~garch(2,1),data = x,cond.dist='std',trace = F)
summary(garch.model3)
#figure13:GARCH(2,2)
garch.model4<-garchFit(~garch(2,2),data = x,cond.dist='std',trace = F)
summary(garch.model4)
#figure14:GARCH(2,3)
garch.model5<-garchFit(~garch(2,3),data = x,cond.dist='std',trace = F)
summary(garch.model5)
#figure15:GARCH(3,2)
garch.model6<-garchFit(~garch(3,2),data = x,cond.dist='std',trace = F)
summary(garch.model6)
#figure16:GARCH(3,3) figure17:结果中取信息准则
garch.model7<-garchFit(~garch(3,3),data = x,cond.dist='std',trace = F)
summary(garch.model7)
#figure18:ARMA(0,0)取残差建模
arma.model3 <- arima(logd,order = c(0,0,0))
summary(arma.model3)
x0 <- arma.model3$residuals
#figure19:结果取信息准则
garch.model0 <- garchFit(~garch(1,1),data=x0,trace=F)
summary(garch.model0)
#figure20:GARCH(1,1)模型残差检验
r<-residuals(garch.model1,standardize=TRUE)
par(mfrow=c(2,2))
acf(r, lag=24)
pacf(r, lag=24)
acf(r^2, lag=24)
pacf(r^2, lag=24)
#figure23:对数收益率预测区间
r.fit<-garch(x,order = c(1,1),cond.dist='std')
pred.model<-predict(r.fit)
plot(pred.model)
par(mfrow=c(1,1))
plot(x,ylab='对数收益率')
abline(h=0,col="blue")
title('对数收益率曲线图')
par(mfrow=c(1,1))
pred.model.up<-as.vector(pred.model[1:260,1])
pred.model.down<-as.vector(pred.model[1:260,2])
time<-c(1:260)/53+2016
x.num<-as.vector(x)
plot(time,x.num,type='l',ylab='对数收益率')
abline(h=0,col="blue")
lines(time,pred.model.up,col='red')
lines(time,pred.model.down,col='red')
title('GARCH(1,1)预测收益率的区间')
#figure22:对数收益率置信区间
plot(garch.model1,which=2)
#figure24:实际波动率模拟
plot(x^2,col="red",xlab="Time",ylab = "Volatility")
#figure25:波动率预测
plot(garch.model1,which=3)