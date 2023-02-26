data(goog)
d<-ts(goog,frequency = 260,start = c(2007))
y_ori <- d
ss <- AddSemilocalLinearTrend(list(), d)
model <- bsts(goog, state.specification = ss, niter = 500)
pred <- predict(model, horizon = 12, burn = 100)
y_range <- as.vector(seq(from = 800, to=1000, by = 1))
pre_error1 <- rep(0,length(y_range))
pre_error2 <- rep(0,length(y_range))
for(i in y_range){
  y <- y_ori[1:i]
  ss <- AddLocalLinearTrend(list(), y)
  model <- bsts(y, state.specification = ss, niter = 500)
  pred <- predict(model, horizon = 12, burn = 100)
  error <- dist(rbind(pred$mean,y_ori[i+1:i+12]),method = "euclidean")/12
  pre_error1[i-799] <- error
}
pre_error1
for(i in y_range){
  y <- y_ori[1:i]
  pre <- forecast(y,12)
  error <- dist(rbind(pre$mean,y_ori[i+1:i+12]),method = "euclidean")/12
  pre_error2[i-799] <- error
}
pre_error2
plot(y_range,pre_error1,xlab="时间变动范围",ylab="horizon=12预测误差",col="red",type="l")
lines(y_range,pre_error2,xlab="时间变动范围",ylab="horizon=12预测误差",col="blue",type="l")