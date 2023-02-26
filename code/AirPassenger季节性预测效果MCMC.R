data(AirPassengers)
y_ori <- log(AirPassengers)
y <- log(AirPassengers)[1:132]
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
model <- bsts(y, state.specification = ss, niter = 500)
pred <- predict(model, horizon = 12, burn = 100)
plot(model)
plot(pred)
pred$mean
y_ori[133:144]