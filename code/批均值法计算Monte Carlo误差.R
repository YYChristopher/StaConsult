mcerror.batch <- function(x,batches){##batch mean批均值法
  iter <- length(x)
  batleng <- ceiling(iter/batches)
  bats <- sort(rep(seq(from=1,to=batches,by=1),batleng))
  bats <- bats[1:iter]
  batmeans <- tapply(x, bats, mean)
  mc.error <- sd(batmeans)/sqrt(batches-1)
  return(mc.error)
}