MakePlots <- function(model, ask = TRUE) {
  ## Make all the plots callable by plot.bsts.
  opar <- par(ask = ask)
  on.exit(par(opar))
  plot.types <- c("state", "components", "residuals",
                  "prediction.errors", "forecast.distribution")
  for (plot.type in plot.types) {
    plot(model, plot.type,main = plot.type)
  }
  if (model$has.regression) {
    regression.plot.types <- c("coefficients", "predictors", "size")
    for (plot.type in regression.plot.types) {
      plot(model, plot.type)
    }
  }
}