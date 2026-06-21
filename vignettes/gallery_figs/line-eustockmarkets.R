## note: replace with tinyplot.ts method once #558 is merged

data("EuStockMarkets", package = "datasets")

eu = data.frame(
  time = rep(time(EuStockMarkets), ncol(EuStockMarkets)),
  market = rep(colnames(EuStockMarkets), each = nrow(EuStockMarkets)),
  value = as.numeric(EuStockMarkets)
)

library("tinyplot")
tinyplot(
  value ~ time | market,
  data = eu,
  facet = "by", legend = FALSE,
  type = "l",
  theme = "clean2",
  main = "EU stock markets performance",
  xlab = NA, ylab = NA,
  yaxl = ","
)
