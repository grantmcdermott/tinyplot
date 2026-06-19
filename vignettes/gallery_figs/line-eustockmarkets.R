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
  facet = "by",
  type = "l",
  theme = "clean2",
  legend = FALSE,
  xlab = "",
  ylab = ""
)
