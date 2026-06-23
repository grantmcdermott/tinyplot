data("EuStockMarkets", package = "datasets")

library("tinyplot")

## demos the tinyplot.ts method (#558)
tinyplot(
  EuStockMarkets,
  yaxl = ",",
   main = "EU stock markets performance",
  theme = "clean2"
)
