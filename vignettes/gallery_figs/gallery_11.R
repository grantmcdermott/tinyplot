library("tinyplot")
tinytheme("tufte")

tinyplot(
  len ~ dose | supp, data = ToothGrowth, lty = 1,
  flip = TRUE,
  type = type_boxplot(boxwex = 0.6, staplewex = 0, outline = FALSE)
)

tinytheme() # reset theme (optional)