library(tinyplot)

aq = airquality
aq$Month = factor(month.name[aq$Month], levels = month.name[5:9])

plt(
  Temp ~ Day | Month, aq, facet = "by", lwd = 3, type = "l",
  legend = FALSE, 
  draw = plt_add(col = "grey", facet = NULL, lwd = 1),
  main = "Temperatures by month",
  sub = "Sometimes, spaghetti plots are the right choice",
  theme = "float", ylim = c(50, 100), frame = FALSE
)
