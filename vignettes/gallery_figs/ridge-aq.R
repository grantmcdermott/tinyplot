library("tinyplot")

aq = transform(
  airquality,
  Month = factor(month.abb[Month], levels = month.abb[5:9]),
  Month2 = factor(month.name[Month], levels = month.name[5:9]),
  Late = ifelse(Day > 15, "Late", "Early")
)
  
tinyplot(
  Month ~ Temp,
  data = aq,
  type = type_ridge(col = "white", gradient = TRUE),
  theme = "ridge2"
)
