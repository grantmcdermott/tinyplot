library("tinyplot")
tinytheme("ridge2")

aq = transform(
  airquality,
  Month = factor(month.abb[Month], levels = month.abb[5:9]),
  Month2 = factor(month.name[Month], levels = month.name[5:9]),
  Late = ifelse(Day > 15, "Late", "Early")
)
  
tinyplot(Month ~ Temp | Temp,
  data = aq,
  type = type_ridge(col = "white")
)
