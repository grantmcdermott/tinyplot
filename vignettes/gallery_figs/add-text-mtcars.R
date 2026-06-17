library("tinyplot")

plt(
  mpg ~ hp | factor(cyl),
  data = mtcars,
  legend = list("topright!", bty = "o", title = "Cyclinders"),
  xlab = "Gross horsepower",
  ylab = "Miles per gallon",
  theme = "classic"
)
plt_add(type = type_text(
  labels = row.names(mtcars),
  adj = -0.05,
  srt = 15,
  xpd = NA,
  repel = TRUE
))
