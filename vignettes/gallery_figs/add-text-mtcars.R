library("tinyplot")

plt(
  mpg ~ hp | factor(cyl),
  data = mtcars,
  legend = list("topright!", bty = "o"),
  theme =  "classic"
)
plt_add(type = type_text(
  labels = row.names(mtcars),
  adj = -0.05,
  srt = 15,
  xpd = NA
))
