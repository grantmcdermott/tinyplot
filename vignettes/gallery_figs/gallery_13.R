library("tinyplot")

tinyplot(mpg ~ hp | factor(cyl),
  data = mtcars,
  type = type_text(
    labels = row.names(mtcars),
    font = 1,
    adj = 0))
