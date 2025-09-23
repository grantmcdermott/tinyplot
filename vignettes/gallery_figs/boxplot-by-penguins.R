library("tinyplot")

plt(
  body_mass ~ species | sex, data = penguins,
  type = type_boxplot(boxwex = 0.6, staplewex = 0, outline = FALSE),
  lty = 1,
  flip = TRUE, yaxl = ",",
  grid = TRUE
)
