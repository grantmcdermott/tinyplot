meat = data.frame(
  consumption = c(8.1, 14.6, 23.2, 30.8, 38, 0.7, 1.3, 4, 7.7, 9.2, 1.2, 1.7, 5.8, 10.3, 12.9, 4.5,
    6.8, 17.9, 27.4, 36.8, 28.5, 29.7, 29.6, 29.7, 27.4, 55.5, 49.2, 44.7, 43.8, 37.8, 22.7, 31.1,
    43.8, 50.9, 52.8, 15.2, 18.8, 21.8, 23, 21.6),
  country = factor(rep(1:2, each = 20), labels = c("China", "United States")),
  decade = factor(rep(paste0(197:201, "0s"), 8)),
  type = factor(rep(c(1:4, 1:4), each = 5), labels = c("Pork", "Beef & other", "Poultry", "Fish & seafood"))
)

library("tinyplot")
tinyplot(
  consumption ~ decade | type,
  facet = country ~ 1,
  data = meat,
  type = "barplot",
  grid = "X",
  flip = TRUE,
  legend = list("bottom!", title = NULL),
  theme = list("minimal", palette.qualitative = palette.colors(palette = "Set 3")[c(4, 6, 1, 5)]),
  main = "Per capita meat and fish consumption by decade",
  sub = "Measured in kilograms per person and year",
  xlab = "",
  ylab = "",
  cap = "Source: Food and Agriculture Organization of the United Nations (via OurWorldInData.org)"
)
