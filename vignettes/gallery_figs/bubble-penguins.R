library(tinyplot)

with(
  na.omit(penguins),
  plt(
    y = bill_len, x = bill_dep, by = sex,
    facet = species,
    cex = body_mass, clim = c(0.5, 4.5),
    col = 'white', fill = 0.5, pch = 21,
    legend = list(labeller = ","),
    type = "p",
    theme = "clean2"
  )
)
