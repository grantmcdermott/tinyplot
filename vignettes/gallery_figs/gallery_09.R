library("tinyplot")
tinytheme()
tinyplot(eruptions ~ waiting, data = faithful, type = "lm")
tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.3))
tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.1, side = 2))
tinyplot_add(type = "p")

