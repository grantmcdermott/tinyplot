library("tinyplot")
tinyplot(eruptions ~ waiting, data = faithful, type = "lm",
         theme = "tufte", palette = c("darkcyan"))
tinyplot_add(type = "p", pch = 21, col = "white", fill = 0.3, cex = 1.5)
tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.3))
tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.1, side = 2))
