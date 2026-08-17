library(tinyplot)

m = as.matrix(mtcars)

plt(
    m,
    type = type_heatmap(scale = "x"),
    col = "white",
    theme = list("heatmap", cex.yaxs = 0.75, cex.xaxs = 1.25)
)
