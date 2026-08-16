library(tinyplot)

m = as.matrix(mtcars)

plt(
    m,
    type = type_heatmap(scale = "x"),
    main = "mtcars heatmap",
    cap = "Note: Values are normalised within each x-axis category",
    col = "white",
    theme = list("heatmap", cex.yaxs = 0.5, cex.xaxs = 1.5)
)
