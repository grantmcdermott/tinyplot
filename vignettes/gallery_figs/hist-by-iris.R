library("tinyplot")

tinyplot(
    ~Petal.Length | Species,
    data = iris,
    type = "histogram",
    breaks = 30,
    main = "Histogram of petal lengths",
    sub = "Grouped by species",
    theme = "bw"
)