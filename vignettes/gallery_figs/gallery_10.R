library("tinyplot")
tinytheme("bw")
tinyplot(
    ~Petal.Width | Species,
    type = "histogram",
    data = iris
)