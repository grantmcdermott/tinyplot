library("tinyplot")

tinyplot(
    x = 1:9, 
    y = c(2,1,2,1,NA,2,1,2,1), 
    type = type_polygon(density = c(10, 20)))
