source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(Sepal.Width ~ Sepal.Length | Species,
           facet = ~Species,
           data = iris, 
           type = type_lm())
  tinyplot_add(type = "p")
}
expect_snapshot_plot(f, label = "tinyplot_add")
