source("helpers.R")
using("tinysnapshot")

## error message cannot be tested in this suite
# expect_error(tinyplot_add(type = "p"), pattern = "No previous tinyplot")

f = function() {
  tinyplot(Sepal.Width ~ Sepal.Length | Species,
           facet = ~Species,
           data = iris, 
           type = "p")
  tinyplot_add(type = type_lm())
}
expect_snapshot_plot(f, label = "tinyplot_add")
