

# tinyplot <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/tinyplot.png)](https://CRAN.R-project.org/package=tinyplot)
<a href="https://grantmcdermott.r-universe.dev"><img src="https://grantmcdermott.r-universe.dev/badges/tinyplot" class="img-fluid" alt="R-universe status badge"></a>
[![R-CMD-check](https://github.com/grantmcdermott/tinyplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grantmcdermott/tinyplot/actions/workflows/R-CMD-check.yaml)
<a href = "https://github.com/grantmcdermott/tinyplot/blob/main/LICENSE.md" target = "_blank"><img src="https://img.shields.io/badge/license-Apache2.0-blue"></a>
[![Docs](https://img.shields.io/badge/docs-homepage-blue.svg)](https://grantmcdermott.com/tinyplot/index.html)
<!-- badges: end -->

## What

A lightweight extension of the base R graphics system, with support for
automatic grouping, legends, facets, and various other enhancements.

**tinyplot** is not yet on CRAN, but can be installed from R-universe.

``` r
install.packages("tinyplot", repos = "https://grantmcdermott.r-universe.dev")
```

Our goal is to submit to CRAN within the first few months of 2024, once
we have settled on some remaining design choices and features support.
You can take a look at the [open
issues](https://github.com/grantmcdermott/tinyplot/issues) to see what’s
currently under consideration. Please feel free to weigh on these if you
have opinions. We want end users to have a say in determining the final
product.

## Why

R users are spoiled for choice when it comes to visualization
frameworks. The options include **ggplot2** (arguably the most important
graphics system of the last decade) and **lattice**, not to mention a
bewildering array of extensions built on top of, around, and in between
these amazing packages.

As a result, it is perhaps not surprising that the base R graphics
system sometimes gets short shrift. This is unfortunate, because base R
offers very powerful and flexible plotting facilities. Just type
`demo(graphics)` or `demo(persp)` into your R console to get an idea.
Or, take a look at
[these](https://github.com/karoliskoncevicius/tutorial_r_introduction/blob/main/baseplotting.md)
[two](https://quizzical-engelbart-d15a44.netlify.app/2021-2022_m2-data-2_visu-2_practice#1)
excellent tutorials. The downside of this power and flexibility is that
base R plotting can require a fair bit of manual tinkering. A case in
point is plotting grouped data with an appropriate legend. Doing so with
the generic `plot()` function can require several function calls or a
loop, fiddling with your plot regions, and then generating the legend
manually.

The **tinyplot** package aims to remove this overhead. It provides a
lightweight (zero dependency) extension of the base R graphics system
with various convenience features, particularly for representing grouped
data. For example, the core `tinyplot()` function—or its shorthand alias
`plt()`—makes it easy to plot different groups of a dataset in a single
function call and highlight these groups using modern colour palettes.
Coincident with this grouping support, **tinyplot** also produces
automatic legends with scope for further customization. While the
package offers several other enhancements like facets, it tries as far
as possible to be a drop-in replacement for the equivalent base plotting
function. Users should generally be able to swap a valid `plot()` call
for `tinyplot()` without any changes to the expected output.

## Quickstart

The **tinyplot** website includes a detailed [introductory
tutorial](https://grantmcdermott.com/tinyplot/vignettes/intro_tutorial.html),
with numerous examples. But here are some quickstart examples of the
package in action.

``` r
library(tinyplot)
```

Grouped scatterplot with automatic legend:

``` r
# with(iris, tinyplot(x = Petal.Length, y = Sepal.Length, by = Species)) # atomic
tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris)             # formula
```

<img src="man/figures/README-quickstart2-1.png" style="width:70.0%" />

If you would prefer to save on a few keystrokes, you can use the
shorthand `plt()` alias instead instead of typing out `tinyplot()` in
full. Here’s the same plot with this shorthand alias, plus a few
aesthetic tweaks:

``` r
plt(
  Sepal.Length ~ Petal.Length | Species, 
  data = iris,
  palette = "dark", pch  = 16,
  grid = TRUE, frame = FALSE
)
```

<img src="man/figures/README-quickstart3-1.png" style="width:70.0%" />

Grouped grouped density plot with automatic legend:

``` r
plt(
  ~ Petal.Length | Species,
  data = iris,
  type = "density",
  palette = "dark", fill = "by",
  grid = TRUE,
  main = "Distribution of petal lengths by species"
)
```

<img src="man/figures/README-quickstart4-1.png" style="width:70.0%" />

Grouped scatterplot with (continuous) gradient legend, combined with
facet layout:

``` r
plt(
  Sepal.Length ~ Petal.Length | Sepal.Length, data = iris,
  facet = ~Species, facet.args = list(bg = "grey90"),
  pch = 19,
  main = "Faceted Species!",
  grid = TRUE, frame = FALSE
)
```

<img src="man/figures/README-quickstart5-1.png" style="width:70.0%" />

Hopefully, these have been enough to pique your interest. Head over to
the [intro
tutorial](https://grantmcdermott.com/tinyplot/vignettes/intro_tutorial.html)
for many more examples, including range plots and customization.
