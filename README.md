
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plot2

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/plot2)](https://CRAN.R-project.org/package=plot2)
[![R-universe status
badge](https://grantmcdermott.r-universe.dev/badges/plot2)](https://grantmcdermott.r-universe.dev)
[![R-CMD-check](https://github.com/grantmcdermott/plot2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grantmcdermott/plot2/actions/workflows/R-CMD-check.yaml)
[![Docs](https://img.shields.io/badge/docs-homepage-blue.svg)](https://grantmcdermott.com/plot2/index.html)
<!-- badges: end -->

## What

A lightweight extension of the base R graphics system, with support for
automatic grouping, legends, facets and various other enhancements.

**plot2** is not yet on CRAN, but can be installed from R-universe.

``` r
install.packages("plot2", repos = "https://grantmcdermott.r-universe.dev")
```

Our goal is to submit to CRAN within the first few months of 2024, once
we have settled on some remaining design choices and features support.
You can take a look at the [open
issues](https://github.com/grantmcdermott/plot2/issues) to see what’s
currently under consideration. Please feel free to weigh on these if you
have opinions. We want end users to have a say in determining the final
product.

## Why

R users are spoiled for choice when it comes to visualization
frameworks. The options include **ggplot2** (arguably the most important
graphics system of the last decade) and **lattice**, not to mention a
bewildering array of extensions built around, on top of, and in between
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

The **plot2** package aims to remove this overhead. It provides a
lightweight (zero dependency) extension of the base R graphics system
with various convenience features, particularly for representing groups
with your data. For example, the core `plot2()` function makes it easy
to plot different categories of a dataset in a single function call and
highlight these categories (groups) using modern colour palettes.
Coincident with this grouping support, `plot2()` also produces automatic
legends with scope for further customization. While the package offers
several other enhancements like facets, it tries as far as possible to
be a drop-in replacement for the equivalent base plot function. Users
should generally be able to swap a valid `plot()` call with `plot2()`
without any changes to the expected output.

## Quickstart

The **plot2** website includes a detailed [introductory
tutorial](https://grantmcdermott.com/plot2/index.html#/vignettes/intro_tutorial),
with numerous examples. But here are some quickstart examples of the
package in action.

``` r
library(plot2)
```

Grouped scatterplot with automatic legend:

``` r
# with(iris, plot2(x = Petal.Length, y = Sepal.Length, by = Species)) # atomic
plot2(Sepal.Length ~ Petal.Length | Species, data = iris)             # formula
```

<img src="man/figures/README-quickstart2-1.png" style="width:70.0%" />

Same plot with a few extra aesthetic tweaks:

``` r
plot2(
  Sepal.Length ~ Petal.Length | Species, 
  data = iris,
  palette = "dark", pch  = 16,
  grid = TRUE, frame = FALSE
)
```

<img src="man/figures/README-quickstart3-1.png" style="width:70.0%" />

Grouped grouped density plot with automatic legend:

``` r
plot2(
  ~ Petal.Length | Species,
  data = iris,
  type = "density",
  palette = "dark", fill = "by",
  grid = TRUE,
  main = "Distribution of petal lengths by species"
)
```

<img src="man/figures/README-quickstart4-1.png" style="width:70.0%" />

Grouped scatterplot, combined with facet layout:

``` r
iris2 = transform(iris, Sepals = ifelse(Sepal.Length>6, "Long", "Short"))
plot2(
  Sepal.Length ~ Petal.Length | Sepals, data = iris2,
  facet = ~Species,
  facet.args = list(bg = "grey90"),
  palette = "classic",
  main = "Faceted Sepals!",
  grid = TRUE, frame = FALSE
)
```

<img src="man/figures/README-quickstart5-1.png" style="width:70.0%" />

Head over to the [intro
tutorial](https://grantmcdermott.com/plot2/index.html#/vignettes/intro_tutorial)
for more examples, including range plots and customization.
