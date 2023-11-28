
# Get Started

Start by loading the package.

``` r
library(plot2)
```

For most of the examples that follow, we’ll use a slightly modified
version of the `airquality` dataset that comes bundled with base R.
Let’s go ahead and create it now.

``` r
aq = airquality
aq$Month = factor(month.abb[aq$Month], levels = month.abb[5:9])
```

## Similarity to `plot()`

As far as possible, `plot2` tries to be a drop-in replacement for
regular `plot` calls.

``` r
par(mfrow = c(1, 2))

plot(0:10, main = "plot")
plot2(0:10, main = "plot2")
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/base_1-1.png"
style="width:70.0%" />

Similarly, we can plot elements from a data frame using either the
atomic or formula methods. Here’s a simple example using the `aq`
dataset that we created earlier.

``` r
# with(aq,  plot2(Day, Temp)) # atomic method (same as below)
plot2(Temp ~ Day, data = aq) # formula method
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/plot2_simple-1.png"
style="width:70.0%" />

## Grouped data

Where `plot2` starts to diverge from its base counterpart is with
respect to grouped data. In particular, `plot2` allows you to
characterize groups using the `by` argument.[1]

``` r
# plot2(aq$Day, aq$Temp, by = aq$Month) # same as below
with(aq, plot2(Day, Temp, by = Month))
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/by-1.png"
style="width:70.0%" />

An arguably more convenient approach is to use the equivalent formula
syntax. Just place the grouping variable after a vertical bar (i.e.,
`|`).

``` r
plot2(Temp ~ Day | Month, data = aq)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/formula-1.png"
style="width:70.0%" />

You can use standard base plotting arguments to adjust features of your
plot. For example, change `pch` (plot character) to get filled points.

``` r
plot2(
  Temp ~ Day | Month, data = aq,
  pch = 16
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/pch_16-1.png"
style="width:70.0%" />

Similarly, converting to a grouped line plot is a simple matter of
adjusting the `type` argument.

``` r
plot2(
  Temp ~ Day | Month, data = aq,
  type = "l"
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/type_l-1.png"
style="width:70.0%" />

The default behaviour of `plot2` is to represent groups through colour.
However, note that we can automatically adjust `pch` and `lty` by groups
too by passing the `"by"` convenience keyword. This can be used in
conjunction with the default group colouring. Or, as a replacement for
group colouring—an option that may be particularly useful for contexts
where colour is expensive or prohibited (e.g., certain academic
journals).

``` r
plot2(
  Temp ~ Day | Month, data = aq,
  type = "l",
  col = "black", # override automatic group colours
  lty = "by"     # change line type by group instead
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/by_lty-1.png"
style="width:70.0%" />

## Colours

On the subject of group colours, these are easily customized via the
`palette` argument. The default group colours are inherited from the
user’s default palette. (Most likely the “R4” set of colors; see
`?palette`). However, all of the various palettes listed by
`palette.pals()` and `hcl.pals()` are supported as convenience
strings.[2] Note that case-insensitive, partial matching for these
convenience string is allowed. For example:

``` r
plot2(
  Temp ~ Day | Month, data = aq,
  type = "l",
  palette = "tableau" # or "ggplot2", "okabe-ito", "set2", "harmonic", etc.
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/palette_tableau-1.png"
style="width:70.0%" />

Beyond these convenience strings, users can also supply a valid
palette-generating function for finer control over transparency, colour
order, and so forth. We’ll see a demonstration of this further below.

To underscore what we said earlier, colours are inherited from the
user’s default palette. So these can also be set globally, just as they
can for the base `plot` function. The next code chunk will set a new
default palette for the remainder of the plots that follow.

``` r
# Set the default palette globally via the generic palette function
palette("tableau")
```

## Legend

In all of the preceding plots, you will have noticed that we get an
automatic legend. The legend position and look can be customized with
the `legend` argument. At a minimum, you can pass the familiar legend
position keywords as a convenience string (“topright”, “left”, etc.).
Moreover, a key feature of `plot2` is that we can easily and elegantly
place the legend *outside* the plot area by adding a trailing “!” to
these keywords. (As you may have realised, the default legend position
is “right!”.) Let’s demonstrate by moving the legend to the left of the
plot:

``` r
plot2(
  Temp ~ Day | Month, data = aq,
  type = "l",
  legend = "left!"
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/legend_bottom-1.png"
style="width:70.0%" />

Beyond the convenience of these positional keywords, the `legend`
argument also permits additional customization by passing an appropriate
function (or, a list of arguments that will be passed on to the standard
`legend()` function internally.) So you can change or turn off the
legend title, remove the bounding box, switch the direction of the
legend text to horizontal, etc. Here’s a grouped density plot example,
where we also add some shading by specifying that the background colour
should vary by groups too.

``` r
with(
  aq,
  plot2(
    density(Temp),
    by = Month,
    fill = "by",                         # add fill by groups
    grid = TRUE,                         # add background grid
    legend = list("topright", bty = "o") # change legend features
  )
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/density_topright-1.png"
style="width:70.0%" />

## Interval plots

`plot2` adds supports for interval plots via the `"pointrange"`,
`"errorbar"`, `"ribbon"` type arguments. An obvious use-case is for
regression analysis and prediction.

``` r
mod = lm(Temp ~ 0 + Month / Day, data = aq)
aq = cbind(aq, predict(mod, interval = "confidence"))

with(
  aq,
  plot2(
    x = Day, y = fit, by = Month,
    ymin = lwr, ymax = upr,
    type = "ribbon",
    grid = TRUE,
    main = "Model predictions"
  )
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/ribbon_pred-1.png"
style="width:70.0%" />

Similarly, we can grab the model estimates to produce nice coefficient
plots.

``` r
coeftab = data.frame(
  gsub("Month", "", names(coef(mod))),
  coef(mod),
  confint(mod)
) |>
  setNames(c("term", "estimate", "ci_low", "ci_high"))

with(
  subset(coeftab, !grepl("Day", term)),
  plot2(
    x = term, y = estimate,
    ymin = ci_low, ymax = ci_high,
    type = "pointrange", # or: "errobar", "ribbon"
    pch = 19, col = "dodgerblue",
    grid = TRUE,
    main = "Average Monthly Effect on Temperature"
  )
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/pointrange-1.png"
style="width:70.0%" />

## Customization

Customizing your plots further is straightforward, whether that is done
by changing global parameters (via `par`) or invoking `plot2` arguments.
Here’s a quick penultimate example, where we change our point character,
tick labels, and font family globally, before adding some transparency
to our colour palette, and use Tufte-style floating axes with a
background panel grid.

``` r
par(
  pch    = 16,           # Filled points as default
  las    = 1,            # Horizontal axis tick labels
  family = "HersheySans" # Use a (built-in) Hershey font instead of Arial default
)

plot2(
  Temp ~ Day | Month, data = aq,
  type = "b",
  palette = palette.colors(palette = "tableau", alpha = 0.5),
  frame = FALSE, grid = TRUE,
  main = "Daily temperatures by month"
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/hershey_plus-1.png"
style="width:70.0%" />

At the risk of repeating ourselves, the use of `par` in the previous
example again underscores the correspondence with the base graphics
system. Because `plot2` is effectively a convenience wrapper around base
`plot`, any global elements that you have set for the latter should
carry over to the former. For nice out-of-the-box themes, we recommend
the **basetheme** package.

``` r
par(pch = 15, las = 0, family = "") # change/revert global changes from above

library(basetheme)
basetheme("royal") # or "clean", "dark", "ink", "brutal", etc.

plot2(
  Temp ~ Day | Month, data = aq,
  type = "b",
  pch = "by",
  palette = "tropic",
  main = "Daily temperatures by month"
)
```

<img
src="vignettes/get_started.markdown_strict_files/figure-markdown_strict/basethme_royal-1.png"
style="width:70.0%" />

``` r

basetheme(NULL)  # back to default theme
dev.off()
#> null device 
#>           1
```

[1] At this point, experienced base plot users might protest that you
*can* colour by groups using the `col` argument, e.g.
`with(aq, plot(Day, Temp, col = Month))`. This is true, but there are
several limitations. First, you don’t get an automatic legend. Second,
the base `plot.formula` method doesn’t specify the grouping within the
formula itself (not a deal-breaker, but not particularly consistent
either). Third, and perhaps most importantly, this grouping doesn’t
carry over to line plots (i.e., type=“l”). Instead, you have to
transpose your data and use `matplot`. See
[this](https://stackoverflow.com/questions/10519873/how-to-create-a-line-plot-with-groups-in-base-r-without-loops)
old StackOverflow thread for a longer discussion.

[2] See the accompanying help pages of those two functions for more
details on the available palettes, or read the
[article](https://arxiv.org/pdf/2303.04918.pdf) by Achim Zeileis and
Paul Murrell.
