# News

_If you are viewing this file on CRAN, please check the
[latest NEWS](https://grantmcdermott.com/tinyplot/NEWS.html) on our website
where the formatting is also better._

## Dev version

### New features

- `type_text()` gains a `family` argument for (separately) controlling the font
  family versus to the main plot text elements. (#494 @grantmcdermott)

### Bug fixes

- For bubble plots, we now drop the minimum legend category (label) if it is
  equal to 0. The previous behaviour was just an artifact of the `pretty` breaks
  algorithm that we use to create discrete legend categories. The interior plot
  elements, e.g. bubble points, are unaffected. (#498 @grantmcdermott)
- `type_text()` now defaults to displaying `y` values if an explicit `label` arg
  is not provided, mirroring the behaviour of the base `text()` function.
  (#501 @grantmcdermott) 

## 0.5.0

### New features

- Added support for "bubble" scatter plots, allowing for point size scaling via
  an appropriate `cex` argument (e.g., a continuous variable from your dataset).
  Simultaneously enables dual-legend support for combined size + color mappings.
  The updated `?type_points` documentation contains several examples.
  (#433 @grantmcdermott)
- Improved horizontal legend spacing, as well as multicolumn legend support. A
  new example in the "Tips & tricks" vignette demonstrates the latter.
  (#446 @grantmcdermott)
- Univariate boxplots (without grouping variable) are now handled in
  `tinyplot.default()`, so that `tinyplot(x, type = "boxplot")` and
  `tinyplot(~ x, type = "boxplot")` essentially produce the same output as
  `boxplot(x)`. (#454 @zeileis)
- `type_errorbar()` and `type_point_range()` get a `dodge` argument.
  (#461 @vincentarelbundock)
- The new `tinyplot(..., theme = <theme>)` argument enables users to invoke
  ephemeral themes as an alternative to the persistent themes that follow
  `tinytheme(<theme>)`. (#484 @grantmcdermott)
- Similarly to how the `x/yaxl` arguments allow for axes label adjustment, users
  can now adjust the legend labels too with
  `tinyplot(..., legend = list(labeller = <labeller>))`. The `labeller` argument
  is passed to `tinylabel`; see the latter's help documentation for examples.
  (#488 @grantmcdermott)

### Bug fixes

- `tinyplot_add()` now evaluates the additional call in the environment from
  which `tinyplot_add()` is called so that it also works in non-base environments
  such as in function definitions. Additionally, the call matching is now more
  precise, matching only `tinyplot()` or `plt()` or their fully-qualified
  counterparts (with `tinyplot::` prefix). Finally, the internals where these
  calls are stored are streamlined, avoiding modifying the user-visible
  `options()`. (#460 @zeileis)
- Fixed several minor `tinylabel` bugs. (#468 @grantmcdermott)
  - `tinylabel(x, "%")` is more precise, preserving unique levels of `x` through
     automatic decimal level determination. Thanks to @etiennebacher for the
     bug report in #449.
  - Numeric labellers now work on appropriate `x`/`y` variables, even if the
    plot type internally coerces it to factor (e.g., `"boxplot"`)
- `type_text()` can now also deal with factor `x`/`y` variables by converting
  them to numeric which helps to add text to barplots etc. (#470 @zeileis)
- Fixed some `tinytheme()` bugs.
  - Sourced (non-interactive) scripts with `tinytheme()` calls now inherit the
    correct parameters and spacing. (#475, #481 @grantmcdermott)
  - Custom `cex` theme settings are now reset correctly. (#482 @grantmcdermott)

### Documentation

- @grantmcdermott's _useR! 2025_ **tinyplot** presentation has been added to the
  website as a standalone
  [vignette](https://grantmcdermott.com/tinyplot/vignettes/useR2025/useR2025.html).

### Internals

- Move `altdoc` from `Suggests` to `Config/Needs/website`.
  Thanks to @etiennebacher for the suggestion and to @eddelbuettel for help
  with the CI implementation.
- Add a `devcontainer.json` file for remote testing. (#480 @grantmcdermott) 

## 0.4.2

### New features

- `type_text()` gains `xpd` and `srt` arguments for controlling text clipping
  and rotation, respectively. (#428 @grantmcdermott)
- Add `xlevels` (in addition to `ylevels`) in `type_spineplot()` for spine plots
  with categorical `x` variable. (#431 @zeileis)

### Bug fixes

- Fixed a long-standing issue whereby resizing the plot window would cause
  secondary plot layers, e.g. from `plt_add()`, to become misaligned in 
  faceted plots (#313). This also resolves a related alignment + layering issue
  specific to the Positron IDE
  ([positron#7316](https://github.com/posit-dev/positron/issues/7316)).
  As an aside, `tinyplot` should now be fully compatible with Positron. (#438 @grantmcdermott)
- Fixed a bug that resulted in y-axis labels being coerced to numeric for
  `"p"`-alike plot types (including `"jitter"`) if `y` is a factor or character.
- Safer handling of pre-plot hooks. Resolves an issue affecting how `tinyplot`
  behaves inside loops, particularly for themed plots where only the final plot
  was being drawn in Quarto/RMarkdown contexts. Special thanks to @hadley and @cderv
  for helping us debug. (#425 @vincentarelbundock)
- The `xlevels` argument of `type_barplot()` could not handle numeric indexes correctly.
  (#431 @zeileis)
- Addressed several shortcomings of the straight line family of types (`type_hline`,
  `type_vline`, `type_abline`) through better recycling logic. For example,
  these types now work correctly across non-`by` facets. Simultaneously, users
  can also call them in a base plot layer, relaxing the requirement that they
  must be called as part of a subsequent plot layer via `tinyplot_add()`. (#422 @grantmcdermott)

## 0.4.1

### Bug fixes

- Fix a narrow `tinytheme("ridge")` regression that was accidentally introduced in
  v0.4.0, which was causing a palette mismatch for gradient legends. (#415 @grantmcdermott)

### Misc

- Revert minimum compatible R version to 4.0.0 (#416 @grantmcdermott)

## 0.4.0

### New features:

#### New plot types

- `"barplot"` / `type_barplot()` for bar plots. This closes out
  one of the last remaining canonical base plot types that we wanted to provide
  a native `tinyplot` equivalent for. (#305 and #360 @zeileis and @grantmcdermott) 
- `"violin"` / `type_violin()` for violin plots. (#354 @grantmcdermott)

#### Other new features

- `tinyplot(..., file = "*.pdf")` will now default to using `cairo_pdf()` if
  cairo graphics are supported on the user's machine. This should help to ensure
  better fidelity of (non-standard) fonts in PDFs. (#311 @grantmcdermott)
- The `palette` argument now accepts a vector or list of manual colours, e.g.
  `tinyplot(..., palette = c("cyan4", "hotpink, "purple4"))`, or
  `tinytheme("clean", palette = c("cyan4", "hotpink, "purple4"))` (#325 @grantmcdermott)
- Two new sets of top-level arguments allow for greater axis customization:
  - `xaxb`/`yaxb` control the manual break points of the axis tick marks. (#400 @grantmcdermott)
  - `xaxl`/`yaxl` apply a formatting function to change the appearance of the
    axis tick labels. (#363, #391 @grantmcdermott)
    
  These `x/yaxb` and `x/yaxl` arguments can be used in complementary fashion;
  see the new (lower-level) `tinylabel` function documentation. For example:
  ```r
  tinyplot((0:10)/10, yaxb = c(.17, .33, .5, .67, .83), yaxl = "%")
  ```
- The `x/ymin` and `x/ymax` arguments can now be specified directly via the
  `tinyplot.formula()` method thanks to better NSE processing. For example,
  instead of having to write
  ```r
  with(dat, tinyplot(x = x, y = y, by = by ymin = lwr, ymax = upr))
  ```
  users can now do
  ```r
  tinyplot(y ~ x | by, dat, ymin = lwr, ymax = upr)
  ```
  
  Underneath the hood, this works by processing these NSE arguments as part of
  formula `model.frame()` and reference against the provided dataset. We plan to
  extend the same logic to other top-level formula arguments such as `weights`
  and `subset` in a future version of tinyplot.
  
### Bug fixes:

- The `tinyplot(..., cex = <cex>)` argument should be respected when using
  `type = "b"`. Thanks to @rjknell for report #307 and @vincentarelbundock for
  the fix.
- The `tinyplot(..., lwd = <lwd>)` argument is now correctly passed down to
  `pt.lwd` for type `"p"`, which sets proper line weight for the border of `pch`
  symbols in legend. Report in #319 and fix in #320 by @kscott-1.
- Passing `x` and/or `y` as character variables now triggers the same default
  plot type behaviour as factors, e.g. boxplots. (#323 @grantmcdermott)
- Scatter plots (`type_points()`/`"p"`) now work even if `x` or `y` is a factor
  or character variable. (#323 @grantmcdermott)
- The `tinyplot(..., col = <col>)` argument now accepts a numeric index.
  (#330 @grantmcdermott)
- `type_text()` now accepts non-character labels. (#336 @grantmcdermott)
- The `tinyplot(..., pch = <pch>)` argument now accepts character literals, e.g.
  `pch = "."`. (#338 @grantmcdermott)
- Line plots (`type_lines()`/`"l"`) now pass on the `bg` argument to the
  drawing function. Thanks to @wviechtb for report in #355 (@zeileis).
- Fixed dynamic y-axis margin spacing for flipped `"boxplot"` and `"jitter"`
  types. Thanks to @eddelbuettel for the report in #357 (@grantmcdermott).
- Fixed dynamic x-axis margin spacing for perpendicular (vertical) label text,
  i.e. cases where `las = 2` or `las = 3`. (#369 @grantmcdermott)
- Better integration with the Positron IDE graphics pane. Thanks to @thomasp85
  for the report and helpful suggestions. (#377, #394 @grantmcdermott)
  - The one remaining Positron issue at present is calling `plt_add()` on a
    faceted plot, but this appears to be an upstream limitation/bug
    [positron#7316](https://github.com/posit-dev/positron/issues/7316).
- Fixed a bug that resulted in y-axis labels being coerced to numeric for
  `"p"`-alike plot types (including `"jitter"`) if `y` is a factor or character.
  (#387 @grantmcdermott)
- Fix a colour recycling regression introduced in v0.3.0. Coincidentally, we
  have improved the consistency across `palette` and `col` arguments,
  particularly with respect to recycling behaviour. Thanks to @eddelbuettel for
  the report (#352) and @grantmcdermott for the fix (#410).

### Website:

- Improved column spacing of Arguments in the References section of the website.
  (#328 thanks to @etiennebacher's upstream `altdoc` fix)
- Added a new "Ticks & tips" vignette for non-standard workarounds.
  (#381 @vincentarelbundock)
- Improved website theme and navigation layout, especially on mobile.
  (#395, #411, #413 @zeileis and @retostauffer)

### Internals:

- The order of the nested loop for drawing interior plot elements has been
  switched. We now loop over facets first (outer loop) before looping over
  groups second (inner loop), rather than vice versa. The old/inverted nesting
  logic was mostly an artifact of development inertia and this new nesting logic
  should simplify the creation of certain plot types. (#331 @grantmcdermott)

## 0.3.0

### New features

**tinyplot** v0.3.0 is a big release with many new features, both internal and
user-facing. Related updates are grouped below for easier navigation.

#### Revamped `type` logic and functional equivalents

_(Primary PR and author: #222 @vincentarelbundock)_

- In addition to the standard character labels (`"p"`, `"density"`, etc.), the
  `type` argument now supports _functional_ equivalents (`type_points()`,
  `type_density()`, etc.). These new functional types all take the form
  `type_*()`.
- The character and functional types are interchangeable. For example,
  ```r
  tinyplot(Nile, type = "hist")
  ```
  and
  ```r
  tinyplot(Nile, type = type_hist())
  ```
  produce exactly the same result.
- The main advantage of the functional `type_*()` variants is that they offer
  much more flexibility and control beyond the default case(s). Users can pass
  appropriate arguments to existing types for customization and can even define
  their own `type_<typename>()` functions. More information is available in the
  dedicated help page for each type (e.g., `?type_hist`, `?type_lm`, etc.)
- On the development side, overhauling the `type` system has also allowed us to
  introduce a number of new plot types and features (see list below). We have
  also simplified our internal codebase, since explicit argument passing
  requires less guesswork on our end. Speaking of which, we now recommended that
  users explicitly pass ancillary type-specific arguments as part of the
  relevant `type_*()` call. For example,
  ```r
  tinyplot(Nile, type = type_hist(breaks = 30))
  ```
  is preferable to
  ```r
  tinyplot(Nile, type = "hist", breaks = 30)
  ```
  While the latter option will still work, we cannot guarantee that argument
  passing will work in every situation. (Reason: Passing ancillary type-specific
  arguments at the top level of the plot call only works if these do not
  conflict with the main arguments of the `tinyplot()` function itself; see
  #267.)
- Some minor breaking changes were unavoidable; see further below.
- For more details on the new `type` system, please see the dedicated
  [Plot types vignette](https://grantmcdermott.com/tinyplot/vignettes/types.html)
  on the website.

#### Support for additional plot types

  - Visualizations:
  
    - `type_spineplot()` (shortcut: `"spineplot"`) spine plots and
    spinograms. These are modified versions of a histogram or mosaic plot,
    and are particularly useful for visualizing factor variables. (#233
    @zeileis with contributions from @grantmcdermott)
    - `type_qq()` (shortcut: "qq") for quantile-quantile plots. (#251
    @vincentarelbundock)
    - `type_ridge()` (shortcut: `"ridge"`) for ridge plots aka Joy plots.
    (#252 @vincentarelbundock, @zeileis, and @grantmcdermott)
    - `type_rug()` (shortcut: `"rug"`) adds a rug to an existing plot. (#276
    @grantmcdermott)
    - `type_text()` (shortcut: `"text"`) adds text annotations. (@vincentarelbundock)
    
  - Models:
    - `type_glm()` (shortcut: `"glm"`) (@vincentarelbundock)
    - `type_lm()` (shortcut: `"lm"`) (@vincentarelbundock)
    - `type_loess()` (shortcut: `"loess"`) (@vincentarelbundock)
    - `type_spline()` (shortcut: `"spline"`) (#241 @grantmcdermott)
    
  - Functions:
    - `type_abline()`: line(s) with intercept and slope (#249 @vincentarelbundock)
    - `type_hline()`: horizontal line(s) (#249 @vincentarelbundock)
    - `type_vline()`: vertical line(s) (#249 @vincentarelbundock)
    - `type_function()`: arbitrary function. (#250 @vincentarelbundock)
    - `type_summary()`: summarize values of `y` along unique values of `x` (#274
    @grantmcdermott)

#### Themes

_(Primary PR and authors: #258 @vincentarelbundock and @grantmcdermott)_

- The new `tinytheme()` function provides a convenient mechanism for styling
  plots according to a variety of pre-defined themes, e.g. `tinytheme("clean")`.
- Users can also add their own custom themes or tweak an existing theme.
- Themes are persistent and will affect all subsequent plots until they are
  explicitly reset, e.g. by calling `tinytheme()` (with no argument) to restore
  the default plot aesthetic.
- Behind the scenes, `tinytheme()` sets a hook for a group graphical parameters
  by passing them through `tpar()`. Users can still use `tpar()` to style their
  plots manually by setting individual graphical parameters. But going forward
  we expect that most **tinyplot** users will prefer the convenience of going
  through `tinytheme()`.
- More details are provided in the dedicated
  [Themes vignette](https://grantmcdermott.com/tinyplot/vignettes/themes.html)
  on the website.

#### Other new features

- New `tinyplot()` arguments:
  -  `flip <logical>` allows for easily flipping (swapping) the orientation
  of the x and y axes. This should work regardless of plot type, e.g.
  `tinyplot(~Sepal.Length | Species, data = iris, type = "density", flip = TRUE)`.
  (#216 @grantmcdermott)
  - `draw = <draw_funcs>` allows users to pass arbitrary drawing functions that
  are evaluated as-is, before the main plotting elements. A core use case is
  drawing common annotations across every facet of a faceted plot, e.g. text or
  threshold lines. (#245 @grantmcdermott)
  - `facet.args` gains a `free = <logical>` sub-argument for independently
  scaling the axes limits of individual facets. (#253 @grantmcdermott)
  
- `tpar()` gains additional `grid.col`, `grid.lty`, and `grid.lwd` arguments for
  fine-grained control over the appearance of the default panel grid when
  `tinyplot(..., grid = TRUE)` is called. (#237 @grantmcdermott)
  
- The new `tinyplot_add()` (alias: `plt_add()`) convenience function allows
easy layering of plots without having to specify repeat arguments. (#246
@vincentarelbundock)

### Breaking changes

- There are a few breaking changes to grouped density plots.
  - The joint smoothing bandwidth is now computed using an observation-weighted
    mean (as opposed to a simple mean). Users can customize this joint bandwidth 
    by invoking the new `type_density(joint.bw = <option>)` argument. See the
    function documentation for details.  (#291 @grantmcdermott and @zeileis)
  - Grouped and/or faceted plots are no longer possible on density objects
    (i.e., via the `tinyplot.density()` method). Instead, please rather call
    `tinyplot(..., type = "density")` or `tinyplot(..., type = type_density())`
    on the raw data and pass grouping or facet arguments as needed.
    (#284 @grantmcdermott)
- The `ribbon.alpha` argument in `tinyplot()` has been deprecated. Use the
  `alpha` argument in `type_ribbon()` (and equivalents) instead: e.g.,
  `tinyplot(..., type = type_ribbon(alpha = 0.5))`.
  - Aside: Please note that this is _not_ equivalent to using
  `tinyplot(..., type = "ribbon", alpha = 0.5)` because the latter matches the
  top-level `alpha` argument of `tinyplot()` itself (and thus modifies the
  entire `palette`, rather than just the ribbon). See our warning about passing
  ancillary type-specific arguments above.

### Bug fixes

- Better preserve facet attributes, thus avoiding misarrangement of facet grids
for density and histogram types. (#209 @zeileis)
- Plots of the form `plt(numeric ~ character)` now work correctly, with the
character variable automatically being coerced to a factor. (#219 @zeileis)
- Respect `xlim` and `ylim` when explicitly supplied by the user. (Thanks to
@mclements for code submission #221)
- Axis titles for flipped (horizontal) boxplots are appropriately swapped too.
(#223 @grantmcdermott)
- Ribbon plots without `ymin` or `ymax` args, now inherit these values from `y`
(#224 @grantmcdermott)
- Plots where `y` is a factor now work automatically, dispatching to the new
`type_spineplot()` type. Thanks to @zeileis for the original suggestion all the
way back in #2 and the eventual solution in #233.
- Free axis scaling now works properly for faceted histograms. The new
`type_histogram(free.breaks = <logical>, drop.zeros = <logical>)` arguments
enable fine-grained control over this behaviour. (#228 @eleuven and
@grantmcdermott)

### Misc

- Continued modularization/abstraction of the code logic. (#214
@vincentarelbundock)
- Major internal refactor of the type drawing and data processing. (#222
@vincentarelbundock)
- Documentation improvements, e.g. explicit guidance on how to specify multiple
grouping variables (thanks to @strengjacke for reporting #213).
  - The new functional type processing system also means that each type now
    has its own help page (e.g. `?type_hist`, `type_ridge`, etc.)

## 0.2.1

New Features:

- The `axes` argument of `tinyplot()`/`plt()` gains extra options for
fine-grained control of the plot axes. In addition to the existing logical
(`TRUE`/`FALSE`) option, users can now specify one of the following character
keywords (or, just their first letters as a convenient shorthand):
  - `"standard"` (with axis, ticks, and labels; equivalent to `TRUE`),
  - `"none"` (no axes; equivalent to `FALSE`),
  - `"ticks"` (only ticks and labels without axis line),
  - `"labels"` (only labels without ticks and axis line),
  - `"axis"` (only axis line and labels but no ticks).

  Simultaneously, the main plotting functions also gain the `xaxt` and `yaxt`
for _separately_ controlling the two axes using the same keyword options. For
example, `plt(0:10, xaxt = "l", yaxt = "t")` will yield a plot where the x-axis
only contains labels and the y-axis contains both labels and ticks, but no axis
line. (#190 @zeileis)
- Support additional boxplot arguments like `varwidth`, `notch`, etc. Note
that `tinyplot(..., type = "boxplot", boxwidth = <num>)` is equivalent to the
`boxplot(..., width = <num>)`; we just use the "box(width)" prefix to avoid 
conflicting with the existing `tinyplot(..., width)` argument.
(#196 @grantmcdermott)

Bug fixes:

- Fix duplicate plots produced with `type = "density"`, which was a regression
accidentally introduced in v0.2.0 (#187 @grantmcdermott)
- Ensure correct boxplot positioning if `x` == `by`, or these two are
functionally identical. (#196 @grantmcdermott)
- `xlab` and `ylab` arguments not respected in some plots. Thanks to @lbelzile
for reporting Issue #203.
- Avoid triggering an inadvertent legend when a function transformation of x is
plotted against x itself, `tinyplot(log(x) ~ x)`. (#197 @zeileis)
- Facets with interactions and/or multivariate formulas (e.g., complex grid
arrangements like `tinyplot(mpg ~ wt, data = mtcars, facet = am + vs ~ gear)`)
now plot all panels correctly, even if some combinations are missing. (#197
@grantmcdermott)
- Fix alignment of facet titles when axes are logged. (#207 @grantmcdermott)
- Consistent decimals for gradient legends (#277 @grantmcdermott) 

Internals:

- Continued modularization of the main code logic. (#192 & #198
@vincentarelbundock)
- Revamped formula processing that allows for better sanity checking and
edge-case logic. (#197 @zeileis)

## 0.2.0

New features:

- Support for additional plot types:
  - `type = "n"`, i.e. empty plot. Since `type = "n"` implicitly assumes points,
  which limits the type of legend that can be drawn alongside the empty plot, we
  have also added a companion `empty` argument that can be used alongside any
  plot type. (#157, #167 @grantmcdermott)
  - `type = "boxplot"`. Simultaneously enables `plt(numeric ~ factor)`
  support, first raised in #2, so that a boxplot is automatically plotted if a
  numeric is plotted against a factor. (#154 @grantmcdermott)
  - `type = "polypath"`. (#159 @grantmcdermott)
  - `type = "rect"`. (#161 @grantmcdermott)
  - `type = "segments"`. (#163 @grantmcdermott)
  - `type = "histogram"` (alias `type = "hist"`). (#164 @grantmcdermott)
  - `type = "jitter"` (alias `type = "j"`). (#170 @grantmcdermott)

Internals:

- The main codebase has been significantly refactored (modularized), which
should simplify future maintenance and enable better user-level error messages
(#171, #173 @vincentarelbundock)

Misc:

- Various documentation improvements.

## 0.1.0

Our first CRAN submission! This v0.1.0 release includes the following new
features and updates:

License:

- Formally switch to Apache 2.0 license. (#141 @grantmcdermott)

Breaking changes:

- To ensure consistent "dot.case" style for all `tinyplot()` function arguments,
the following two arguments have been renamed (`old` => `new`):
  - `par_restore` => `restore.par` (note the change in word order too!)
  - `ribbon_alpha` => `ribbon.alpha`
  
  We don't believe that these two arguments are much used in practice. So
  hopefully it will only have a negligible effect on existing `tinyplot` code in
  the wild, even though it is a breaking change. (#149 @grantmcdermott)

New features:

- Gradient legends are now supported if a continuous variable is passed to
`by`. Thanks to @zeileis for detailed feedback and advice around the default
palette choice (a restricted version of the "viridis" palette), as well as
StackOverflow user mnel, whose answer
[here](https://stackoverflow.com/a/13355440) provided the inspiration for the
final implementation. (#122 @grantmcdermott)
- Ordered factors now inherit a discrete sequential color palette ("viridis") by
default. Thanks to @zeileis for the suggestion. (#130 @grantmcdermott)
- Support user-supplied polygons. (#127 @grantmcdermott)
- Support for the `lwd` argument for adjusting line widths. Similar to `pch`,
`lty`, etc. this arguments also accepts a "by" convenience keyword to
automatically vary line widths by group. (#134 @grantmcdermott)
- `tpar()` now accepts standard `par()` arguments in addition to the
`tinyplot`-specific ones. This allows users to set or query graphical parameters
via a single convenience function, instead having to invoke `tpar` and `par`
separately. (#140 @grantmcdermott)
  - As an aside, `tpar()` has gained some additional parameters for fine-grained
  control of global plot defaults, including `grid`, `ribbon.alpha`, and various
  `file.*` parameters (see next bullet point).
- Users can write plots directly to disk using the new `file` argument,
alongside corresponding `width` and `height` arguments for output customization
(both of which are defined in inches). For example,
`tinyplot(..., file = "~/myplot.png", width = 8, height = 5)`. This
implementation relies on a simple internal wrapper around the traditional R
external graphics devices like `png()`, `pdf()`, etc. But it may prove more
convenient, since the current global graphics parameters held in `(t)par()` are
carried over to the external device too and don't need to be reset. Note that
the appropriate device type is determined automatically by the file extension,
which must be one of ".png", ".jpg" (".jpeg"), ".pdf", or ".svg".
(#143 @grantmcdermott)
- We have a shiny new `tinyplot` logo. (#148 @grantmcdermott)
- The new `get_saved_par()` function can be used to retrieve the `par` settings
from immediately before or immediately after the preceding `tinyplot` call.
This function replaces some older (non-exported) internal functions that
`tinyplot` was using to restore and control `par` environments. But it could
also prove help to end users who are looking for additional ways to restore
`par` settings after the fact. See `?get_saved_par` for some examples. (#152
@grantmcdermott)
- `tinyplot`/`plt` gaina a new `alpha = <numeric[0,1]>` convenience argument for
adding transparency to plot elements and colours. Example use:
`plt(rnorm(1e3), pch = 19, alpha = 0.3)`. (#129 @grantmcdermott)
- Similar to the preceding news item, transparency can be added to (grouped)
background fill by passing `bg` (or its alias, `fill`) a numeric in the range
`[0,1]`. This feature has the same effect as `bg = "by"` except for the added
transparency. Example use:
`tinyplot(lat ~ long | depth, data = quakes, pch = 21, cex = 2, bg = 0.2)`. (#129
@grantmcdermott)


Bug fixes:

- Fixed bug that prevented `tpar(facet.x = ...)` args from being passed forward
and set correctly. (#137 @grantmcdermott)
- Fixed bug where custom legends weren't working with `type = "density"`. (#147
@grantmcdermott)

Internals:

- We no longer ship the vignette(s) with the built package. This helps to reduce
the size of the installation tarball and also avoids some redundancy with the
actual help documentation (since many of the examples are the same). Note that
the vignettes are all still rendered and available online at the `tinyplot`
[website](https://grantmcdermott.com/tinyplot/).
(#135 @grantmcdermott)
- Similarly, we anticipate skipping tests on CRAN since the large suite of test
snapshots (images) held in `inst/tinytest` is pushing the install tarball over
CRAN's recommended 5 MB limit. Please note that local testing of the package
requires adding the `NOT_CRAN=TRUE` environment variable to your .Renviron file
(or, exporting it in your .bashrc/.zshrc/etc. dotfile if you prefer that
approach). (#145 @vincentarelbundock & @grantmcdermott)
- Update some test snapshots to match slight changes in the way that R 4.4.0
calculates `density` grid coords. (#150 @grantmcdermott)


## 0.0.5

**IMPORTANT BREAKING CHANGE:**

The package has been renamed to **tinyplot**. (#22 @grantmcdermott)

This package renaming also incorporates the following function changes:

- `plot2()` is replaced by `tinyplot()` (or its shorthand alias `plt()`).
- `par2()` is replaced by `tpar()`.

So, where you used to write...

```r
library(plot2)
plot2(Sepal.Length ~ Petal.Length | Species, iris)
```

... you should now write:

```r
library(tinyplot)
tinyplot(Sepal.Length ~ Petal.Length | Species, iris)

# Or, use the equivalent shorthand `plt` alias
plt(Sepal.Length ~ Petal.Length | Species, iris)
```

The package URLs have been updated accordingly:

- GitHub: https://github.com/grantmcdermott/tinyplot
- Website: https://grantmcdermott.com/tinyplot
- R-universe: https://grantmcdermott.r-universe.dev/tinyplot

Many thanks to everyone who provided thoughtful feedback about this prospective
name change, especially @zeileis and @vincentarelbundock for kicking off the
discussion, as well as the 100+ participants who voted in the social media
poll.

For more details about the rational underlying this renaming decision, please
see the following GitHub comment, as well as the discussion that preceded it:
https://github.com/grantmcdermott/plot2/issues/22#issuecomment-1928472754


##  0.0.4

Website:

We now have a dedicated website! (#80 @vincentarelbundock)

New features:

- Support for `cex` and `bg` (alias `fill`) arguments. The latter also permit
the "by" convenience keyword similar to `lty` and `pch`. This is useful for
plotting filled point characters (e.g., pch = 21), where you want a different
colour for the fill and border. (#50, #75 @grantmcdermott)
- Support for filled density plots. (#58 @grantmcdermott)
- The new `add` argument allows new plot2 objects to be added to / on top of the
existing plot window. (#60 @grantmcdermott)
- Support for one-sided formulas, e.g. `plot2(~ Temp | Month, airquality)`. (#62
@grantmcdermott and @zeileis)
- Support for `plot2(x, type = "density")` as an alternative to
`plot2(density(x))`. Works for both the atomic and one-sided formula methods.
(#66 @grantmcdermott)
- Support for "area" type plots as a special case of ribbon plots. (#68
@grantmcdermott)
- Partial matching for palette keywords. (#74 @grantmcdermott)
- `plot2` gains a new `facet` argument for drawing faceted plots. Users can
override the default square arrangement by passing the desired number of facet
rows or columns to the companion `facet.args` helper function. Facets can be
combined with `by` grouping, or used on their own.
(#83, #91, #94, #96, #101, #103 @grantmcdermott)
- Users can now control `plot2`-specific graphical parameters globally via
the new `par2()` function (which is modeled on the base `par()` function). At
the moment only a subset of global parameters, mostly related to legend and
facet behaviour, are exposed in `par2`. But users can expect that more will be
added in future releases. (#33, #94 @grantmcdermott)

Bug fixes:

- Y-label correctly prints if a function was used for the atomic plot method,
e.g. `plot2(rnorm(100)`. (#52 etiennebacher)
- Ribbon plot types are now automatically ordered by the x variable. (#54
@grantmcdermott)
- Interval plots like ribbons, errorbars, and pointranges are now correctly
plotted even if a y variable isn't specified. (#54 @grantmcdermott)
- Correctly label date-time axes. (#77 @grantmcdermott and @zeileis)
- Improved consistency of legend and facet margins across different plot types
and placement, via the new `lmar` and `fmar` arguments of `par2()`. The default
legend margin is `par2(lmar = c(1,0, 0.1)`, which means that there is 1.0 line
of padding between the legend and the plot region (inside margin) and 0.1 line 
of padding between the legend and edge of the graphics device (outer margin).
Similarly, the default facet padding is `par2(fmar = c(1,1,1,1)`, which means
that there is a single line of padding around each side of the individual
facets. Users can override these defaults by passing numeric vectors of the
appropriate length to `par2()`. For example, `par2(lmar = c(0,0.1)` would shrink
the inner gap between the legend and plot region to zero, but leave the small
outer gap to outside of the graphics device unchanged. (#94 @grantmcdermott)
- Fix bug where grid wasn't auto-expanding correctly for area plots. (#92
@grantmcdermott)

##  0.0.3

Breaking changes:

- Colour palettes are now controlled via a single `palette` argument that
unifies the old `palette` and (deprecated) `palette.args` arguments. In
addition, the default palette for small groups has been changed from "Okabe-Ito"
to "R4". (#31 and #32 @grantmcdermott)
- Legends are now controlled via a single `legend` argument that unifies the
previous (deprecated) `legend.position` and `legend.args` arguments.  This
change also enables several enhancements over the old legend behaviour; see
below. (#34 @grantmcdermott)

New features:

- Add support for the argument `log`. (#15 @etiennebacher)
- Add support for grouped density plots. (#18 @grantmcdermott)
- Add support for (both grouped and non-grouped) "c", "h", "s", and "S" types.
(#26 @grantmcdermott)
- Both the `pch` and `lty` arguments now accept a "by" convenience keyword for
automatically adjusting plot characters and line types by groups. (#28
@grantmcdermott)
- Add outside ("!") placement support for remaining legend keywords, e.g.
"top!", "left!", "topright!", etc. Users also gain finer control over many other
aspects of the legend via the new unified `legend` argument, including changing
labels, turning of the legend title, and so on. (#34 @grantmcdermott) 
- Add support for `"pointrange"`, `"errobar"`, and `"ribbon"` plot types. (#35
@vincentarelbundock, #40 and #46 @grantmcdermott)
- Support `grid = TRUE` as an alternative to `grid = grid()`. (#43
@grantmcdermott)

Bug fixes:

- Setting `par(pch=X)` globally is now respected. (#20 @grantmcdermott)
- Fix x-axis scale/index when y is NULL. (#24 @grantmcdermott)
- Setting a global palette, e.g. `palette("ggplot2")` is now respected. (#44
@grantmcdermott)

##  0.0.2

Breaking changes:

- Legend defaults have been changed. The default position is now "right!" and
drawn without a border, i.e. bty = "n" (#14 by @grantmcdermott).

New features:

- Allow users to specify different `pch`, `lty`, and `col` types per group (#5
and #11 by @vincentarelbundock).

Bug fixes:

- Adding further elements to `plot2` now works (#13 by @grantmcdermott, thanks
@karoliskoncevicius for reporting).

Internals:

- Better formula parsing for `plot2.formula` (#8 by @zeileis).
- Unit testing scaffolding based on **tinysnaphot** (#11 by @vincent).

Project:

- @vincentarelbundock and @zeileis have joined the project as core contributors.
🎉

##  0.0.1

* Initial release on GitHub.
