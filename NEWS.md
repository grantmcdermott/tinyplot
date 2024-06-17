# News

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
`plt(lat ~ long | depth, data = quakes, pch = 21, cex = 2, bg = 0.2)`. (#129
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
