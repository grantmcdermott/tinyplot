# News

##  0.0.3.915 (development version)

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
combined with `by` grouping, or used on their own. (#83, #91, #94
@grantmcdermott)
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
