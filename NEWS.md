# plot2 0.0.3.9003 (development version)

New features:

- Support for `cex` and `bg` arguments. The latter also permits the "by"
convenience keyword similar to `lty` and `pch`. This is useful for plotting
filled point characters (e.g., pch = 21), where you want a different colour for
the fill and border. (#50 @grantmcdermott)

Bug fixes:

- Y-label correctly prints if a function was used for the atomic plot method,
e.g. `plot2(rnorm(100)`. (#52 etiennebacher)
- Ribbon plot types are now automatically ordered by the x variable (#54
@grantmcdermott).
- Interval plots like ribbons, errorbars, and pointranges are now correctly
plotted even if a y variable isn't specified (#54 @grantmcdermott).

# plot2 0.0.3

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

# plot2 0.0.2

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

# plot2 0.0.1

* Initial release on GitHub.
