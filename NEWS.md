# News

##  0.0.3.912 (development version)

Website:

We now have a dedicated website! ([#80](https://github.com/grantmcdermott/plot2/issues/80) [@vincentarelbundock](https://github.com/vincentarelbundock))

New features:

- Support for `cex` and `bg` (alias `fill`) arguments. The latter also permit
the "by" convenience keyword similar to `lty` and `pch`. This is useful for
plotting filled point characters (e.g., pch = 21), where you want a different
colour for the fill and border. ([#50](https://github.com/grantmcdermott/plot2/issues/50), [#75](https://github.com/grantmcdermott/plot2/issues/75) [@grantmcdermott](https://github.com/grantmcdermott))
- Support for filled density plots. ([#58](https://github.com/grantmcdermott/plot2/issues/58) [@grantmcdermott](https://github.com/grantmcdermott))
- The new `add` argument allows new plot2 objects to be added to / on top of the
existing plot window. ([#60](https://github.com/grantmcdermott/plot2/issues/60) [@grantmcdermott](https://github.com/grantmcdermott))
- Support for one-sided formulas, e.g. `plot2(~ Temp | Month, airquality)`. ([#62](https://github.com/grantmcdermott/plot2/issues/62) [@grantmcdermott](https://github.com/grantmcdermott) and [@zeileis](https://github.com/zeileis))
- Support for `plot2(x, type = "density")` as an alternative to
`plot2(density(x))`. Works for both the atomic and one-sided formula methods.
([#66](https://github.com/grantmcdermott/plot2/issues/66) [@grantmcdermott](https://github.com/grantmcdermott))
- Support for "area" type plots as a special case of ribbon plots. ([#68](https://github.com/grantmcdermott/plot2/issues/68) [@grantmcdermott](https://github.com/grantmcdermott))
- Partial matching for palette keywords. ([#74](https://github.com/grantmcdermott/plot2/issues/74) [@grantmcdermott](https://github.com/grantmcdermott))

Bug fixes:

- Y-label correctly prints if a function was used for the atomic plot method,
e.g. `plot2(rnorm(100)`. ([#52](https://github.com/grantmcdermott/plot2/issues/52) etiennebacher)
- Ribbon plot types are now automatically ordered by the x variable. ([#54](https://github.com/grantmcdermott/plot2/issues/54) [@grantmcdermott](https://github.com/grantmcdermott))
- Interval plots like ribbons, errorbars, and pointranges are now correctly
plotted even if a y variable isn't specified. ([#54](https://github.com/grantmcdermott/plot2/issues/54) [@grantmcdermott](https://github.com/grantmcdermott))
- Correctly label date-time axes. ([#77](https://github.com/grantmcdermott/plot2/issues/77) [@grantmcdermott](https://github.com/grantmcdermott) and [@zeileis](https://github.com/zeileis))

##  0.0.3

Breaking changes:

- Colour palettes are now controlled via a single `palette` argument that
unifies the old `palette` and (deprecated) `palette.args` arguments. In
addition, the default palette for small groups has been changed from "Okabe-Ito"
to "R4". ([#31](https://github.com/grantmcdermott/plot2/issues/31) and [#32](https://github.com/grantmcdermott/plot2/issues/32) [@grantmcdermott](https://github.com/grantmcdermott))
- Legends are now controlled via a single `legend` argument that unifies the
previous (deprecated) `legend.position` and `legend.args` arguments.  This
change also enables several enhancements over the old legend behaviour; see
below. ([#34](https://github.com/grantmcdermott/plot2/issues/34) [@grantmcdermott](https://github.com/grantmcdermott))

New features:

- Add support for the argument `log`. ([#15](https://github.com/grantmcdermott/plot2/issues/15) [@etiennebacher](https://github.com/etiennebacher))
- Add support for grouped density plots. ([#18](https://github.com/grantmcdermott/plot2/issues/18) [@grantmcdermott](https://github.com/grantmcdermott))
- Add support for (both grouped and non-grouped) "c", "h", "s", and "S" types.
([#26](https://github.com/grantmcdermott/plot2/issues/26) [@grantmcdermott](https://github.com/grantmcdermott))
- Both the `pch` and `lty` arguments now accept a "by" convenience keyword for
automatically adjusting plot characters and line types by groups. ([#28](https://github.com/grantmcdermott/plot2/issues/28) [@grantmcdermott](https://github.com/grantmcdermott))
- Add outside ("!") placement support for remaining legend keywords, e.g.
"top!", "left!", "topright!", etc. Users also gain finer control over many other
aspects of the legend via the new unified `legend` argument, including changing
labels, turning of the legend title, and so on. ([#34](https://github.com/grantmcdermott/plot2/issues/34) [@grantmcdermott](https://github.com/grantmcdermott)) 
- Add support for `"pointrange"`, `"errobar"`, and `"ribbon"` plot types. ([#35](https://github.com/grantmcdermott/plot2/issues/35) [@vincentarelbundock](https://github.com/vincentarelbundock), [#40](https://github.com/grantmcdermott/plot2/issues/40) and [#46](https://github.com/grantmcdermott/plot2/issues/46) [@grantmcdermott](https://github.com/grantmcdermott))
- Support `grid = TRUE` as an alternative to `grid = grid()`. ([#43](https://github.com/grantmcdermott/plot2/issues/43) [@grantmcdermott](https://github.com/grantmcdermott))

Bug fixes:

- Setting `par(pch=X)` globally is now respected. ([#20](https://github.com/grantmcdermott/plot2/issues/20) [@grantmcdermott](https://github.com/grantmcdermott))
- Fix x-axis scale/index when y is NULL. ([#24](https://github.com/grantmcdermott/plot2/issues/24) [@grantmcdermott](https://github.com/grantmcdermott))
- Setting a global palette, e.g. `palette("ggplot2")` is now respected. ([#44](https://github.com/grantmcdermott/plot2/issues/44) [@grantmcdermott](https://github.com/grantmcdermott))

##  0.0.2

Breaking changes:

- Legend defaults have been changed. The default position is now "right!" and
drawn without a border, i.e. bty = "n" ([#14](https://github.com/grantmcdermott/plot2/issues/14) by [@grantmcdermott](https://github.com/grantmcdermott)).

New features:

- Allow users to specify different `pch`, `lty`, and `col` types per group ([#5](https://github.com/grantmcdermott/plot2/issues/5)
and [#11](https://github.com/grantmcdermott/plot2/issues/11) by [@vincentarelbundock](https://github.com/vincentarelbundock)).

Bug fixes:

- Adding further elements to `plot2` now works ([#13](https://github.com/grantmcdermott/plot2/issues/13) by [@grantmcdermott](https://github.com/grantmcdermott), thanks [@karoliskoncevicius](https://github.com/karoliskoncevicius) for reporting).

Internals:

- Better formula parsing for `plot2.formula` ([#8](https://github.com/grantmcdermott/plot2/issues/8) by [@zeileis](https://github.com/zeileis)).
- Unit testing scaffolding based on **tinysnaphot** ([#11](https://github.com/grantmcdermott/plot2/issues/11) by [@vincent](https://github.com/vincent)).

Project:

- [@vincentarelbundock](https://github.com/vincentarelbundock) and [@zeileis](https://github.com/zeileis) have joined the project as core contributors.
🎉

##  0.0.1

* Initial release on GitHub.