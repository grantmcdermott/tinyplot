# plot2 0.0.2.9006 (development version)

Breaking changes:

- The grouped colour palette functionality has been unified into a single
`palette` argument, with the former `palette.args` argument being deprecated
(#30, @grantmcdermott). In addition, the default palette for small groups has
been changed from "Okabe-Ito" to "R4" (#32 @grantmcdermott). 

New features:

- Add support for the argument `log` (#15, @etiennebacher).
- Add support for grouped density plots (#18, @grantmcdermott).
- Add support for (both grouped and non-grouped) "c", "h", "s", and "S" types
(#26, @grantmcdermott).
- Both the `pch` and `lty` arguments now accept a "by" convenience keyword for
automatically adjusting plot characters and line types by groups (#28,
@grantmcdermott).
- Point-range plots with `type="pr"` (#35 @vincentarelbundock)

Bug fixes:

- Setting `par(pch=X)` globally is now respected (#20, @grantmcdermott).
- Fix x-axis scale/index when y is NULL (#24, @grantmcdermott)

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
