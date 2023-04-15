# plot2 0.0.2.9004 (development version)

New features:

- Add support for the argument `log` (#15, @etiennebacher).
- Add support for grouped density plots (#18, @grantmcdermott).

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
