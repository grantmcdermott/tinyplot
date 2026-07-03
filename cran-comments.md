## Overview

**tinyplot** v0.7.0 is a feature release. It brings major updates to our theming
logic and aesthetic stylings, several new plot types and `tinyplot.*` methods,
and various other enhancements and bug fixes. See NEWS.md for the full list of
changes.

## Test environments
macOS (local)
GitHub Actions (ubuntu-24.04): release, devel
Win Builder

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependency checks

We ran reverse-dependency checks on all 12 CRAN revdeps. All packages that
could be checked showed no status change from the current CRAN version of
tinyplot. One package, 'RTMB', failed to install in our CI environment under
both the old and new versions of tinyplot (identical ERROR, unrelated to this
submission -- RTMB requires a TMB/C++ toolchain build that did not complete in
our runner).

P.S. We continue to run a comprehensive test suite comprising hundreds of test
snapshots (i.e., SVG images) as part of our CI development workflow. See:
https://github.com/grantmcdermott/tinyplot/tree/main/inst/tinytest/_tinysnapshot
However, we have removed these test snapshots from our CRAN submission to reduce
the size of of install target and stay within CRAN's recommend size limits.
