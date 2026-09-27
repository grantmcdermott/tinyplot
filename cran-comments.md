## Overview

**tinyplot** v0.8.0 is a feature release. It adds several new plot types
(hexbin, tile, heatmap, sina), a new `tinyplot.array()` method, substantial
facet enhancements, new arguments for ordering and labelling categorical
variables, performance improvements, and many bug fixes. See NEWS.md for the
full list of changes.

This release includes one minor breaking change: line types now order
categorical `x` data by factor levels, rather than order of appearance, for
consistency with other plot types. The old behaviour remains available via
an explicit argument.

## Test environments
Arch Linux (local)
GitHub Actions (ubuntu-24.04): release, devel
Win Builder

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependency checks

TODO: fill in after revdep-v0.8.0 workflow completes.

P.S. We continue to run a comprehensive test suite comprising hundreds of test
snapshots (i.e., SVG images) as part of our CI development workflow. See:
https://github.com/grantmcdermott/tinyplot/tree/main/inst/tinytest/_tinysnapshot
However, we have removed these test snapshots from our CRAN submission to reduce
the size of the install target and stay within CRAN's recommended size limits.
