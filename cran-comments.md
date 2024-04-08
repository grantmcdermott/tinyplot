## Overview

(This is a resubmission that fixes some minor pre-release check issues caught
previously.) 

- This is a new package that provides a lightweight extension of the base R
graphics system (including automatic legends for grouped data, facets, etc.)
- We run a comprehensive test suite as part of our CI development workflow on
GitHub, which includes hundreds of test snapshots (i.e., SVG images). However,
we have removed these tests from our CRAN submission since their volume results
in a large install tarball, beyond CRAN's recommend size limits.

## Test environments
Local: Arch Linux
GitHub Actions (ubuntu-22.04): release, devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
