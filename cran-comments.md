## Overview

**tinyplot** v0.4.1 is a hotfix / patch release that fixes a regression that we
accidentally introduced in v0.4.0, which was affecting `"ridge"` plot legends.
We apologize for the <1 month CRAN re-submission cadence, but felt that this was
important enough to address quickly.

## Test environments
Arch Linux (local)
GitHub Actions (ubuntu-22.04): release, devel
Win Builder

## R CMD check results

0 errors | 0 warnings | 0 notes

P.S. We continue to run a comprehensive test suite comprising hundreds of test
snapshots (i.e., SVG images) as part of our CI development workflow. See:
https://github.com/grantmcdermott/tinyplot/tree/main/inst/tinytest/_tinysnapshot
However, we have removed these test snapshots from our CRAN submission to reduce
the size of of install target and stay within CRAN's recommend size limits.
