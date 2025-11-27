## Overview

**tinyplot** v0.6.0 a minor release containing mostly bug fixes.

_Note:_ This is a re-submission that fixes a regression discovered by CRAN's
revdep checks (specific to the **parttree** package). Apologies for missing it
the first time around.

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
