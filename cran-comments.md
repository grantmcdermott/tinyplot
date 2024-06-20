## Overview

This is a resubmission that addresses several action items requested by the CRAN
team after our first submission in April. While we have directly resolved most
of the CRAN requests, we wish to highlight two issues that require some
additional context: 

1. We were asked to provide DOI links to methods references in our Description.
However, we not have any appropriate references, given that this is a conceptually
simple graphics extension package.

2. In addition to ensuring that all Examples restore the user's original `par`
settings upon completion (which we have addressed), we were asked to ensure that
a user's `par` settings are immediately restored upon exiting our main
`tinyplot` function. However, this more stringent requirement would negate a
main feature of the function---and the **tinyplot** package at large---since,
for example, we need to adjust `par` settings to add legends outside of the plot
margin. Restoring `par` settings by default would mean that users could no
longer add elements like `abline` to a newly created tinyplot in a safe
or consistent way, since the underlying coordinate system has been reverted.
After some back and forth with Prof. Kurt Hornik (see email correspondence with
Achim Zeileis), we understand that this requirement is at least partially
motivated by a lack of precedence. We have thus taken the following steps to
motivate, safeguard, and document the default **tinyplot** behaviour with
respect to `par`:

    - Users can immediately restore their original `par` settings directly by
    invoking the `tinyplot(..., restore.par = TRUE)` argument.

    - If users need to revert `par` settings after the fact---i.e. after a
    tinyplot has already been drawn---then we provide an additional
    `get_saved_par()` convenience function for doing so. This function retrieves
    the state of the user's `par` settings from immediately before the preceding
    `tinyplot()` call, which are automatically saved in a hidden environment
    variable, and can then be used to restore `par` from this original state.
    The `?get_saved_par` help documentation provides several examples, as well
    as a detailed explanation of our rationale (need) for persisting `par`
    settings by default to ensure a consistent user experience. We have
    similarly documented steps to revert or control `par` settings in the help
    documentation of other **tinyplot** functions.

Again, we realise that we are in somewhat uncharted waters here without obvious
precedent. But we hope these additional functions and our explicit documentation
will meet the CRAN requirements. We are happy to discuss further with the CRAN
team if that would prove helpful. 

Original submission notes:

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
