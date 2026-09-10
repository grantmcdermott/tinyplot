## Shared singleton handling for the density-family types.
##
## type_density(), type_violin() and type_ridge() all estimate a density per
## group, which means splitting `datapoints` on some combination of the primary
## axis, `by` and `facet`, then discarding any cell too small to smooth.


## Drop the split cells that are too small to estimate a density from.
##
## `datapoints` is the list returned by split(). `singletons` is one of "drop"
## (silent), "warn" (drop, but say how many went), or "none" (keep them, and let
## the bandwidth rules complain if they cannot cope).
##
## Note that this counts rows rather than filtering on them directly: split()
## emits a cell for every level *combination*, so the 0-row cells it invents
## have to be told apart from the 1-row cells the user actually supplied. Only
## the latter are worth warning about; a plain Filter(nrow > 1) conflates the
## two and reports groups that never existed.
drop_singletons = function(datapoints, singletons) {
    ## empty cells are an artefact of the split, never a user group, so they go
    ## in every case
    if (singletons == "none") {
        return(Filter(function(k) nrow(k) > 0, datapoints))
    }
    nobs = vapply(datapoints, nrow, integer(1))
    if (singletons == "warn" && any(nobs == 1L)) {
        warning(
            "Dropped ", sum(nobs == 1L), " singleton group(s). Densities ",
            "require at least 2 observations.\n",
            call. = FALSE
        )
    }
    datapoints[nobs > 1L]
}
