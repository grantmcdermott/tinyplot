## Shared singleton handling for the density-family types.
##
## type_density(), type_violin() and type_ridge() all estimate a density per
## group, which means splitting `datapoints` on some combination of the primary
## axis, `by` and `facet`, then discarding any cell too small to smooth. The
## discard is what these helpers implement, along with the optional warning that
## names the groups that went missing.


## The levels split() will group a vector on, without coercing it: factors keep
## their own (unused ones included), anything else gets the sorted unique values
## that factor() would derive anyway. Same pattern as facet.R's panel list.
group_levels = function(x) {
    if (is.factor(x)) levels(x) else sort(unique(x))
}


## The variable name to print in front of a split key's value. Each key prefers
## the label the reader actually sees on the plot -- the legend title for `by`,
## the axis label for the primary axis keys ("x" for violin, "y" for ridge) --
## and falls back to the deparsed input recorded by tinyplot(). The axis label
## is the better source of the two here: under the formula method `x_dep` and
## `y_dep` are just the forwarded placeholders "x" and "y". `facet` never comes
## through here; it has its own titles, built by facet_titles().
key_title = function(key, settings) {
    if (key == "by") {
        return(settings[["legend_args"]][["title"]] %||% settings[["by_dep"]])
    }
    axlab = settings[[paste0(key, "lab")]]
    axlab %||% settings[[paste0(key, "_dep")]] %||% key
}


## Human-readable labels for a set of split cells.
##
## `keys` is the named list of level vectors that `datapoints` was split on, in
## split order; `idx` indexes the resulting cells. Those cells are the cross
## product of the levels with the *first* key varying fastest, which is exactly
## what expand.grid() produces -- so the group behind each cell is recovered
## positionally, rather than by splitting the "."-pasted cell names back apart
## (which breaks on any level containing a ".").
singleton_labels = function(idx, keys, settings) {
    grid = expand.grid(keys, stringsAsFactors = FALSE)
    labs = NULL
    for (key in names(keys)) {
        ## A lone "" is the placeholder that datapoints carries for an absent
        ## grouping variable: no `by`, no `facet`, or a `by` that type_ridge()
        ## folded into the primary axis. Nothing to report for those.
        if (identical(keys[[key]], "")) next
        vals = grid[[key]][idx]
        if (key == "facet") {
            lab = facet_titles(
                vals,
                prefix = TRUE,
                facet_vars = settings[["facet_vars"]],
                facet_grid = isTRUE(attr(settings[["facet"]], "facet_grid"))
            )
            ## grids join the two strip titles with "~"; reads better as a list
            lab = gsub("~", ", ", lab, fixed = TRUE)
        } else {
            lab = paste0(key_title(key, settings), " = ", vals)
        }
        labs = if (is.null(labs)) lab else paste(labs, lab, sep = ", ")
    }
    labs
}


## Drop the split cells that are too small to estimate a density from.
##
## `datapoints` is the list returned by split(); `keys` is the named list of
## level vectors it was split on (only consulted when warning, so callers may
## pass NULL otherwise). `singletons` is one of "drop" (silent), "warn" (drop,
## but say which groups went), or "none" (keep them, and let the bandwidth rules
## complain if they cannot cope).
drop_singletons = function(datapoints, keys, singletons, settings) {
    ## empty cells are an artefact of the split, never a user group, so they go
    ## in every case
    if (singletons == "none") {
        return(Filter(function(k) nrow(k) > 0, datapoints))
    }
    nobs = vapply(datapoints, nrow, integer(1))
    singl = which(nobs == 1L)
    if (singletons == "warn" && length(singl) > 0L) {
        labs = singleton_labels(singl, keys, settings)
        msg = paste0("Dropped ", length(singl), " singleton group(s)")
        if (length(labs) > 0L) {
            msg = paste0(msg, ":\n", paste0("  ", labs, collapse = "\n"))
        }
        warning(msg, "\n", call. = FALSE)
    }
    datapoints[nobs > 1L]
}


## Level vectors for the keys `datapoints` is about to be split on, in split
## order. Only needed to name the offending groups, so it returns NULL unless
## we are actually going to warn -- and it has to be called *before* the split,
## while `datapoints` is still the data frame.
##
## `levels` supplies a key's levels directly, for the case where the column no
## longer carries them: type_violin() recodes `x` to integer plotting positions
## before splitting, so its labels have to come from the levels it saved first.
singleton_keys = function(datapoints, keys, singletons, levels = list()) {
    if (singletons != "warn") return(NULL)
    out = lapply(keys, function(key) {
        levels[[key]] %||% group_levels(datapoints[[key]])
    })
    names(out) = keys
    out
}
