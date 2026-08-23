#' @rdname type_ribbon
#' @export
type_area = function(alpha = NULL, stack = FALSE, bylevels = NULL, FUN = NULL) {
    out = list(
        draw = NULL,
        data = data_area(alpha = alpha, stack = stack, bylevels = bylevels, FUN = FUN),
        name = "area"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_area = function(alpha = NULL, stack = FALSE, bylevels = NULL, FUN = NULL) {
    assert_flag(stack)
    assert_function(FUN, null.ok = TRUE)
    if (!is.null(bylevels) &&
        !is.character(bylevels) && !is.numeric(bylevels) && !is.function(bylevels)) {
        stop(
            "`bylevels` must be NULL, a character or numeric vector, or a function.",
            call. = FALSE
        )
    }
    # Stacked bands don't overlap, so the usual semi-transparent ribbon fill
    # only mutes them; default to opaque unless the user asks otherwise.
    ribbon.alpha = if (is.null(alpha) && isTRUE(stack)) {
        1
    } else {
        sanitize_ribbon_alpha(alpha)
    }

    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "xlabs"))

        # Categorical x -> integer positions plus axis labels (cf. data_ribbon)
        if (is.character(datapoints$x)) datapoints$x = as.factor(datapoints$x)
        if (is.factor(datapoints$x)) {
            xlvls = levels(datapoints$x)
            xlabs = seq_along(xlvls)
            names(xlabs) = xlvls
            datapoints$x = as.integer(datapoints$x)
        }

        # Collapse repeated cells *before* ranking below, so that `bylevels`
        # sees the values that actually get drawn. Ranking first would sort on
        # raw per-cell sums, which unequal cell counts can order differently
        # from the aggregated bands.
        if (isTRUE(stack)) {
            datapoints = aggregate_cells(datapoints, FUN = FUN)
        }

        # The `by` level order sets the band order, and with it the legend
        # order and the palette assignment, so this has to happen up front.
        by = NULL
        if (!is.null(bylevels)) {
            datapoints$by = sanitize_bylevels(
                datapoints$by, datapoints$y, datapoints$x, bylevels
            )
            by = datapoints$by
        }

        if (isTRUE(stack)) {
            # bands read bottom-up, so the legend key should too
            settings[["type_hints"]][["legend_reversed"]] = TRUE
            datapoints = stack_area(datapoints)
        } else {
            datapoints$ymax = datapoints$y
            datapoints$ymin = rep.int(0, nrow(datapoints))
        }

        x = datapoints$x
        y = datapoints$y
        ymax = datapoints$ymax
        ymin = datapoints$ymin
        type = "ribbon"

        # ribbon.alpha comes from parent scope, so assign it locally
        ribbon.alpha = ribbon.alpha

        # legend customizations
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% par("lwd")
        settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% 0
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

        vars_to_settings = c(
            "datapoints",
            "x",
            "y",
            "ymax",
            "ymin",
            "xlabs",
            "type",
            "ribbon.alpha"
        )
        # keep settings$by in step with datapoints$by if we releveled it
        if (!is.null(by)) vars_to_settings = c(vars_to_settings, "by")

        env2env(environment(), settings, vars_to_settings)
    }
    return(fun)
}


## Collapse repeated cells down to one `y` per group per `x`. Stacking needs
## exactly one value per cell; repeats -- typically a variable that is in the
## data but not in the plot -- would otherwise be cumsum'd against each other
## into overlapping bands. Mirrors data_barplot(), default statistic included,
## so that the same data stacks to the same heights whether it is drawn as bars
## or as an area.
aggregate_cells = function(datapoints, FUN = NULL) {
    cellid = paste(datapoints$facet, datapoints$x, datapoints$by, sep = "\r")
    if (!anyDuplicated(cellid)) {
        return(datapoints)
    }
    if (is.null(FUN)) FUN = function(x, ...) mean(x, ..., na.rm = TRUE)
    aggregate(
        datapoints[, "y", drop = FALSE],
        datapoints[, c("x", "by", "facet")],
        FUN = FUN
    )
}


## Cumulatively stack `y` across the `by` groups, separately within each facet
## and x position. Groups accumulate in `by` level order, so the first level
## forms the bottom band. Returns `datapoints` with `ymin`/`ymax` set to the
## band edges and `y` set to the running total (the ribbon's line is drawn at
## `y`, i.e. along the top of each band).
stack_area = function(datapoints) {
    # A gap in one group would otherwise drop every group stacked above it back
    # down to zero, so complete the grid and treat missing (or NA) cells as
    # contributing zero. Cross `by` against the (facet, x) pairs that were
    # actually observed, not against every x in the data: a facet must not
    # inherit x positions that only exist in some other facet, or it ramps to
    # zero across a range it never spanned.
    fx = unique(datapoints[, c("facet", "x")])
    fx = fx[order(fx$facet, fx$x), , drop = FALSE]
    cells = merge(fx, data.frame(by = unique(datapoints$by)), by = NULL)
    if (nrow(cells) > nrow(datapoints)) {
        datapoints = merge(
            cells, datapoints,
            by = c("x", "by", "facet"), all.x = TRUE, sort = FALSE
        )
    }
    datapoints$y[is.na(datapoints$y)] = 0

    # cumsum has to run across groups within each (facet, x) cell...
    cellord = order(datapoints$facet, datapoints$x, datapoints$by)
    datapoints = datapoints[cellord, , drop = FALSE]
    cell = paste(datapoints$facet, datapoints$x, sep = "\r")
    datapoints$ymax = ave(datapoints$y, cell, FUN = cumsum)
    datapoints$ymin = datapoints$ymax - datapoints$y
    datapoints$y = datapoints$ymax

    # ... but the polygons are traced along x, so restore group-major ordering
    xord = order(datapoints$facet, datapoints$by, datapoints$x)
    datapoints = datapoints[xord, , drop = FALSE]

    return(datapoints)
}
