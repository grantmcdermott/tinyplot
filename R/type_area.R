#' @rdname type_ribbon
#' @export
type_area = function(alpha = NULL, stack = FALSE) {
    out = list(
        draw = NULL,
        data = data_area(alpha = alpha, stack = stack),
        name = "area"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_area = function(alpha = NULL, stack = FALSE) {
    assert_flag(stack)
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

        if (isTRUE(stack)) {
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

        env2env(environment(), settings, c(
            "datapoints",
            "x",
            "y",
            "ymax",
            "ymin",
            "xlabs",
            "type",
            "ribbon.alpha"
        ))
    }
    return(fun)
}


## Cumulatively stack `y` across the `by` groups, separately within each facet
## and x position. Groups accumulate in `by` level order, so the first level
## forms the bottom band. Returns `datapoints` with `ymin`/`ymax` set to the
## band edges and `y` set to the running total (the ribbon's line is drawn at
## `y`, i.e. along the top of each band).
stack_area = function(datapoints) {
    # A gap in one group would otherwise drop every group stacked above it back
    # down to zero, so complete the facet x by x grid and treat missing (or NA)
    # cells as contributing zero.
    cells = expand.grid(
        x = sort(unique(datapoints$x)),
        by = unique(datapoints$by),
        facet = unique(datapoints$facet),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
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
