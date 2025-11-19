#' Histogram plot type
#'
#' @md
#' @description Type function for histogram plots. `type_hist` is an alias for
#'   `type_histogram`.
#' @param breaks Passed to \code{\link[graphics]{hist}}. One of:
#' - a vector giving the breakpoints between histogram cells,
#' - a function to compute the vector of breakpoints,
#' - a single number giving the number of cells for the histogram,
#' - a character string naming an algorithm to compute the number of cells (see ‘Details’ of \code{\link[graphics]{hist}}),
#' - a function to compute the number of cells.
#' In the last three cases the number is a suggestion only; as the breakpoints
#' will be set to pretty values, the number is limited to 1e6 (with a warning if
#' it was larger). If breaks is a function, the x vector is supplied to it as
#' the only argument (and the number of breaks is only limited by the amount of
#' available memory).
#' @param free.breaks Logical indicating whether the breakpoints should be
#'   computed separately for each group or facet? Default is `FALSE`, meaning
#'   that the breakpoints are computed from the full dataset; thus ensuring
#'   common bin widths across each group/facet. Can also use `free` as an
#'   acceptable argument alias. Ignored if there are no groups and/or facets.
#' @param drop.zeros Logical indicating whether bins with zero counts should be
#'   dropped before plotting. Default is `TRUE`. Note that switching to `FALSE`
#'   may interfere with faceted plot behaviour if `facet.args = list(free)`,
#'   since the `x` variable is effectively recorded over the full range of the
#'   x-axis (even if it does not extend over this range for every group).
#' @inheritParams graphics::hist
#' @examples
#' # "histogram"/"hist" type convenience string(s)
#' tinyplot(Nile, type = "histogram")
#'
#' # Use `type_histogram()` to pass extra arguments for customization
#' tinyplot(Nile, type = type_histogram(breaks = 30))
#' tinyplot(Nile, type = type_histogram(breaks = 30, freq = FALSE))
#' # etc.
#'
#' # Grouped histogram example
#' tinyplot(
#'     ~ Petal.Width | Species,
#'     type = "histogram",
#'     data = iris
#' )
#'
#' # Faceted version
#' tinyplot(
#'     ~Petal.Width,
#'     facet = ~Species,
#'     type = "histogram",
#'     data = iris
#' )
#'
#' # For visualizing faceted histograms across varying scales, you may also wish
#' # to impose free histogram breaks too (i.e., calculate breaks separately for
#' # each group). Compare:
#'
#' # free facet scales + shared histogram breaks, versus...
#' tinyplot(
#'     ~Petal.Width,
#'     facet = ~Species,
#'     facet.args = list(free = TRUE),
#'     type = type_histogram(),
#'     data = iris
#' )
#' # ... free facet scales + free histogram breaks
#' tinyplot(
#'     ~Petal.Width,
#'     facet = ~Species,
#'     facet.args = list(free = TRUE),
#'     type = type_histogram(free = TRUE),
#'     data = iris
#' )
#'
#' @export
type_histogram = function(breaks = "Sturges",
                          freq = NULL, right = TRUE,
                          free.breaks = FALSE, drop.zeros = TRUE) {
    out = list(
        data = data_histogram(
            breaks = breaks,
            free.breaks = free.breaks, drop.zeros = drop.zeros,
            freq = freq, right = right),
        draw = draw_rect(),
        name = "histogram"
    )
    class(out) = "tinyplot_type"
    return(out)
}
#' @export
#' @name type_hist
#' @rdname type_histogram
type_hist = type_histogram


data_histogram = function(breaks = "Sturges",
                          free.breaks = FALSE, drop.zeros = TRUE,
                          freq = NULL, right = TRUE) {
    hbreaks = breaks
    hfree.breaks = free.breaks
    hdrop.zeros = drop.zeros
    hfreq = freq
    hright = right

    fun = function(settings, .breaks = hbreaks, .freebreaks = hfree.breaks, .freq = hfreq, .right = hright, .drop.zeros = hdrop.zeros, ...) {
        list2env(settings[c("palette", "bg", "col", "plot", "datapoints", "ymin", "ymax", "xmin", "xmax", "freq", "ylab", "xlab", "facet", "ribbon.alpha")], environment())

        hbreaks = ifelse(!sapply(.breaks, is.null), .breaks, "Sturges")

        if (is.null(by) && is.null(palette)) {
            if (is.null(col)) col = par("fg")
            if (is.null(bg)) bg = "lightgray"
        } else {
            if (is.null(bg)) bg = ribbon.alpha
        }

        if (!.freebreaks) xbreaks = hist(datapoints$x, breaks = hbreaks, right = .right, plot = FALSE)$breaks
        datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)

        datapoints = lapply(datapoints, function(k) {
            if (.freebreaks) xbreaks = breaks
            h = hist(k$x, breaks = xbreaks, right = .right, plot = FALSE)
            # zero count cases
            if (.drop.zeros) {
                nzidx = which(h$counts > 0)
                h$density = h$density[nzidx]
                h$counts = h$counts[nzidx]
                h$breaks = h$breaks[c(1, nzidx + 1)]
                h$mids = h$mids[nzidx]
            }
            freq = if (!is.null(.freq)) .freq else is.null(.freq) && h$equidist
            out = data.frame(
                by = k$by[1], # already split
                facet = k$facet[1], # already split
                ymin = 0,
                ymax = if (freq) h$counts else h$density,
                xmin = h$breaks[-1],
                xmax = h$mids + (h$mids - h$breaks[-1]),
                freq = freq
            )
            return(out)
        })
        datapoints = do.call(rbind, datapoints)

        if (is.null(ylab)) {
            ylab = ifelse(datapoints$freq[1], "Frequency", "Density")
        }

        # browser()
        update_settings(settings,
            x = c(datapoints$xmin, datapoints$xmax),
            y = c(datapoints$ymin, datapoints$ymax),
            ymin = datapoints$ymin,
            ymax = datapoints$ymax,
            xmin = datapoints$xmin,
            xmax = datapoints$xmax,
            ylab = ylab,
            col = col,
            bg = bg,
            datapoints = datapoints,
            by = if (length(unique(datapoints$by)) == 1) by else datapoints$by,
            facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet
        )
    }
    return(fun)
}
