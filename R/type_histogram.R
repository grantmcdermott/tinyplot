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
#' @examples
#' # "histogram"/"hist" type convenience string(s)
#' tinyplot(Nile, type = "histogram")
#' 
#' # Use `type_histogram()` to pass extra arguments for customization
#' tinyplot(Nile, type = type_histogram(breaks = 30))
#' @export
type_histogram = function(breaks = "Sturges") {
    out = list(
        data = data_histogram(breaks = breaks),
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


data_histogram = function(breaks = "Sturges") {
    hbreaks = breaks
    fun = function(by, facet, ylab, col, bg, ribbon.alpha, datapoints, .breaks = hbreaks, ...) {
        hbreaks = ifelse(!is.null(.breaks), .breaks, "Sturges")

        if (is.null(ylab)) ylab = "Frequency"
        if (is.null(by) && is.null(palette)) {
            if (is.null(col)) col = par("fg")
            if (is.null(bg)) bg = "lightgray"
        } else {
            if (is.null(bg)) bg = ribbon.alpha
        }

        datapoints_breaks = hist(datapoints$x, breaks = hbreaks, plot = FALSE)
        datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)

        datapoints = lapply(datapoints, function(k) {
            h = hist(k$x, breaks = datapoints_breaks$breaks, plot = FALSE)
            out = data.frame(
                by = k$by[1], # already split
                facet = k$facet[1], # already split
                ymin = 0,
                ymax = h$counts,
                xmin = h$breaks[-1],
                xmax = h$mids + (h$mids - h$breaks[-1])
            )
            return(out)
        })
        datapoints = do.call(rbind, datapoints)

        out = list(
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
        return(out)
    }
    return(fun)
}


