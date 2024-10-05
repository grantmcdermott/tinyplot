type_histogram = function() {
    out <- list(
        data = data_histogram(),
        draw = draw_rect(),
        name = "histogram"
    )
    class(out) <- "tinyplot_type"
    return(out)
}


data_histogram = function() {
    fun = function(by, facet, ylab, col, bg, ribbon.alpha, datapoints, breaks = NULL, ...) {
        hbreaks = ifelse(!is.null(breaks), breaks, "Sturges")

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


