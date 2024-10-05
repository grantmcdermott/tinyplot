type_boxplot = function() {
  out <- list(
    draw = draw_boxplot(),
    data = data_boxplot(),
    name = "boxplot"
  )
  class(out) <- "tinyplot_type"
  return(out)
}


data_boxplot = function() {
    fun = function(datapoints, bg, col, palette, ...) {
        # Convert x to factor if it's not already
        datapoints$x = as.factor(datapoints$x)

        # Handle factor levels and maintain order
        xlvls = levels(datapoints$x)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        datapoints$x = as.integer(datapoints$x)

        # Handle ordering based on by and facet variables
        null_by = length(unique(datapoints$by)) == 1
        null_facet = length(unique(datapoints$facet)) == 1

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        if (length(unique(datapoints[["by"]])) == 1 && is.null(palette)) {
            if (is.null(col)) col = par("fg")
            if (is.null(bg)) bg = "lightgray"
        } else {
            bg = "by"
        }

        # Reorder x, y, ymin, and ymax based on the order determined
        datapoints = datapoints[xord,]

        # Return the result as a list called 'out'
        out = list(
            x = datapoints$x,
            y = datapoints$y,
            ymin = datapoints$ymin,
            ymax = datapoints$ymax,
            xlabs = xlabs,
            datapoints = datapoints,
            col = col,
            bg = bg)

        if (length(unique(datapoints$by)) > 1) out[["by"]] = datapoints$by
        if (length(unique(datapoints$facet)) > 1) out[["facet"]] = datapoints$facet

        return(out)
    }
    return(fun)
}



draw_boxplot = function() {
    fun = function(i, ix, iy, ipch, ilty, icol, ibg, dots, x_by = FALSE, facet_by = FALSE, split_data, ...) {

        at_ix <- unique(ix)
        horizontal <- if (!is.null(dots[["horizontal"]])) dots[["horizontal"]] else FALSE
        range_ix <- if (!is.null(dots[["range"]])) dots[["range"]] else 1.5
        boxwidth_ix <- if (!is.null(dots[["boxwidth"]])) dots[["boxwidth"]] else NULL
        varwidth_ix <- if (!is.null(dots[["varwidth"]])) dots[["varwidth"]] else FALSE
        notch_ix <- if (!is.null(dots[["notch"]])) dots[["notch"]] else FALSE
        outline_ix <- if (!is.null(dots[["outline"]])) dots[["outline"]] else TRUE
        boxwex_ix <- if (!is.null(dots[["boxwex"]])) dots[["boxwex"]] else 0.8
        if (isTRUE(x_by)) boxwex_ix <- boxwex_ix * 2
        staplewex_ix <- if (!is.null(dots[["staplewex"]])) dots[["staplewex"]] else 0.5
        outwex_ix <- if (!is.null(dots[["outwex"]])) dots[["outwex"]] else 0.5

        # Handle multiple groups
        if (!is.null(split_data) && isFALSE(x_by) && isFALSE(facet_by) && length(split_data) > 1) {
            boxwex_ix_orig <- boxwex_ix
            boxwex_ix <- boxwex_ix / length(split_data) - 0.01
            at_ix <- at_ix + seq(
                -((boxwex_ix_orig - boxwex_ix) / 2),
                ((boxwex_ix_orig - boxwex_ix) / 2),
                length.out = length(split_data)
            )[i]
        }

        boxplot(
            formula = iy ~ ix,
            pch = ipch,
            lty = ilty,
            border = icol,
            col = ibg,
            add = TRUE, axes = FALSE,
            horizontal = horizontal,
            at = at_ix,
            range = range_ix,
            width = boxwidth_ix,
            varwidth = varwidth_ix,
            notch = notch_ix,
            outline = outline_ix,
            boxwex = boxwex_ix,
            staplewex = staplewex_ix,
            outwex = outwex_ix
        )
    }
    return(fun)
}


