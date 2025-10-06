#' Boxplot type
#' 
#' @description Type function for producing box-and-whisker plots.
#' Arguments are passed to \code{\link[graphics]{boxplot}}, although `tinyplot`
#' scaffolding allows added functionality such as grouping and faceting.
#' Box-and-whisker plots are the default plot type if `x` is a factor and `y` is
#' numeric.
#'
#' @inheritParams graphics::boxplot
#' @examples
#' # "boxplot" type convenience string
#' tinyplot(count ~ spray, data = InsectSprays, type = "boxplot")
#' 
#' # Note: Specifying the type here is redundant. Like base plot, tinyplot
#' # automatically produces a boxplot if x is a factor and y is numeric
#' tinyplot(count ~ spray, data = InsectSprays)
#' 
#' # Grouped boxplot example
#' tinyplot(len ~ dose | supp, data = ToothGrowth, type = "boxplot")
#' 
#' # Use `type_boxplot()` to pass extra arguments for customization
#' tinyplot(
#'   len ~ dose | supp, data = ToothGrowth, lty = 1,
#'   type = type_boxplot(boxwex = 0.3, staplewex = 0, outline = FALSE)
#' )
#' @export
type_boxplot = function(
    range = 1.5,
    width = NULL,
    varwidth = FALSE,
    notch = FALSE,
    outline = TRUE,
    boxwex = 0.8,
    staplewex = 0.5,
    outwex = 0.5) {
  out = list(
    draw = draw_boxplot(
      range = range,
      width = width,
      varwidth = varwidth,
      notch = notch,
      outline = outline,
      boxwex = boxwex,
      staplewex = staplewex,
      outwex = outwex),
    data = data_boxplot(),
    name = "boxplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}



draw_boxplot = function(range, width, varwidth, notch, outline, boxwex, staplewex, outwex) {
    fun = function(iby, ix, iy, ipch, ilty, icol, ibg, x_by = FALSE, facet_by = FALSE, ngrps = 1, flip, ...) {

        at_ix = unique(ix)
        if (isTRUE(x_by)) boxwex = boxwex * 2

        # Handle multiple groups
        if (ngrps > 1 && isFALSE(x_by) && isFALSE(facet_by)) {
            boxwex_orig = boxwex
            boxwex = boxwex / ngrps - 0.01
            at_ix = at_ix + seq(
              -((boxwex_orig - boxwex) / 2),
              ((boxwex_orig - boxwex) / 2),
              length.out = ngrps
            )[iby]
        }

        boxplot(
            formula = iy ~ ix,
            pch = ipch,
            lty = ilty,
            border = icol,
            col = ibg,
            horizontal = flip,
            add = TRUE, axes = FALSE,
            at = at_ix,
            range = range,
            width = width,
            varwidth = varwidth,
            notch = notch,
            outline = outline,
            boxwex = boxwex,
            staplewex = staplewex,
            outwex = outwex
        )
    }
    return(fun)
}



data_boxplot = function() {
    fun = function(settings, ...) {
        list2env(settings, envir = environment())
        # Convert x to factor if it's not already
        datapoints$x = as.factor(datapoints$x)

        # Handle factor levels and maintain order
        xlvls = levels(datapoints$x)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        datapoints$x = as.integer(datapoints$x)

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        # Check if user provided palette from raw_input (before substitute)
        user_palette = raw_input$palette
        if (length(unique(datapoints[["by"]])) == 1 && is.null(user_palette)) {
            if (is.null(col)) col = par("fg")
            if (is.null(bg)) bg = "lightgray"
        } else {
            if (is.null(bg)) bg = "by"
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

        do.call(update_settings, c(list(settings), out))
    }
    return(fun)
}




