#' Barplot type
#'
#' @description Type function for producing barplots. For formulas of type
#'   `~ x` (without left-hand side) the barplot visualizes the counts (absolute
#'   frequencies) of the levels of `x`. For formulas of type `y ~ x` the value
#'   of `y` within each level of `x` is visualized, if necessary aggregated
#'   using some function (default: mean).
#'
#' @param width numeric, optional vector of bar widths. (The distance between
#'   the midpoints of the bars is always 1.)
#' @param beside logical. In case of a `by` grouping variable, should bars be
#'   juxtaposed? Default is to use stacked bars instead.
#' @param FUN a function to compute the summary statistic for `y` within each
#'   group of `x` in case of using a two-sided formula `y ~ x` (default: mean).
#' @param xlevels a character or numeric vector specifying in which order the
#'   levels of the `x` variable should be plotted.
#' @param drop.zeros logical. Should bars with zero height be dropped? If set
#'   to `FALSE` (default) a zero height bar is still drawn for which the border
#'   lines will still be visible.
#'
#' @examples
#' # Basic examples of frequency tables (without y variable)
#' tinyplot(~ cyl, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE)
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE, fill = 0.2)
#' 
#' # Note: Above we used automatic argument passing for `beside`. But this
#' # wouldn't work for `width`, since it would conflict with the top-level
#' # `tinyplot(..., width = <width>)` argument. It's safer to pass these args
#' # through the `type_barplot()` functional equivalent.
#' tinyplot(~ cyl | vs, data = mtcars, fill = 0.2,
#'   type = type_barplot(beside = TRUE, drop.zeros = TRUE, width = .65))
#'
#' tinytheme("clean2")
#' 
#' # Example for numeric y aggregated by x (default: FUN = mean) + facets
#' tinyplot(extra ~ ID | group, facet = "by", data = sleep,
#'   type = "barplot", beside = TRUE, fill = 0.6)
#' 
#' # Fancy frequency table:
#' tinyplot(Freq ~ Sex | Survived, facet = ~ Class, data = as.data.frame(Titanic),
#'   type = "barplot", facet.args = list(nrow = 1), flip = TRUE, fill = 0.6)
#'
#' tinytheme()
#' 
#' @export
type_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL, drop.zeros = FALSE) {
  out = list(
    data = data_barplot(width = width, beside = beside, FUN = FUN, xlevels = xlevels, drop.zeros = drop.zeros),
    draw = draw_rect(),
    name = "barplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom stats aggregate
data_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL, drop.zeros = FALSE) {
    fun = function(datapoints, col, bg, lty, lwd, palette, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, xaxt = NULL, yaxt = NULL, axes = TRUE, facet_by = NULL, ...) {

        ## tabulate/aggregate datapoints
        if (is.null(datapoints$y)) {
          xlab = ylab
          ylab = "Count"
          
          datapoints$y = numeric(nrow(datapoints))          
          if (!is.null(FUN)) warning("without 'y' variable 'FUN' specification is ignored")
          FUN = length
        } else {
          if (is.null(FUN)) FUN = function(x, ...) mean(x, ..., na.rm = TRUE)
        }
        if (!is.factor(datapoints$x)) datapoints$x = factor(datapoints$x)
        if (!is.null(xlevels)) datapoints$x = factor(datapoints$x, levels = if(is.numeric(xlevels)) levels(x)[xlevels] else xlevels)
        datapoints = aggregate(datapoints[, "y", drop = FALSE], datapoints[, c("x", "by", "facet")], FUN = FUN, drop = FALSE)
        datapoints$y[is.na(datapoints$y)] = 0 #FIXME: always?#
        if (!is.factor(datapoints$by)) datapoints$by = factor(datapoints$by)
        if (!is.factor(datapoints$facet)) datapoints$facet = factor(datapoints$facet)
        
        if (!beside && any(datapoints$y < 0)) {
          warning("'beside' must be TRUE if there are negative 'y' values")
          beside = TRUE
        }
        if (is.null(xlim)) xlim = c(1, length(levels(datapoints$x))) + c(-0.5, 0.5) * width
        if (is.null(ylim)) ylim = if (beside || length(unique(datapoints$by)) == 1L) {
          c(pmin(0, min(datapoints$y, na.rm = TRUE) * 1.02), pmax(0, max(datapoints$y, na.rm = TRUE) * 1.02))
        } else {
          c(0, max(tapply(datapoints$y, interaction(datapoints$x, datapoints$facet), sum, na.rm = TRUE)) * 1.02)
        }

        ## default color palette
        ngrps = length(unique(datapoints$by))
        if (ngrps == 1L && is.null(palette)) {
          if (is.null(col)) col = par("fg")
          if (is.null(bg)) bg = "grey"
        } else {
          if (is.null(bg)) bg = "by"
        }

        ## calculate bar rectangles per facet 
        sdat = split(datapoints, datapoints$facet)
        datapoints = lapply(sdat, function(df)  {
          
          df = df[order(df$x), , drop = FALSE]
          nx = length(levels(df$x))
          nb = length(levels(df$by))
          
          if (beside) {        
            xl = as.numeric(df$x) - width/2 + (as.numeric(df$by) - 1) * width/nb * as.numeric(!facet_by)
            xr = if (facet_by) xl + width else xl + width/nb
            yb = 0
            yt = df$y
          } else {
            cs = tapply(df$y, df$x, function(z) cumsum(c(0, z)))
            xl = as.numeric(df$x) - width/2
            xr = xl + width
            yb = if (facet_by) 0 else unlist(lapply(cs, `[`, -(nb + 1L)))
            yt = if (facet_by) df$y else unlist(lapply(cs, `[`, -1L))
          }
          
          df$xmin = xl
          df$xmax = xr
          df$ymin = yb
          df$ymax = yt
          df$nx = nx
          
          if (drop.zeros) {
            yb = rep_len(yb, length(yt))
            yok = abs(yt - yb) > 0
            df = df[yok,  , drop = FALSE]
          }
          
          return(df)
        })
        datapoints = do.call("rbind", datapoints)
        nx = datapoints$nx[1]
        datapoints$nx = NULL
        xlabs = 1L:nx
        names(xlabs) = levels(datapoints$x)
        
        out = list(
          datapoints = datapoints,
          xlab = xlab,
          ylab = ylab,
          xlim = xlim,
          ylim = ylim,
          axes = FALSE, #FIXME
          axes = TRUE,
          xlabs = xlabs, 
          frame.plot = FALSE,
          xaxs = "r",
          xaxt = if (xaxt == "s") "l" else xaxt,
          yaxs = "i",
          col = col,
          bg = bg
        )
        return(out)
    }
    return(fun)
}

