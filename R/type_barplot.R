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
#' @param center logical or numeric. In case of stacked barplots (`beside = FALSE`)
#'   should the bars be centered (or all start at zero, default)? If set to
#'   `TRUE` the center is at the mid-point of the middle category (in case of
#'   uneven number of categories) or between the two middle categories (in case
#'   of an even number). Additionally it is possible to set `center = 2` or
#'   `center = 2.5` to indicate that centering should be after the second category
#'   or the mid-way in the third category, respectively.
#' @param FUN a function to compute the summary statistic for `y` within each
#'   group of `x` in case of using a two-sided formula `y ~ x` (default: mean).
#' @param xlevels a character or numeric vector specifying the ordering of the
#'   levels of the `x` variable (if character) or the corresponding indexes
#'   (if numeric) for the plot.
#' @param xaxlabels a character vector with the axis labels for the `x` variable,
#'   defaulting to the levels of `x`.
#' @param offset optional specification for shifting bar baselines, accepting
#'   one of two distinct forms. See the Examples for illustrations of both.
#' 
#'   - *Positions* via an unnamed numeric scalar or vector. Bars start at the
#'   offset value(s) rather than zero, matched per x-level after any `xlevels`
#'   reordering (a scalar is applied to all bars). Useful for waterfall charts.
#'   The positional form cannot be combined with `center`.
#'   - *Category* via a character vector such as `offset = "Unsure"`, or a
#'   named numeric vector such as `offset = c(Unsure = 1.1)`. The named
#'   level(s) of the `by` grouping are "set aside", i.e. pulled out of the
#'   (optionally centered) stack and drawn as standalone bars. This is useful
#'   for diverging/Likert plots where a neutral category (e.g. "Unsure") is
#'   shown apart from the diverging stack. The category form requires a `by`
#'   grouping and `beside = FALSE`, but can be combined with `center`.
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
#' # Reorder x variable categories either by their character levels or numeric indexes
#' tinyplot(~ cyl, data = mtcars, type = "barplot", xlevels = c("8", "6", "4"))
#' tinyplot(~ cyl, data = mtcars, type = "barplot", xlevels = 3:1)
#' 
#' # Note: Above we used automatic argument passing for `beside`. But this
#' # wouldn't work for `width`, since it would conflict with the top-level
#' # `tinyplot(..., width = <width>)` argument. It's safer to pass these args
#' # through the `type_barplot()` functional equivalent.
#' tinyplot(
#'   ~ cyl | vs, data = mtcars, fill = 0.2,
#'   type = type_barplot(beside = TRUE, drop.zeros = TRUE, width = 0.65)
#' )
#' 
#' # Example for numeric y aggregated by x (default: FUN = mean) + facets
#' tinyplot(
#'   extra ~ ID | group, facet = "by", data = sleep,
#'   type = "barplot", fill = 0.6,
#'   theme = "clean2"
#' )
#' 
#' # Fancy frequency table:
#' tinyplot(
#'   Freq ~ Sex | Survived, data = as.data.frame(Titanic),
#'   facet = ~ Class, facet.args = list(nrow = 1),
#'   type = "barplot", flip = TRUE, fill = 0.6,
#'   theme = "clean2"
#' )
#'
#' # Centered barplot for conditional proportions of hair color (black/brown vs.
#' # red/blond) given eye color and sex
#' hec = as.data.frame(proportions(HairEyeColor, 2:3))
#' hcols = c("black", "sienna", "indianred", "goldenrod")
#' tinyplot(
#'   Freq ~ Eye | Hair, data = hec,
#'   facet = ~ Sex, facet.args = list(ncol = 1),
#'   type = "barplot", center = TRUE,
#'   flip = TRUE, yaxl = "percent",
#'   theme = list("clean2", palette.qualitative = hcols)
#' )
#'
#' # Use cases for the `offset` argument
#'
#' # 1. Waterfall plot
#' d = data.frame(item = c("Sales", "Services", "Costs", "Returns", "TOTAL"),
#'                value = c(100, 40, -80, -10, 50))
#' d$item = factor(d$item, levels = d$item)
#' d$offset = c(0, cumsum(d$value[1:3]), 0)
#' tinyplot(
#'   value ~ item | I(value < 0), data = d,
#'   type = type_barplot(offset = d$offset), legend = FALSE
#' )
#' tinyplot_add(type = type_vline(4.5), lty = 2)
#'
#' # 2. Diverging/Likert layout: a character (or named numeric) offset "sets
#' # aside" the named category, pulling it out of the centered stack and drawing
#' # it as a standalone bar. Here a neutral "Unsure" response is shown apart from
#' # the diverging agree/disagree scale.
#' lik = expand.grid(
#'   question = c("Pay", "Workload", "Manager", "Culture"),
#'   response = c("Strong disagree", "Disagree", "Agree", "Strong agree", "Unsure")
#' )
#' lik$response = factor(lik$response, levels = unique(lik$response))
#' lik$share = c( # proportions summing to 1 within each question
#'   .10, .25, .05, .15,
#'   .20, .30, .15, .20,
#'   .35, .20, .40, .30,
#'   .25, .15, .35, .20,
#'   .10, .10, .05, .15
#' )
#' # diverging palette: reds (disagree) -> blues (agree), grey for "Unsure"
#' pal = c("#b2182b", "#ef8a62", "#67a9cf", "#2166ac", "grey")
#' tinyplot(
#'   share ~ question | response, data = lik,
#'   type = "barplot", center = TRUE, offset = "Unsure",
#'   flip = TRUE, xlab = NA, ylab = NA, yaxl = "percent",
#'   legend = list("top!", title = NULL),
#'   theme = list("clean2", palette.qualitative = pal),
#'   main = "Hypothetical Likert example with category offset"
#' )
#' tinyplot_add(type = "vline")
#'
#' @export
type_barplot = function(width = 5/6, beside = FALSE, center = FALSE, offset = NULL, FUN = NULL, xlevels = NULL, xaxlabels = NULL, drop.zeros = FALSE) {
  out = list(
    data = data_barplot(width = width, beside = beside, center = center, offset = offset, FUN = FUN, xlevels = xlevels, xaxlabels = xaxlabels, drop.zeros = drop.zeros),
    draw = draw_rect(),
    name = "barplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom stats aggregate
data_barplot = function(width = 5/6, beside = FALSE, center = FALSE, offset = NULL, FUN = NULL, xlevels = NULL, xaxlabels = NULL, drop.zeros = FALSE) {
    fun = function(settings, ...) {
        env2env(
          settings,
          environment(),
          c(
            "datapoints", "null_by", "facet_by",
            "xlab", "ylab", "xlim", "ylim", "yaxl", "xaxt",
            "null_palette", "col", "bg"
          )
        )

        ## tabulate/aggregate datapoints
        if (is.null(datapoints$y)) {
          if (is.null(xlab) || xlab == "Index") xlab = ylab
          if (is.null(settings$y_dep) && is.null(ylab)) ylab = "Count"
          datapoints$y = numeric(nrow(datapoints))          
          if (!is.null(FUN)) warning("without 'y' variable 'FUN' specification is ignored")
          FUN = length
        } else {
          if (is.null(FUN)) FUN = function(x, ...) mean(x, ..., na.rm = TRUE)
        }
        if (!is.factor(datapoints$x)) datapoints$x = factor(datapoints$x)
        if (!is.null(xlevels)) {
          xlevels = if(is.numeric(xlevels)) levels(datapoints$x)[xlevels] else xlevels
          if (anyNA(xlevels) || !all(xlevels %in% levels(datapoints$x))) warning("not all 'xlevels' correspond to levels of 'x'")
          datapoints$x = factor(datapoints$x, levels = xlevels)
        }
        if (!is.null(xaxlabels)) levels(datapoints$x) = xaxlabels
        datapoints = aggregate(datapoints[, "y", drop = FALSE], datapoints[, c("x", "by", "facet")], FUN = FUN, drop = FALSE)
        datapoints$y[is.na(datapoints$y)] = 0 #FIXME: always?#
        if (!is.factor(datapoints$by)) datapoints$by = factor(datapoints$by)
        if (!is.factor(datapoints$facet)) datapoints$facet = factor(datapoints$facet)
        
        ## `offset` accepts two distinct forms:
        ##  - unnamed numeric -> positional, keyed by x-level (waterfall)
        ##  - character or *named* numeric -> keyed by `by`-level: those groups
        ##    are "set aside", i.e. pulled out of the (optionally centered) stack
        ##    and drawn as standalone bars (diverging/Likert layout).
        aside = NULL # named numeric of set-aside `by`-levels -> baseline value
        if (!is.null(offset)) {
          offset_bylevel = is.character(offset) ||
            (is.numeric(offset) && !is.null(names(offset)) && any(nzchar(names(offset))))
          if (offset_bylevel) {
            if (isTRUE(null_by)) {
              stop("a character or named 'offset' requires a 'by' grouping variable")
            }
            if (isTRUE(facet_by)) {
              stop("a character or named 'offset' is not supported when 'facet' is the 'by' grouping")
            }
            if (beside) {
              stop("a character or named 'offset' requires stacked bars; set 'beside = FALSE'")
            }
            nm = if (is.character(offset)) offset else names(offset)
            bad = setdiff(nm, levels(datapoints$by))
            if (length(bad)) {
              stop(sprintf(
                "'offset' must name levels of the 'by' grouping; unknown: %s",
                paste(bad, collapse = ", ")
              ))
            }
            if (is.character(offset)) {
              ## auto-placement: baseline = max full-column total (incl. set-aside)
              col_tot = tapply(
                datapoints$y, interaction(datapoints$x, datapoints$facet),
                sum, na.rm = TRUE
              )
              base_auto = max(col_tot, na.rm = TRUE)
              aside = stats::setNames(rep(base_auto, length(offset)), offset)
            } else {
              aside = offset
            }
            offset = NULL # disable the positional post-hoc shift below
          } else {
            if (!is.numeric(offset)) stop("'offset' must be numeric")
            if (!isFALSE(center)) {
              warning("'offset' cannot be combined with 'center'; ignoring 'center'")
              center = FALSE
            }
            nx_levels = nlevels(datapoints$x)
            if (length(offset) == 1L) {
              offset = rep(offset, nx_levels)
            } else if (length(offset) != nx_levels) {
              stop(sprintf(
                "'offset' must be length 1 or %d (number of x levels), got %d",
                nx_levels, length(offset)
              ))
            }
          }
        }
        if (is.null(offset) && is.null(aside) && isFALSE(null_by) && isFALSE(facet_by) && !beside && any(datapoints$y < 0)) {
          warning("'beside' must be TRUE if there are negative 'y' values")
          beside = TRUE
        }
        if (beside & !isFALSE(center)) {
          warning("'center' is currently only supported for 'beside = FALSE'")
        }
        null_ylim = is.null(ylim)
        offset_sum = function(z, center = TRUE, na.rm = TRUE) {
          n = length(z)
          if (isFALSE(center) || n < 1L) return(0)
          mid = if (isTRUE(center)) n/2 else center
          z[floor(mid) + 1L] = (mid - floor(mid)) * z[floor(mid) + 1L]
          sum(z[0L:floor(mid) + 1L], na.rm = TRUE)
        }
        if (is.null(xlim)) xlim = c(1, nlevels(datapoints$x)) + c(-0.5, 0.5) * width
        if (is.null(ylim)) ylim = if (beside || length(unique(datapoints$by)) == 1L) {
          c(pmin(0, min(datapoints$y, na.rm = TRUE) * 1.02), pmax(0, max(datapoints$y, na.rm = TRUE) * 1.02))
        } else {
          is_off = if (is.null(aside)) rep(FALSE, nrow(datapoints)) else datapoints$by %in% names(aside)
          ## range of the centered/stacked retained (non-set-aside) categories
          stack_range = range(unlist(tapply(
            seq_len(nrow(datapoints)),
            interaction(datapoints$x, datapoints$facet),
            function(idx) {
              z = datapoints$y[idx]
              keep = !is_off[idx]
              zc = z; zc[!keep] = 0
              c(0, sum(zc, na.rm = TRUE)) - offset_sum(z[keep], center = center)
            }
          )))
          ## range of the standalone set-aside bars (stacked from their baselines)
          off_range = NULL
          if (any(is_off)) {
            base = aside[as.character(datapoints$by[is_off])]
            ytop = unlist(tapply(
              seq_len(sum(is_off)),
              interaction(datapoints$x[is_off], datapoints$facet[is_off]),
              function(j) base[j] + cumsum(datapoints$y[is_off][j])
            ))
            off_range = range(c(base, ytop), na.rm = TRUE)
          }
          range(c(stack_range, off_range), na.rm = TRUE) * 1.02
        }

        ## default color palette
        ngrps = length(unique(datapoints$by))
        if (ngrps == 1L && null_palette) {
          # With a theme palette active, leave bg = NULL so the fill tracks
          # the resolved border colour (see by_bg). Otherwise use neutral grey.
          if (is.null(bg) && is.null(get_tpar("palette.qualitative", default = NULL))) bg = "grey"
        } else {
          if (is.null(bg)) bg = "by"
        }

        ## calculate bar rectangles per facet 
        sdat = split(datapoints, datapoints$facet)
        datapoints = lapply(sdat, function(df)  {
          
          df = df[order(df$x), , drop = FALSE]
          nx = nlevels(df$x)
          nb = nlevels(df$by)
          
          if (beside) {
            xl = as.numeric(df$x) - width/2 + (as.numeric(df$by) - 1) * width/nb * as.numeric(!facet_by)
            xr = if (facet_by) xl + width else xl + width/nb
            yb = 0
            yt = df$y
          } else {
            is_off = if (is.null(aside)) rep(FALSE, nrow(df)) else df$by %in% names(aside)
            xl = as.numeric(df$x) - width/2
            xr = xl + width
            ## stack/center only the retained categories: set-aside rows are
            ## zeroed so they don't grow or shift the centered stack, but the
            ## centering midpoint is computed from the retained subset alone.
            cs = tapply(seq_len(nrow(df)), df$x, function(idx) {
              z = df$y[idx]
              keep = !is_off[idx]
              zc = z; zc[!keep] = 0
              cumsum(c(0, zc)) - offset_sum(z[keep], center = center)
            })
            yb = if (facet_by) 0 else unlist(lapply(cs, `[`, -(nb + 1L)))
            yt = if (facet_by) df$y else unlist(lapply(cs, `[`, -1L))
            ## set-aside rows: standalone bars stacked from their baseline value
            if (any(is_off)) {
              base = aside[as.character(df$by[is_off])]
              ot = unlist(tapply(
                seq_len(sum(is_off)),
                df$x[is_off],
                function(j) base[j] + cumsum(df$y[is_off][j])
              ))
              ob = ot - df$y[is_off]
              yb[is_off] = ob
              yt[is_off] = ot
            }
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

        # Apply offset: shift bar baselines after rectangle computation
        if (!is.null(offset)) {
          off = offset[as.numeric(datapoints$x)]
          datapoints$ymin = datapoints$ymin + off
          datapoints$ymax = datapoints$ymax + off
          if (null_ylim) {
            ylim = range(c(0, datapoints$ymin, datapoints$ymax), na.rm = TRUE) * 1.02
          }
        }

        if (!isFALSE(center)) {
          if (is.null(yaxl)) {
            yaxl = abs
          } else if (is.character(yaxl)) {
            yaxl = paste0("abs_", yaxl)
          }
        }

        axes = TRUE
        frame.plot = FALSE
        xaxs = "r"
        xaxt = if (xaxt == "s") "l" else xaxt
        yaxs = "i"
        
        # legend customizations
        settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% 0
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25
        
        env2env(environment(), settings, c(
          "datapoints",
          "xlab",
          "ylab",
          "xlim",
          "ylim",
          "axes",
          "xlabs",
          "frame.plot",
          "xaxs",
          "xaxt",
          "yaxl",
          "yaxs",
          "col",
          "bg"
        ))
    }
    return(fun)
}

