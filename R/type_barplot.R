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
#' @param xlevels,xord arguments controlling the order of the `x` variable, and
#'   hence of the x-axis. Supply one or the other; if both arguments are
#'   provided, `xlevels` takes precedence and `xord` is silently ignored.
#'
#'   - `xlevels` specifies the levels _literally_, either a character vector of
#'   level names in the desired order (e.g., `c("C", "B", "A")`), or a numeric
#'   vector of the corresponding level indexes (e.g. `3:1`).
#'
#'   - `xord` instead accepts a keyword or custom function, which then _derives_
#'   the order from the data. Options are:
#'
#'     - `"desc(ending)"` and `"asc(ending)"` rank (sort) the categories by bar
#'     height, tallest or shortest first. Both the abbreviated and long form
#'     strings are permitted, as are the `"decreasing"` and `"increasing"`
#'     aliases. Note that the ranking is applied to the *aggregated* bars, i.e.
#'     whatever `FUN` produced, rather than the underlying rows. With `by`
#'     groups or facets, a single ordering is computed and shared across all of
#'     them, by summing each category's bars over every group and facet. For
#'     stacked bars that sum is the height of the full stack; with
#'     `beside = TRUE` it is the group total rather than any individual bar. (A
#'     factor carries one level order, so a per-facet ranking is not
#'     expressible.)
#'     - `"asis"` or `"rev"` permute the existing levels without consulting the
#'     data at all. The former takes the categories in the order that they
#'     appear in the data, while the latter reverses the current level order.
#'     - a custom function that determines both the ranking statistic and its
#'     direction. The statistic is always sorted in ascending order, so
#'     `function(y) -median(y)` ranks by median, largest first.
#'
#'   Note that a numeric `x` is coerced to a factor before the bars are drawn,
#'   so it is reordered like any other categorical variable.
#'   Each argument defaults to `NULL`, i.e. keep the existing factor levels.
#' @param offset optional specification for shifting bar baselines, accepting
#'   one of two distinct forms. See the Examples for illustrations of both.
#' 
#'   - *Positions* via an unnamed numeric scalar or vector. Bars start at the
#'   offset value(s) rather than zero, matched per x-level after any `xlevels`
#'   or `xord` reordering (a scalar is applied to all bars). Useful for
#'   waterfall charts.
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
#' @param lighten logical. Should the fills use a lighter, opaque tint of the
#'   series colour(s)? Default is `TRUE`, which keeps single- and multi-group
#'   displays consistent and lets the fill read cleanly over grid lines. Set to
#'   `FALSE` to use the fully-saturated palette colour(s) instead.
#' @param xaxlabels \[Deprecated\] a character vector with the axis labels for
#'   the `x` variable. Use the top-level `xaxl` argument instead, which now
#'   accepts a dictionary mapping old labels to new ones, and applies
#'   consistently across plot types. This argument will be removed in a future
#'   release.
#'
#' @examples
#' #
#' ## Basic use (raw values)
#' 
#' tinyplot(GNP ~ Year, data = longley, type = "barplot")
#' 
#' tinyplot(demand ~ Time, data = BOD, type = "bar") # "bar" is a shorthand
#' tinyplot_add(type = "text", pos = 3, xpd = NA)    # add y values as text
#' 
#' #
#' ## Aggregated vs grouped values (multiple ys per x)
#' 
#' # each person receives two drugs
#' sleep2 = transform(sleep, drug = group) # less misleading name
#' 
#' # default aggregation FUN is mean
#' tinyplot(
#'   extra ~ ID, data = sleep2,
#'   type = "barplot",
#'   main = "Mean extra sleep from 2 soporiphic drugs"
#' )
#' # switch to diff (answers a more relevant q: who benefits most from drug 2?)
#' tinyplot(
#'   extra ~ ID, data = sleep2,
#'   type = "barplot", FUN = diff,
#'   main = "Sleep gain (drug 2 vs drug 1)"
#' )
#' # we can sort in descending (or ascending) order too
#' tinyplot(
#'   extra ~ ID, data = sleep2,
#'   type = "barplot", FUN = diff, xord = "desc",
#'   main = "Sleep gain (drug 2 vs drug 1), ordered"
#' )
#' 
#' # of course, we don't have to aggregate if we specify groups (stacked or non)
#' tinyplot(extra ~ ID | drug, data = sleep2, type = "barplot", beside = TRUE)

#' # Note: We used automatic argument passing for 'xord', `FUN`, etc. above. But
#' # this wouldn't work for `width`, since it would conflict with the top-level
#' # `tinyplot(..., width = <width>)` argument. It's safer to pass these args
#' # through the `type_barplot()` functional equivalent...
#' 
#' tinyplot(
#'   extra ~ ID | drug, data = sleep2,
#'   type = type_barplot(beside = TRUE, xord = "desc", width = 0.5)
#' )
#' 
#' #
#' ## matrix method (no formula required)
#' 
#' tinyplot(VADeaths, type = "barplot")
#' tinyplot(VADeaths, type = "barplot", beside = TRUE)
#' # etc. see ?tinyplot.matrix
#' 
#' #
#' ## Frequency tables
#' 
#' # No y variable (frequency calculated on the fly)
#' tinyplot(~ cyl, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE)
#' 
#' # Fancy frequency table (y = frequency aleady computed)
#' tinyplot(
#'   Freq ~ Sex | Survived, data = as.data.frame(Titanic),
#'   facet = ~ Class, facet.args = list(nrow = 1),
#'   type = "barplot", beside = TRUE, flip = TRUE,
#'   theme = "clean2"
#' )
#' 
#' #
#' ## Centering
#'
#' # Centered barplot for conditional proportions of "dark" (black/brown) vs.
#' # "fair" (red/blond) hair color, conditional on eye color and sex.
#' # Aside: use `lighten = FALSE` to avoid lightening the bar fill colors.
#' hec = as.data.frame(proportions(HairEyeColor, 2:3))
#' hcols = c("black", "sienna", "indianred", "goldenrod")
#' tinyplot(
#'   Freq ~ Eye | Hair, data = hec,
#'   facet = ~ Sex, facet.args = list(ncol = 1),
#'   type = type_barplot(center = TRUE, lighten = FALSE),
#'   flip = TRUE, yaxl = "percent",
#'   theme = list("clean2", palette.qualitative = hcols)
#' )
#' tinyplot_add(type = "vline", col = "white")
#'
#' #
#' ## Offset examples
#'
#' # 1. Waterfall plot
#' d = data.frame(item = c("Sales", "Services", "Costs", "Returns", "TOTAL"),
#'                value = c(100, 40, -80, -10, 50))
#' d$item = factor(d$item, levels = d$item)
#' d$offset = c(0, cumsum(d$value[1:3]), 0)
#' tinyplot(
#'   value ~ item | I(value < 0), data = d,
#'   type = type_barplot(offset = d$offset, lighten = FALSE),
#'   col = NA, # (optional: turn off border)
#'   legend = FALSE
#' )
#' tinyplot_add(type = type_vline(4.5), lty = 2, col = "grey50")
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
#'   type = type_barplot(center = TRUE, offset = "Unsure", lighten = FALSE),
#'   flip = TRUE, xlab = NA, ylab = NA, yaxl = "percent",
#'   legend = list("top!", title = FALSE),
#'   theme = list("clean2", palette.qualitative = pal),
#'   main = "Hypothetical Likert example with category offset"
#' )
#' tinyplot_add(type = "vline")
#' tinyplot_add(type = "vline", v = 1, lty = 2)
#'
#' @export
type_barplot = function(width = 5/6, beside = FALSE, center = FALSE, offset = NULL, FUN = NULL, xlevels = NULL, xord = NULL, drop.zeros = FALSE, lighten = TRUE, xaxlabels = NULL) {
  if (!is.null(xaxlabels)) {
    warning(
      "'xaxlabels' is deprecated; use the top-level 'xaxl' argument instead, ",
      "e.g. tinyplot(..., xaxl = c(old = \"new\")) to rename particular ",
      "categories, or xaxl = function(x) ... to compute the labels.",
      call. = FALSE
    )
  }
  out = list(
    data = data_barplot(width = width, beside = beside, center = center, offset = offset, FUN = FUN, xlevels = xlevels, xord = xord, xaxlabels = xaxlabels, drop.zeros = drop.zeros, lighten = lighten),
    draw = draw_rect(),
    name = "barplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom stats aggregate
data_barplot = function(width = 5/6, beside = FALSE, center = FALSE, offset = NULL, FUN = NULL, xlevels = NULL, xord = NULL, xaxlabels = NULL, drop.zeros = FALSE, lighten = TRUE) {
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
          if (is.null(xlab) || identical(xlab, "Index")) xlab = ylab
          if (is.null(settings$y_dep) && is.null(ylab)) ylab = "Count"
          datapoints$y = numeric(nrow(datapoints))          
          if (!is.null(FUN)) warning("without 'y' variable 'FUN' specification is ignored")
          FUN = length
        } else {
          if (is.null(FUN)) FUN = function(x, ...) mean(x, ..., na.rm = TRUE)
        }
        if (!is.factor(datapoints$x)) datapoints$x = factor(datapoints$x)
        datapoints$x = sanitize_xlevels(datapoints$x, xlevels)
        ## "asis" means "the order the categories appear in the data", and the
        ## aggregate() below destroys that by sorting on the grouping columns.
        ## It consults no `y`, so apply it here while the row order still
        ## survives. The ranking keywords have the opposite requirement -- they
        ## must see the aggregated bars -- and so stay below.
        if (identical(xord, "asis") && is.null(xlevels)) {
          datapoints$x = sanitize_ord(
            datapoints$x, NULL, NULL,
            xord, arg = "xord", keywords = ord_keywords_scalar
          )
        }
        if (!is.null(xaxlabels)) levels(datapoints$x) = xaxlabels
        datapoints = aggregate(datapoints[, "y", drop = FALSE], datapoints[, c("x", "by", "facet")], FUN = FUN, drop = FALSE)
        datapoints$y[is.na(datapoints$y)] = 0 #FIXME: always?#
        if (!is.factor(datapoints$by)) datapoints$by = factor(datapoints$by)
        if (!is.factor(datapoints$facet)) datapoints$facet = factor(datapoints$facet)

        ## `xord` ranks on the *aggregated* bars, so it has to run after the
        ## aggregate() above -- ranking the raw cells would sort on sums while
        ## the plot draws whatever FUN produced. It also has to run before the
        ## `offset` block below, which is keyed positionally by x-level.
        if (!is.null(xord) && !identical(xord, "asis") && is.null(xlevels)) {
          datapoints$x = sanitize_ord(
            datapoints$x, datapoints$y, NULL,
            xord, arg = "xord", keywords = ord_keywords_scalar
          )
          datapoints = datapoints[order(datapoints$facet, datapoints$by, datapoints$x), , drop = FALSE]
        }
        
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

        ## fill lightening (see by_bg)
        settings[["lighten"]] = lighten

        ## default color palette
        ngrps = length(unique(datapoints$by))
        if (ngrps == 1L && null_palette) {
          # With a theme palette active, leave bg = NULL so the fill tracks
          # the resolved border colour (see by_bg). Otherwise use the neutral
          # "lightgray" shared by all single-group area fills (matches base R
          # hist()/boxplot()).
          if (is.null(bg) && is.null(get_tpar("palette.qualitative", default = NULL))) bg = "lightgray"
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

