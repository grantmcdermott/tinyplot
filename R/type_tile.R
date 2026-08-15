#' Tile and heatmap plot types
#'
#' @description Type functions for tile plots, i.e. a grid of rectangles whose
#'   fill colour encodes a third variable. `type_tile()` is the default building
#'   block for these gridded shapes, drawing the values exactly as supplied. It
#'   underpins heatmaps, correlation matrices, calendar plots, confusion
#'   matrices, and similar displays.
#'
#'   `type_heatmap()` is a specialised case that first rescales the fill values
#'   within each category of one axis. Reach for it when those values are not
#'   already on a common scale.
#'
#' @details Tile plots are specified as `z ~ x` with the fill variable passed as
#'   the `by` grouping, i.e. `tinyplot(y ~ x | z, type = "tile")`. The `x` and
#'   `y` variables may be factors, characters, or numerics; the `by` variable
#'   supplies the fill and will typically be numeric, yielding a continuous
#'   colour gradient and colourbar legend. Omitting `by` leaves the tiles
#'   unfilled, since there is nothing for the fill to encode; pass an explicit
#'   `fill` (or `bg`) if you want a uniform colour in that case.
#'
#'   Unlike the closely-related \code{\link{type_rect}}, which requires explicit
#'   `xmin`/`xmax`/`ymin`/`ymax` bounds, `type_tile()` derives the tile bounds
#'   for you: each tile is centred on its `x`/`y` position and extends
#'   `width/2` and `height/2` in each direction. Categorical axes are converted
#'   to consecutive integer positions and the axis tick labels are taken from
#'   the factor levels automatically.
#'
#'   Explicit bounds still take precedence. Passing any of `xmin`, `xmax`,
#'   `ymin`, or `ymax` leaves that dimension untouched, which is useful for
#'   irregular or unequal-width tiles (e.g. binned continuous data). Bounds may
#'   be given for one axis while the other is derived.
#'
#'   Note that tiles are opaque and drawn edge-to-edge, so the default axis
#'   padding and grid lines of most themes are redundant (and the grid is hidden
#'   behind the tiles in any case). We therefore ship a dedicated `"heatmap"`
#'   theme that removes the padding and grid, rotates the tick labels, and
#'   switches to a sequential palette. See [`tinytheme()`] and the Examples.
#'
#'   `type_heatmap()`'s `scale` argument is the analogue of the `scale` argument
#'   in base R's \code{\link[stats]{heatmap}}, and like the latter it z-scores
#'   along the chosen margin by default. Pass `method = "rescale"` to map each
#'   group onto the unit \[0, 1\] interval instead.
#'
#'   `type_heatmap()` also reverses the y-axis by default, so that the first
#'   row sits at the top, matching how one reads a matrix (and again cf. base
#'   R's `heatmap()` and \code{\link[graphics]{image}}). Pass an explicit `ylim`
#'   to override. `type_tile()` makes no such adjustment, since it draws the
#'   values exactly as supplied.
#'
#'   Either way, note that scaling along a margin necessarily discards the
#'   *relative* spread of each group: a narrow-range column will occupy as much
#'   of the colour ramp as a wide-range one, since both are divided by their own
#'   measure of spread. That is the price of making a matrix of incomparable
#'   units legible; use `scale = "none"` (or `type_tile()`) when preserving
#'   cross-group magnitudes matters more.
#'
#' @param width,height Numeric tile dimensions in data units. Both default to
#'   `1`, which produces contiguous tiles on categorical (or unit-spaced
#'   numeric) axes. Values below `1` inset the tiles, leaving gaps between them.
#'   Recycled across tiles, so a vector may be used for variable sizes.
#' @examples
#' # It is recommended to use the dedicated "heatmap" theme for tile plots
#' tinytheme("heatmap")
#'
#' #
#' ## type_tile ----
#' 
#' # Correlation matrix of the base `attitude` dataset in "long" form.
#' catt = as.data.frame(as.table(cor(attitude)), responseName = "Correlation")
#'
#' tinyplot(Var1 ~ Var2 | Correlation, data = catt, type = "tile")
#'
#' # fancier version where we reverse the y-axis (to mimic the usual correlation
#' # matrix layout), add white borders around each tile, and suppress the legend
#' # but layer on the values as text
#' tinyplot(
#'   Var1 ~ Var2 | Correlation, data = catt,
#'   type = "tile",
#'   col = "white",
#'   legend = FALSE,
#'   main = "Correlation matrix of base attitude dataset",
#'   xlab = NA, ylab = NA,
#'   ylim = "rev"
#' )
#' tinyplot_add(type = "text", labels = round(catt$Correlation, 2))
#'
#' # Pass scaled tile widths and heights through type_tile() for a gridded look
#' tinyplot(
#'   Var1 ~ Var2 | Correlation, data = catt,
#'   type = type_tile(width = 0.9, height = 0.9)
#' )
#' 
#' # It doesn't really work for this example, but you can easily switch to a
#' # diverging palettes if it makes sense for your data
#' tinyplot(
#'   Var1 ~ Var2 | Correlation, data = catt,
#'   type = type_tile(width = 0.9, height = 0.9),
#'   palette = "tropic"
#' )
#'
#' # Numeric axes work too, e.g. a (reshaped long) data.frame of volcano heights
#' volc = data.frame(
#'   x         = as.vector(row(volcano)),
#'   y         = as.vector(col(volcano)),
#'   elevation = as.vector(volcano)
#' )
#' tinyplot(
#'   y ~ x | elevation, data = volc,
#'   type = "tile",
#'   theme = "void", # void theme looks better with this numeric example
#'   xlab = NA, ylab = NA,
#'   main = "Maunga Whau volcano"
#' )
#'
#' #
#' ## type_heatmap ----
#' 
#' # Raw data matrices are usually dominated by their largest-magnitude column.
#' # `type_heatmap()` can rescale within each column to make the rest legible.
#' mt = as.data.frame(as.table(as.matrix(mtcars)))
#'
#' # first, the unscaled version: only `disp` and `hp` are visible
#' tinyplot(
#'   Var1 ~ Var2 | Freq, data = mt,
#'   type = "heatmap",
#'   xlab = NA, ylab = NA
#' )
#'
#' # and now scaled within each x variable (i.e., column). The default is to
#' # z-score, matching base R's `heatmap(scale = "column")`.
#' tinyplot(
#'   Var1 ~ Var2 | Freq, data = mt,
#'   type = type_heatmap(scale = "x"),
#'   xlab = NA, ylab = NA
#' )
#'
#' # `method = "rescale"` maps each column onto [0, 1] instead. This uses the
#' # colour ramp more fully, at the cost of pinning every column's min and max to
#' # the same two colours.
#' tinyplot(
#'   Var1 ~ Var2 | Freq, data = mt,
#'   type = type_heatmap(scale = "x", method = "rescale"),
#'   xlab = NA, ylab = NA
#' )
#'
#' #
#' ## tips ----
#' 
#' # tip 1: use tinyplot.matrix() directly to avoid reshaping
#' tinyplot(as.matrix(mtcars), type = type_heatmap(scale = "x"), col = "white")
#'
#' # tip 2: use per-axis tick label scaling (cex) for dense heatmaps
#' tinyplot(as.matrix(mtcars), type = type_heatmap(scale = "x"), col = "white",
#'          theme = list("heatmap", cex.yaxs = 0.75, cex.xaxs = 1.5))
#' 
#' ## restore the default theme
#' tinytheme()
#'
#' @seealso \code{\link{type_rect}} for the lower-level rectangle type that
#'   `type_tile()` builds on, and [`tinytheme()`] for the companion `"heatmap"`
#'   theme.
#'
#' @order 1
#' @export
type_tile = function(width = 1, height = 1) {
  assert_numeric(width)
  assert_numeric(height)
  out = list(
    draw = draw_rect(),
    data = data_tile(width = width, height = height),
    name = "tile"
  )
  class(out) = "tinyplot_type"
  return(out)
}


## type_heatmap(), a specialised case of type_tile(), lives in type_heatmap.R
## but documents itself onto this same help page via `@rdname type_tile`.
## data_tile()'s `scale`/`method` arguments below exist to serve it; type_tile()
## itself has no `scale` argument and always draws the values as supplied.


data_tile = function(
    width = 1, height = 1, scale = "none", method = "zscore") {
  fun = function(settings, ...) {
    env2env(
      settings,
      environment(),
      c(
        "datapoints", "xlabs", "ylabs", "xaxt", "yaxt", "bg", "fill",
        "null_by", "by", "by_dep", "legend_args"
      )
    )

    # Tiles are a filled mark: the `by` variable encodes the *fill*, not the
    # outline. Default `bg` to the palette so that a bare `type = "tile"` is
    # filled, matching how the user would otherwise have to spell it out with
    # `fill = "by"`. An explicit bg/fill still wins.
    #
    # Without a `by` variable there is nothing for the fill to encode, so leave
    # the tiles unfilled (cf. type_rect()) rather than flooding every one with
    # the same flat colour, which would read as a solid black grid.
    if (is.null(bg) && is.null(fill) && !isTRUE(null_by)) bg = "by"

    # Optional z-scoring of the fill values within each x (or y) category, cf.
    # `heatmap(scale=)`. Must happen *before* the factor -> integer conversion
    # below, which needs the axis variables still as factors to group on. Note
    # this keys off the axis as written in the formula: flip_datapoints() runs
    # later in the pipeline, so `flip` does not invert the meaning.
    if (!identical(scale, "none")) {
      if (isTRUE(null_by) || !is.numeric(datapoints[["by"]])) {
        # Nothing numeric to standardize. Also catches `facet = "by"`, which
        # coerces `by` to a factor upstream in sanitize_facet(). Warn rather
        # than error: the plot is still perfectly drawable, just unscaled.
        warning(
          "`type_tile(scale=)` requires a numeric `by` (fill) variable. ",
          "Ignoring `scale`.",
          call. = FALSE
        )
      } else {
        # Group on the axis position *and* the facet, so each panel is scaled
        # independently. Pooling across panels defeats the purpose: a panel on
        # a different order of magnitude would pin its whole range to one end
        # of the ramp and lose all within-panel structure. `datapoints$facet`
        # is always present (a constant "" when unfaceted).
        grp = interaction(
          datapoints[[scale]], datapoints[["facet"]], drop = TRUE
        )
        z = scale_by_group(datapoints[["by"]], grp, method = method)
        flat = attr(z, "flat")
        if (length(flat) > 0L) {
          warning(
            sprintf(
              paste(
                "No variation within %d %s of `%s`;",
                "set to the scale midpoint: %s"
              ),
              length(flat), if (length(flat) > 1L) "groups" else "group",
              scale, paste(flat, collapse = ", ")
            ),
            call. = FALSE
          )
        }
        if (anyNA(z)) {
          warning(
            "Missing values in `by`; those tiles are left unfilled.",
            call. = FALSE
          )
        }
        attributes(z) = NULL
        # Both slots are needed: the tile fills read `datapoints$by`, but the
        # gradient legend's tick labels come from the bare `by`, so updating
        # only one would leave the colourbar numbers disagreeing with the
        # colours (cf. type_hexbin()).
        datapoints[["by"]] = z
        by = z
        # A scaled fill is no longer in the units of the `by` variable, so a
        # legend still titled e.g. "Freq" would be actively misleading. Note the
        # formula method has already pre-filled the title with the variable
        # name, so annotate whatever is there rather than only filling a blank.
        # The grepl() guard keeps this idempotent under tinyplot_add() replay.
        sfx = if (identical(method, "zscore")) "(z-score)" else "(rescaled)"
        ttl = legend_args[["title"]] %||% by_dep
        if (is.character(ttl) && length(ttl) == 1L && nzchar(ttl) &&
            !grepl(sfx, ttl, fixed = TRUE)) {
          legend_args[["title"]] = paste0(ttl, "\n", sfx)
        }
      }
    }

    # A categorical axis carries its own tick labels, so convert to consecutive
    # integer positions and hand the levels off to the axis machinery. Numeric
    # axes are already positional and keep their default (computed) ticks.
    for (ax in c("x", "y")) {
      v = datapoints[[ax]]
      if (is.null(v) || !(is.factor(v) || is.character(v))) next
      if (!is.factor(v)) v = factor(v)
      labs = seq_along(levels(v))
      names(labs) = levels(v)
      datapoints[[ax]] = as.numeric(v)
      if (ax == "x") {
        xlabs = xlabs %||% labs
        # cf. data_barplot(): "l" keeps the labels but drops the tick marks,
        # which have no meaning for a categorical position.
        if (identical(xaxt, "s")) xaxt = "l"
      } else {
        ylabs = ylabs %||% labs
        if (identical(yaxt, "s")) yaxt = "l"
      }
    }

    # Derive the tile bounds, but never clobber user-supplied ones: an explicit
    # xmin/xmax (or ymin/ymax) is how irregular or unequally-sized tiles get
    # specified, so each axis is derived only if *both* of its bounds are absent.
    if (is.null(datapoints[["xmin"]]) && is.null(datapoints[["xmax"]])) {
      w = rep_len(width, nrow(datapoints)) / 2
      datapoints[["xmin"]] = datapoints[["x"]] - w
      datapoints[["xmax"]] = datapoints[["x"]] + w
    }
    if (is.null(datapoints[["ymin"]]) && is.null(datapoints[["ymax"]])) {
      h = rep_len(height, nrow(datapoints)) / 2
      datapoints[["ymin"]] = datapoints[["y"]] - h
      datapoints[["ymax"]] = datapoints[["y"]] + h
    }

    # Match type_rect()'s legend keys for the discrete case. A numeric `by`
    # renders a colourbar instead, where these are simply ignored.
    settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
    settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
    settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% par("lwd")
    settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% 0
    settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
    settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

    env2env(
      environment(),
      settings,
      c(
        "datapoints", "xlabs", "ylabs", "xaxt", "yaxt", "bg", "by",
        "legend_args"
      )
    )
  }
  return(fun)
}
