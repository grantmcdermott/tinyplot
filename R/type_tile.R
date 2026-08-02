#' Tile (heatmap) plot type
#'
#' @description Type function for tile plots, i.e. a grid of rectangles whose
#'   fill colour encodes a third variable. This is the standard building block
#'   of heatmaps, correlation matrices, and calendar plots. `type_heatmap` is an
#'   alias for `type_tile`.
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
#' @param width,height Numeric tile dimensions in data units. Both default to
#'   `1`, which produces contiguous tiles on categorical (or unit-spaced
#'   numeric) axes. Values below `1` inset the tiles, leaving gaps between them.
#'   Recycled across tiles, so a vector may be used for variable sizes.
#'
#' @examples
#' # It is recommended to use the dedicated "heatmap" theme for this type
#' tinytheme("heatmap")
#'
#' # Correlation matrix of the base `attitude` dataset. The `x` and `y`
#' # variables are factors, so tile bounds and axis labels are derived
#' # automatically.
#' catt = as.data.frame(cor(attitude))
#' catt = cbind(
#'   stack(catt),
#'   ind2 = factor(row.names(catt), levels = row.names(catt))
#' )
#'
#' tinyplot(ind2 ~ ind | values, data = catt, type = "tile")
#'
#' # slightly fancier version, where we suppress the legend but layer on the values
#' # as text
#' tinyplot(
#'   ind2 ~ ind | values, data = catt,
#'   type = "tile",
#'   legend = FALSE,
#'   xlab = NA, ylab = NA,
#'   main = "Correlation matrix of base attitude dataset"
#' )
#' tinyplot_add(type = "text", labels = round(catt$values, 2), col = "white")
#'
#' # aside: "heatmap" is an alias for "tile"
#' tinyplot(ind2 ~ ind | values, data = catt, type = "heatmap")
#'
#' # Pass scaled tile widths and heights through type_tile() for a gridded look
#' tinyplot(
#'   ind2 ~ ind | values, data = catt,
#'   type = type_tile(width = 0.9, height = 0.9)
#' )
#'
#' # Numeric axes work too, e.g. a (long-format) matrix of volcano heights
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
#' ## restore the default theme
#' tinytheme()
#'
#' @seealso \code{\link{type_rect}} for the lower-level rectangle type that
#'   `type_tile()` builds on, and [`tinytheme()`] for the companion `"heatmap"`
#'   theme.
#'
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

#' @rdname type_tile
#' @export
type_heatmap = type_tile


data_tile = function(width = 1, height = 1) {
  fun = function(settings, ...) {
    env2env(
      settings,
      environment(),
      c("datapoints", "xlabs", "ylabs", "xaxt", "yaxt", "bg", "fill", "null_by")
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
      c("datapoints", "xlabs", "ylabs", "xaxt", "yaxt", "bg")
    )
  }
  return(fun)
}
