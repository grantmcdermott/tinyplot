#' @rdname type_tile
#' @param scale Character. Should the `by` (fill) values be scaled *within*
#'   each category of one axis? One of `"none"` (default, i.e. the raw values
#'   are used), `"x"`, or `"y"`. Scaling is what makes a raw matrix legible
#'   when its variables span very different magnitudes: left alone, the
#'   largest-magnitude column monopolises the entire colour ramp. See Examples.
#'
#'   Note that `"x"` and `"y"` refer to the axes *as written in the formula*,
#'   i.e. before any `flip = TRUE` is applied. We deliberately avoid base R's
#'   `"row"`/`"column"` wording, since a tile's position depends on which
#'   variable the user placed where in the formula, so there is no fixed matrix
#'   orientation to refer to.
#'
#'   Rescaling is computed independently per facet; pooling across facets would
#'   pin a panel on a different scale to one end of the ramp and lose its
#'   internal structure. Since rescaled values are no longer in the units of the
#'   `by` variable, the legend title is annotated accordingly.
#' @param method Character. How should the values be rescaled, if `scale` is not
#'   `"none"`? Either `"zscore"` (default) to centre each group and divide by its
#'   standard deviation, or `"rescale"` to map each group onto the unit interval
#'   \[0, 1\]. Ignored when `scale = "none"`.
#'
#'   `"zscore"` matches base R's \code{\link[stats]{heatmap}} and keeps values
#'   comparable across groups, since `-1` means "one standard deviation below
#'   this group's mean" everywhere. `"rescale"` instead pins every group's
#'   minimum and maximum to the ends of the colour ramp, which uses the palette
#'   more fully but makes the endpoints an artefact of the transform rather than
#'   a feature of the data.
#'
#'   Groups with no spread---a constant column, or a single tile---would divide
#'   by zero, so they are set to the midpoint of the target range (`0.5` and `0`
#'   respectively) and a warning is emitted.
#'
#' @importFrom stats sd
#' @order 2
#' @export
type_heatmap = function(
    width = 1,
    height = 1,
    scale = c("none", "x", "y"),
    method = c("zscore", "rescale")) {
  assert_numeric(width)
  assert_numeric(height)
  if (length(scale) > 1L) scale = scale[1L]
  assert_choice(scale, c("none", "x", "y"))
  if (length(method) > 1L) method = method[1L]
  assert_choice(method, c("zscore", "rescale"))
  out = list(
    draw = draw_rect(),
    data = data_heatmap(
      width = width, height = height, scale = scale, method = method
    ),
    # Deliberately reports "tile": the two types are interchangeable as far as
    # the rest of the pipeline is concerned, and nothing downstream needs to
    # tell them apart. Keeps the option of diverging later.
    name = "tile"
  )
  class(out) = "tinyplot_type"
  return(out)
}


## type_heatmap() is data_tile() plus one extra convention: the first row sits
## at the *top*, matching how one reads a matrix (cf. `heatmap()`, `image()`).
## Kept separate from data_tile() so that type_tile() keeps drawing values
## exactly as supplied.
data_heatmap = function(
    width = 1, height = 1, scale = "none", method = "zscore") {
  tile_fun = data_tile(
    width = width, height = height, scale = scale, method = method
  )
  fun = function(settings, ...) {
    tile_fun(settings, ...)
    # Only default the reversal when the user has left `ylim` alone: an explicit
    # `ylim` is a direct instruction about axis direction and must win. We set
    # the already-parsed `rev_y` flag rather than `ylim = "reverse"`, because
    # sanitize_lim_rev() resolves that keyword much earlier in the pipeline, so
    # a character `ylim` set here would reach lim_args() unparsed. The flag is
    # also idempotent (so it cannot double-reverse if something upstream has
    # asked for the same thing) and flip_datapoints() knows to swap it under
    # `flip = TRUE`.
    if (isTRUE(settings$null_ylim)) settings$rev_y = TRUE
  }
  return(fun)
}


## Rescale `by` within each level of `g`, either to the unit interval
## (method = "rescale") or as a z-score (method = "zscore"). Both divide by a
## measure of spread, so a group with no spread (all values identical, or a
## single observation) would produce NaN. That is much worse than it sounds:
## `range()` of a vector containing one NaN is NaN, so the draw loop's colour
## indices all become NA and tiles blank out across the *whole* plot, not just
## the offending group. Map such groups to the midpoint of the target range
## instead, and report them back so the caller can warn -- a silently flattened
## group otherwise reads as a genuine mid-scale value.
##
## Lives here rather than next to its call site in data_tile(), since scaling is
## a heatmap concern: data_tile()'s `scale` branch is only ever reached via
## type_heatmap(), type_tile() having no `scale` argument to trigger it.
scale_by_group = function(by, g, method = "zscore") {
  gi = if (is.factor(g)) g else factor(g)
  mid = if (identical(method, "zscore")) 0 else 0.5
  flat = character(0)
  out = unsplit(
    lapply(split(seq_along(by), gi), function(ix) {
      v = by[ix]
      if (identical(method, "zscore")) {
        s = sd(v, na.rm = TRUE)
        if (!is.finite(s) || s == 0) {
          flat[[length(flat) + 1L]] <<- as.character(gi[ix][1L])
          return(rep.int(mid, length(v)))
        }
        return((v - mean(v, na.rm = TRUE)) / s)
      }
      # rescale_num()'s default `from` is range(x), which propagates an NA to
      # every element, so compute the range with na.rm explicitly.
      rng = range(v, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) {
        flat[[length(flat) + 1L]] <<- as.character(gi[ix][1L])
        return(rep.int(mid, length(v)))
      }
      rescale_num(v, from = rng, to = c(0, 1))
    }),
    gi
  )
  attr(out, "flat") = flat
  out
}
