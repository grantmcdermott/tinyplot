#' @title Set or query graphical parameters
#'
#' @description Extends \code{\link[graphics]{par}}, serving as a (near) drop-in
#'   replacement for setting or querying graphical parameters. The key
#'   differences is that, beyond supporting the standard group of R graphical
#'   parameters in \code{\link[graphics]{par}}, `tpar` also supports additional
#'   graphical parameters that are provided by `tinyplot`. Similar to
#'   \code{\link[graphics]{par}}, parameters are set by passing appropriate
#'   `key = value` argument pairs, and multiple parameters can be set or queried
#'   at the same time.
#'
#' @md
#' @param ... arguments of the form `key = value`. This includes all of the
#'   parameters typically supported by \code{\link[graphics]{par}}, as well as
#'   the `tinyplot`-specific ones described in the 'Graphical Parameters'
#'   section below.
#'
#' @details The `tinyplot`-specific parameters are saved in an internal
#'   environment called `.tpar` for performance and safety reasons. However,
#'   they can also be set at package load time via \code{\link[base]{options}},
#'   which may prove convenient for users that want to enable different default
#'   behaviour at startup (e.g., through an `.Rprofile` file). These options all
#'   take a `tinyplot_*` prefix, e.g.
#'   `options(tinyplot_grid = TRUE, tinyplot_facet.bg = "grey90")`.
#'
#' For their part, any "base" graphical parameters are caught dynamically and
#'   passed on to \code{\link[graphics]{par}} as appropriate. Technically, only
#'   parameters that satisfy `par(..., no.readonly = TRUE)` are evaluated.
#'
#' However, note the important distinction: `tpar` only evaluates parameters
#'   from \code{\link[graphics]{par}} if they are passed _explicitly_ by the
#'   user. This means that `tpar` should not be used to capture the (invisible)
#'   state of a user's entire set of graphics parameters, i.e. `tpar()` !=
#'   `par()`. If you want to capture the _all_ existing graphics settings, then
#'   you should rather use `par()` instead.
#'
#' @returns When parameters are set, their previous values are returned in an
#'   invisible named list. Such a list can be passed as an argument to `tpar` to
#'   restore the parameter values.
#'
#'   When just one parameter is queried, the value of that parameter is returned
#'   as (atomic) vector. When two or more parameters are queried, their values
#'   are returned in a list, with the list names giving the parameters.
#'
#'   Note the inconsistency: setting one parameter returns a list, but querying
#'   one parameter returns a vector.
#'
#' @section Additional Graphical Parameters:
#'
#' * `adj.xlab`: Numeric value between 0 and 1 controlling the alignment of the x-axis label.
#' * `adj.ylab`: Numeric value between 0 and 1 controlling the alignment of the y-axis label.
#' * `facet.bg`: Character or integer specifying the facet background colour. If an integer, will correspond to the user's default colour palette (see `palette`). Passed to `rect`. Defaults to `NULL` (none).
#' * `facet.border`: Character or integer specifying the facet border colour. If an integer, will correspond to the user's default colour palette (see `palette`). Passed to `rect`. Defaults to `NA` (none).
#' * `facet.cex`: Expansion factor for facet titles. Defaults to `1`.
#' * `facet.col`: Character or integer specifying the facet text colour. If an integer, will correspond to the user's default global colour palette (see `palette`). Defaults to `NULL`, which is equivalent to "black".
#' * `facet.font`: An integer corresponding to the desired font face for facet titles. For most font families and graphics devices, one of four possible values: `1` (regular), `2` (bold), `3` (italic), or `4` (bold italic). Defaults to `NULL`, which is equivalent to `1` (i.e., regular).
#' * `file.height`: Numeric specifying the height (in inches) of any plot that is written to disk using the `tinyplot(..., file = X)` argument. Defaults to `7`.
#' * `file.res`: Numeric specifying the resolution (in dots per square inch) of any plot that is written to disk in bitmap format (i.e., PNG or JPEG) using the `tinyplot(..., file = X)` argument. Defaults to `300`.
#' * `file.width`: Numeric specifying the width (in inches) of any plot that is written to disk using the `tinyplot(..., file = X)` argument. Defaults to `7`.
#' * `fmar`: A numeric vector of form `c(b,l,t,r)` for controlling the (base) margin padding, in terms of lines, between the individual facets in a faceted plot. Defaults to `c(1,1,1,1)`. If more than three facets are detected, the `fmar` parameter is scaled by 0.75 to reduce excess whitespace. For 2x2 plots, the padding better matches the `cex` expansion logic of base graphics.
#' * `grid.col`: Character or (integer) numeric that specifies the color of the panel grid lines. Defaults to `"lightgray"`.
#' * `grid.lty`: Character or (integer) numeric that specifies the line type of the panel grid lines. Defaults to `"dotted"`.
#' * `grid.lwd`: Non-negative numeric giving the line width of the panel grid lines. Defaults to `1`.
#' * `grid`: Logical indicating whether a background panel grid should be added to plots automatically. Defaults to `NULL`, which is equivalent to `FALSE`.
#' * `lmar`: A numeric vector of form `c(inner, outer)` that gives the margin padding, in terms of lines, around the automatic `tinyplot` legend. Defaults to `c(1.0, 0.1)`. The inner margin is the gap between the legend and the plot region, and the outer margin is the gap between the legend and the edge of the graphics device.
#' * `ribbon.alpha`: Numeric factor in the range `[0,1]` for modifying the opacity alpha of "ribbon" and "area" type plots. Default value is `0.2`.
#'
#' @importFrom graphics par
#' @importFrom utils modifyList
#'
#' @examples
#' # Return a list of existing base and tinyplot graphic params
#' tpar("las", "pch", "facet.bg", "facet.cex", "grid")
#'
#' # Simple facet plot with these default values
#' tinyplot(mpg ~ wt, data = mtcars, facet = ~am)
#'
#' # Set params to something new. Similar to graphics::par(), note that we save
#' # the existing values at the same time by assigning to an object.
#' op = tpar(
#'   las       = 1,
#'   pch       = 2,
#'   facet.bg  = "grey90",
#'   facet.cex = 2,
#'   grid      = TRUE
#' )
#'
#' # Re-plot with these new params
#' tinyplot(mpg ~ wt, data = mtcars, facet = ~am)
#'
#' # Reset back to original values
#' tpar(op)
#'
#' # Important: tpar() only evalutes parameters that have been passed explicitly
#' #   by the user. So it it should not be used to query and set (restore)
#' #   parameters that weren't explicitly requested, i.e. tpar() != par().
#'
#' # Note: The tinyplot-specific parameters can also be be set via `options`
#' #   with a `tinyplot_*` prefix, which can be convenient for enabling
#' #   different default behaviour at startup time (e.g., via an .Rprofile
#' #   file). Example:
#' # options(tinyplot_grid = TRUE, tinyplot_facet.bg = "grey90")
#'
#' @export
tpar = function(...) {
  opts = list(...)
  if (length(opts) == 1 && is.null(names(opts))) {
    if (inherits(opts[[1]], "list") && !is.null(names(opts[[1]]))) {
      opts = opts[[1]]
    }
  }


  ###### Assign parameters

  # assign tinyplot-specific arguments with known names to .tpar
  assign_tpar(opts)

  # return informative error messages if the input is invalid
  assert_tpar(.tpar)

  # if tpar(...) includes arguments that are not known to be tinyplot-specific,
  # we set a hook to set them using par() when the graphic device is started
  nam = names(opts)
  if (!is.null(nam)) {
    base_params = setdiff(nam, known_tpar)
    base_params = opts[base_params]
    tpar_hook = function() {
      do.call(par, base_params)
    }
    setHook("before.plot.new", tpar_hook, action = "append")
    do.call(par, base_params)
  }


  ###### Retrieve parameters

  # User didn't assign any new values, but may have requested explicit (print
  # of) some existing value(s)
  tpar_old = as.list(.tpar)
  if (is.null(nam)) {
    known_par = names(par(no.readonly = TRUE))
    if (!is.null(nam)) {
      used_par = intersect(nam, known_par)
    } else {
      used_par = intersect(opts, known_par)
    }
    if (length(used_par)) {
      if (!is.null(nam)) used_par = opts[used_par]
      used_par_old = par(used_par)
      tpar_old = modifyList(as.list(.tpar), used_par_old, keep.null = TRUE)
    }
    if (!is.null(opts) && length(opts) != 0) {
      # specific values requested
      ret = (`names<-`(lapply(opts, function(x) .tpar[[x]]), opts))
      if (length(used_par)) {
        ret_par = par(used_par)
        ret = modifyList(ret, ret_par, keep.null = TRUE)
      }
      if (length(ret) == 1) ret = ret[[1]]
      return(ret)
    } else {
      # no specific request; return all existing values invisibly
      return(invisible(tpar_old))
    }
    # assign new values, but still return old values for saving existing settings
    # a la `oldpar = par(param = new_value)`
  } else {
    `names<-`(lapply(nam, function(x) .tpar
      [[x]]), nam)
    return(invisible(tpar_old))
  }
}


# Two levels of priority:
#
get_tpar = function(opts, default = NULL) {
  # parameter priority
  # .tpar[["name"]] -> par("name")
  for (o in opts) {
    tp = .tpar[[o]]
    if (!is.null(tp)) {
      return(tp)
    }

    p = suppressWarnings(par(o))
    if (!is.null(p)) {
      return(p)
    }
  }
  return(default)
}


known_tpar = c(
    "adj.main",
    "adj.sub",
    "adj.xlab",
    "adj.ylab",
    "cex.xlab",
    "cex.ylab",
    "col.axis",
    "col.xaxs",
    "col.yaxs",
    "facet.bg",
    "facet.border",
    "facet.cex",
    "facet.col",
    "facet.font",
    "file.height",
    "file.res",
    "file.width",
    "fmar",
    "grid",
    "grid.bg",
    "grid.col",
    "grid.lty",
    "grid.lwd",
    "lmar",
    "lty.xaxs",
    "lty.yaxs",
    "lwd.xaxs",
    "lwd.yaxs",
    "lwd.axis",
    "ribbon.alpha",
    "side.sub",
    "tinytheme",
    "xaxt",
    "yaxt"
)


assign_tpar = function(opts) {
  for (n in intersect(names(opts), known_tpar)) {
    .tpar[[n]] = opts[[n]]
  }
}


assert_tpar = function(.tpar) {
  assert_numeric(.tpar[["adj.main"]], len = 1, lower = 0, upper = 1, null.ok = TRUE, name = "adj.main")
  assert_numeric(.tpar[["adj.sub"]], len = 1, lower = 0, upper = 1, null.ok = TRUE, name = "adj.sub")
  assert_numeric(.tpar[["adj.xlab"]], len = 1, lower = 0, upper = 1, null.ok = TRUE, name = "adj.xlab")
  assert_numeric(.tpar[["adj.ylab"]], len = 1, lower = 0, upper = 1, null.ok = TRUE, name = "adj.ylab")
  assert_numeric(.tpar[["lmar"]], len = 2, null.ok = TRUE, name = "lmar")
  assert_numeric(.tpar[["ribbon.alpha"]], len = 1, lower = 0, upper = 1, null.ok = TRUE, name = "ribbon.alpha")
  assert_numeric(.tpar[["grid.lwd"]], len = 1, lower = 0, null.ok = TRUE, name = "grid.lwd")
  assert_flag(.tpar[["grid"]], null.ok = TRUE, name = "grid")
  assert_numeric(.tpar[["file.res"]], len = 1, lower = 0, null.ok = TRUE, name = "file.res")
  assert_numeric(.tpar[["file.height"]], len = 1, lower = 0, null.ok = TRUE, name = "file.height")
  assert_numeric(.tpar[["file.width"]], len = 1, lower = 0, null.ok = TRUE, name = "file.width")
  assert_numeric(.tpar[["facet.font"]], len = 1, null.ok = TRUE, name = "facet.font")
  assert_numeric(.tpar[["facet.cex"]], len = 1, null.ok = TRUE, name = "facet.cex")
  assert_numeric(.tpar[["side.sub"]], len = 1, null.ok = TRUE, name = "side.sub")
  assert_string(.tpar[["grid.bg"]], null.ok = TRUE, name = "grid.bg")
  assert_numeric(.tpar[["fmar"]], len = 4, null.ok = TRUE, name = "fmar")

  facet.col = .tpar[["facet.col"]]
  if (!is.null(facet.col)) {
    if (!is.null(facet.col) && !is.numeric(facet.col) && !is.character(facet.col)) {
      stop("facet.col needs to be NULL, or a numeric or character", call. = FALSE)
    }
    assert_true(length(facet.col) == 1, name = "length(facet.col)==1")
  }

  facet.bg = .tpar$facet.bg
  if (!is.null(facet.bg)) {
    if (!is.numeric(facet.bg) && !is.character(facet.bg)) {
      stop("facet.bg needs to be NULL, or a numeric or character", call. = FALSE)
    }
    assert_true(length(facet.bg) == 1, name = "length(facet.bg)==1")
  }

  facet.border = .tpar$facet.border
  if (!is.null(facet.border)) {
    if (!is.numeric(facet.border) && !is.character(facet.border) && !is.na(facet.border)) {
      stop("facet.border needs to be NULL, or a numeric, character, or NA", call. = FALSE)
    }
    assert_true(length(facet.border) == 1, name = "length(facet.border)==1")
  }
}


init_tpar = function() {
  # Figure output options if written to file
  .tpar$file.width = if (is.null(getOption("tinyplot_file.width"))) 7 else as.numeric(getOption("tinyplot_file.width"))
  .tpar$file.height = if (is.null(getOption("tinyplot_file.height"))) 7 else as.numeric(getOption("tinyplot_file.height"))
  .tpar$file.res = if (is.null(getOption("tinyplot_file.res"))) 300 else as.numeric(getOption("tinyplot_file.res"))

  # Facet margin, i.e. gap between the individual facet windows
  .tpar$fmar = if (is.null(getOption("tinyplot_fmar"))) c(1, 1, 1, 1) else as.numeric(getOption("tinyplot_fmar"))

  # Other facet options
  .tpar$facet.cex = if (is.null(getOption("tinyplot_facet.cex"))) 1 else as.numeric(getOption("tinyplot_facet.cex"))
  .tpar$facet.font = if (is.null(getOption("tinyplot_facet.font"))) NULL else as.numeric(getOption("tinyplot_facet.font"))
  .tpar$facet.col = if (is.null(getOption("tinyplot_facet.col"))) NULL else getOption("tinyplot_facet.col")
  .tpar$facet.bg = if (is.null(getOption("tinyplot_facet.bg"))) NULL else getOption("tinyplot_facet.bg")
  .tpar$facet.border = if (is.null(getOption("tinyplot_facet.border"))) NA else getOption("tinyplot_facet.border")

  # Plot grid
  .tpar$grid = if (is.null(getOption("tinyplot_grid"))) FALSE else as.logical(getOption("tinyplot_grid"))
  .tpar$grid.col = if (is.null(getOption("tinyplot_grid.col"))) "lightgray" else getOption("tinyplot_grid.col")
  .tpar$grid.lty = if (is.null(getOption("tinyplot_grid.lty"))) "dotted" else getOption("tinyplot_grid.lty")
  .tpar$grid.lwd = if (is.null(getOption("tinyplot_grid.lwd"))) 1 else as.numeric(getOption("tinyplot_grid.lwd"))

  # Legend margin, i.e. gap between the legend and the plot elements
  .tpar$lmar = if (is.null(getOption("tinyplot_lmar"))) c(1.0, 0.1) else as.numeric(getOption("tinyplot_lmar"))

  # Alpha fill (transparency) default for ribbon and area plots
  .tpar$ribbon.alpha = if (is.null(getOption("tinyplot_ribbon.alpha"))) 0.2 else as.numeric(getOption("tinyplot_ribbon.alpha"))
}
