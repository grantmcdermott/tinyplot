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
#' \tabular{lll}{
#'   `facet.cex` \tab\tab Expansion factor for facet titles. Defaults to `1`.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `facet.font` \tab\tab An integer corresponding to the desired font face for facet titles. For most font families and graphics devices, one of four possible values: `1` (regular), `2` (bold), `3` (italic), or `4` (bold italic). Defaults to `NULL`, which is equivalent to `1` (i.e., regular).\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `facet.col` \tab\tab Character or integer specifying the facet text colour. If an integer, will correspond to the user's default global colour palette (see \code{\link[grDevices]{palette}}). Defaults to `NULL`, which is equivalent to "black".\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `facet.bg` \tab\tab Character or integer specifying the facet background colour. If an integer, will correspond to the user's default colour palette (see \code{\link[grDevices]{palette}}). Passed \code{\link[graphics]{rect}}. Defaults to `NULL` (none).\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `facet.border` \tab\tab Character or integer specifying the facet border colour. If an integer, will correspond to the users default colour palette (see \code{\link[grDevices]{palette}}). Passed \code{\link[graphics]{rect}}. Defaults to `NA` (none).\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `file.height` \tab\tab Numeric specifying the height (in inches) of any plot that is written to disk using the `tinyplot(..., file = X)` argument. Defaults to 7.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `file.width` \tab\tab Numeric specifying the width (in inches) of any plot that is written to disk using the `tinyplot(..., file = X)` argument. Defaults to 7.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `file.res` \tab\tab Numeric specifying the resolution (in dots per square inch) of any plot that is written to disk in bitmap format (i.e., PNG or JPEG) using the `tinyplot(..., file = X)` argument. Defaults to 300.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `fmar` \tab\tab A numeric vector of form `c(b,l,t,r)` for controlling the (base) margin padding, in terms of lines, between the individual facets in a faceted plot. Defaults to `c(1,1,1,1)`, i.e. a single line of padding around each facet. If more that three facets are detected, the `fmar` parameter is scaled by 0.75 (i.e., three-quarters) to reduce the excess whitespace that would otherwise arise due to the absent axes lines and labels. (An exception is made for 2x2 plots to better match the `cex` expansion logic of the base graphics system under this particular layout.) Similarly, note that an extra 0.5 lines is subtracted from each side of the facet padding for plots that aren't framed, to reduce excess whitespace.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `grid` \tab\tab Logical indicating whether a background panel grid should be added to plots automatically. Defaults to NULL, which is equivalent to `FALSE`.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `lmar` \tab\tab A numeric vector of form `c(inner, outer)` that gives the margin padding, in terms of lines, around the automatic `tinyplot` legend. Defaults to `c(1.0, 0.1)`, where the first number represents the "inner" margin between the legend and the plot region, and the second number represents the "outer" margin between the legend and edge of the graphics device. (Note that an exception for the definition of the "outer" legend margin occurs when the legend placement is `"top!"`, since the legend is placed above the plot region but below the main title. In such cases, the outer margin is relative to the existing gap between the title and the plot region, which is itself determined by `par("mar")[3]`.)\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `ribbon.alpha` \tab\tab Numeric factor in the range `[0,1]` for modifying the opacity alpha of "ribbon" and "area" (and alike) type plots. Default value is `0.2`.\cr
#' }
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
#'    las       = 1,
#'    pch       = 2,
#'    facet.bg  = "grey90",
#'    facet.cex = 2,
#'    grid      = TRUE
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
  
  facet.col = facet.bg = facet.border = used_par_old = NULL
  
  opts = list(...)
  if (length(opts)==1 && is.null(names(opts))) {
    if (inherits(opts[[1]], "list") && !is.null(names(opts[[1]]))) {
      opts = opts[[1]]
    }
  }
  
  tpar_old = as.list(.tpar)
  nam = names(opts)
  
  known_par = names(par(no.readonly = TRUE))
  if (!is.null(nam)) {
    used_par = intersect(nam, known_par)
  } else {
    used_par = intersect(opts, known_par)
  }
  if (length(used_par)) {
    if (!is.null(nam)) used_par = opts[used_par]
    used_par_old = par(used_par)
    tpar_old = modifyList(tpar_old, used_par_old, keep.null = TRUE)
  }
  
  if (length(opts$facet.cex)) {
    facet.cex = as.numeric(opts$facet.cex)
    if(!is.numeric(facet.cex)) stop("facet.cex needs to be numeric")
    if(length(facet.cex)!=1) stop("facet.cex needs to be of length 1")
    .tpar$facet.cex = facet.cex
  }
  
  if (length(opts$facet.font)) {
    facet.font = as.numeric(opts$facet.font)
    if(!is.numeric(facet.font)) stop("facet.font needs to be numeric")
    if(length(facet.font)!=1) stop("facet.font needs to be of length 1")
    .tpar$facet.font = facet.font
  }
  
  if (length(opts$facet.col) || ("facet.col" %in% nam && is.null(opts$facet.col))) {
    facet.col = opts$facet.col
    if(!is.null(facet.col) && !is.numeric(facet.col) && !is.character(facet.col)) stop("facet.col needs to be NULL, or a numeric or character")
    if(!is.null(facet.col) && length(facet.col)!=1) stop("facet.col needs to be of length 1")
    .tpar$facet.col = facet.col
  }
  
  if (length(opts$facet.bg) || ("facet.bg" %in% nam && is.null(opts$facet.bg))) {
    facet.bg = opts$facet.bg
    if(!is.null(facet.bg) && !is.numeric(facet.bg) && !is.character(facet.bg)) stop("facet.bg needs to be NULL, or a numeric or character")
    if(!is.null(facet.bg) && length(facet.bg)!=1) stop("facet.bg needs to be of length 1")
    .tpar$facet.bg = facet.bg
  }
  
  if (length(opts$facet.border)) {
    facet.border = opts$facet.border
    if(!is.na(facet.border) && !is.numeric(facet.border) && !is.character(facet.border)) stop("facet.border needs to be NA, or a numeric or character")
    if(length(facet.border)!=1) stop("facet.border needs to be of length 1")
    .tpar$facet.border = facet.border
  }
  
  if (length(opts$file.width)) {
    file.width = as.numeric(opts$file.width)
    if(!is.numeric(file.width)) stop("file.width needs to be numeric")
    .tpar$file.width = file.width
  }
  
  if (length(opts$file.height)) {
    file.height = as.numeric(opts$file.height)
    if(!is.numeric(file.height)) stop("file.height needs to be numeric")
    .tpar$file.height = file.height
  }
  
  if (length(opts$file.res)) {
    file.res = as.numeric(opts$file.res)
    if(!is.numeric(file.res)) stop("file.res needs to be numeric")
    .tpar$file.res = file.res
  }
  
  if (length(opts$fmar)) {
    fmar = as.numeric(opts$fmar)
    if(!is.numeric(fmar)) stop("fmar needs to be numeric")
    if(length(fmar)!=4) stop("fmar needs to be of length 4, i.e. c(b,l,t,r)")
    .tpar$fmar = fmar
  }
  
  if (length(opts$grid)) {
    grid = as.logical(opts$grid)
    if(!is.null(grid) && !is.logical(grid)) stop("grid needs to be NULL or logical")
    .tpar$grid = grid
  }
  
  if (length(opts$lmar)) {
    lmar = as.numeric(opts$lmar)
    if(!is.numeric(lmar)) stop("lmar needs to be numeric")
    if(length(lmar)!=2) stop("lmar needs to be of length 2, i.e. c(inner, outer)")
    .tpar$lmar = lmar
  }
  
  if (length(opts$ribbon.alpha)) {
    ribbon.alpha = as.numeric(opts$ribbon.alpha)
    if (!is.numeric(ribbon.alpha) || ribbon.alpha<0 || ribbon.alpha>1) stop("ribbon.alpha needs to be a numeric in the range [0,1]")
    .tpar$ribbon.alpha = ribbon.alpha
  }
  
  ## Like par(), we want the return object to be dependent on inputs...
  
  # User didn't assign any new values, but may have requested explicit (print
  # of) some existing value(s)
  if (is.null(nam)) {
    if (!is.null(opts) && length(opts)!=0) {
      # specific values requested
      ret = (`names<-`(lapply(opts, function(x) .tpar[[x]]), opts))
      if (length(used_par)) {
        ret_par = par(used_par)
        ret = modifyList(ret, ret_par, keep.null = TRUE)
      }
      if (length(ret)==1) ret = ret[[1]]
      return(ret)
    } else {
      # no specific request; return all existing values invisibly
      return(invisible(tpar_old))
    }
  # assign new values, but still return old values for saving existing settings
  # a la `oldpar = par(param = new_value)`
  } else {
    `names<-`(lapply(nam, function(x) .tpar[[x]]), nam)
    return(invisible(tpar_old))
  }
  
}

