#' @title Set or query tinyplot parameters  
#'   
#' @description `tpar` can be used to set or query the additional set of
#'   graphical parameters provided by `tinyplot` (i.e., beyond the base set
#'   provided by  \code{\link[graphics]{par}}). Similar to its base counterpart, parameters can be set
#'   by passing the appropriate tag-value argument pairs to `tpar`. Multiple
#'   parameters can be set or queried at the same time, as a list.
#'   
#' @md
#' @param ... arguments of the form `tag = value`. Supported `tinyplot` parameters
#'   are described in the 'Graphical Parameters' section below.
#'
#' @section Graphical Parameters:
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
#'   `fmar` \tab\tab A numeric vector of form `c(b,l,t,r)` for controlling the (base) margin padding, in terms of lines, between the individual facets in a faceted plot. Defaults to `c(1,1,1,1)`, i.e. a single line of padding around each facet. If more that three facets are detected, the `fmar` parameter is scaled by 0.75 (i.e., three-quarters) to reduce the excess whitespace that would otherwise arise due to the absent axes lines and labels. (An exception is made for 2x2 plots to better match the `cex` expansion logic of the base graphics system under this particular layout.) Similarly, note that an extra 0.5 lines is subtracted from each side of the facet padding for plots that aren't framed, to reduce excess whitespace.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `last_facet_par` \tab\tab Full list of graphical parameters used to constructed the most recent faceted `tinyplot` plot during the current session. Unlike other `tpar` parameters, this parameter is intended for internal use (specifically, to enable adding further elements on top of an existing faceted plot) and should _not_ be set by the user.\cr
#'   \tab\tab\cr
#'   \tab\tab\cr
#'   `lmar` \tab\tab A numeric vector of form `c(inner, outer)` that gives the margin padding, in terms of lines, around the automatic `tinyplot` legend. Defaults to `c(1.0, 0.1)`, where the first number represents the "inner" margin between the legend and the plot region, and the second number represents the "outer" margin between the legend and edge of the graphics device. (Note that an exception for the definition of the "outer" legend margin occurs when the legend placement is `"top!"`, since the legend is placed above the plot region but below the main title. In such cases, the outer margin is relative to the existing gap between the title and the plot region, which is itself determined by `par("mar")[3]`.)\cr
#' }
#' 
#' @export
tpar = function(...) {
  
  facet.col = facet.bg = facet.border = NULL
  
  opts = list(...)
  tpar_old = as.list(.tpar)
  nam = names(opts)
  
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
  
  if (length(opts$facet.col)) {
    if(!is.numeric(facet.col) || !is.character(facet.col)) stop("facet.col needs to be a numeric or character")
    if(length(facet.col)!=1) stop("facet.col needs to be of length 1")
    .tpar$facet.col = facet.col
  }
  
  if (length(opts$facet.bg)) {
    if(!is.numeric(facet.bg) || !is.character(facet.bg)) stop("facet.bg needs to be a numeric or character")
    if(length(facet.bg)!=1) stop("facet.bg needs to be of length 1")
    .tpar$facet.bg = facet.bg
  }
  
  if (length(opts$facet.border)) {
    if(!is.numeric(facet.border) || !is.character(facet.border)) stop("facet.border needs to be a numeric or character")
    if(length(facet.border)!=1) stop("facet.border needs to be of length 1")
    .tpar$facet.border = facet.border
  }
  
  if (length(opts$fmar)) {
    fmar = as.numeric(opts$fmar)
    if(!is.numeric(fmar)) stop("fmar needs to be numeric")
    if(length(fmar)!=4) stop("fmar needs to be of length 4, i.e. c(b,l,t,r)")
    .tpar$fmar = fmar
  }
  
  # if (length(opts$grid)) {
  #   grid = as.logical(opts$grid)
  #   if(!is.logical(grid)) stop("grid needs to be logical")
  #   .tpar$grid = grid
  # }
  
  if (length(opts$last_facet_par)) {
    last_facet_par = opts$last_facet_par
    if(!(is.null(last_facet_par) || is.list(last_facet_par))) stop("last_facet_par needs to be NULL or a list")
    .tpar$last_facet_par = last_facet_par
  }
  
  if (length(opts$lmar)) {
    lmar = as.numeric(opts$lmar)
    if(!is.numeric(lmar)) stop("lmar needs to be numeric")
    if(length(lmar)!=2) stop("lmar needs to be of length 2, i.e. c(inner, outer)")
    .tpar$lmar = lmar
  }
  
  ## Like par(), we want the return object to be dependent on inputs...
  
  # User didn't assign any new values, but may have requested explicit (print
  # of) some existing value(s)
  if (is.null(nam)) {
    if (!is.null(opts) && length(opts)!=0) {
      # specific values requested
      ret = (`names<-`(lapply(opts, function(x) .tpar[[x]]), opts))
      if (length(ret)==1) ret = ret[[1]]
      return(ret)
    } else {
      # no specific request; return all existing values invisibly
      return(invisible(tpar_old))
    }
  # assign new values, but still return old values for saving existing settings
  # a la opar = par(param = new_value)
  } else {
    `names<-`(lapply(nam, function(x) .tpar[[x]]), nam)
    return(invisible(tpar_old))
  }
  
}



