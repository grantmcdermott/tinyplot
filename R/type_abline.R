#' Add straight lines to a plot
#' @description
#' These functions add straight line(s) through the current plot.
#' @details
#' While `type_abline`, `type_hline`, and `type_vline` can be called in a base
#' plot layer, we expect that they will typically be called as subsequent
#' layers via [`tinyplot_add`].
#' @section Recycling logic:
#' The recycling behaviour of the line parameters (i.e., `a`, `b`, `h`, or `v`)
#' is adaptive, depending on whether `by` or `facet` grouping is detected. While
#' this leads to different recycling scenarios, the underlying code logic
#' follows sensible heuristics designed to match user expectations.
#'
#' Parameter lengths must equal one of four options:
#'
#' 1. Single value (i.e., length = 1), i.e. simplest case where the same line is
#' applied uniformly across all groups and facets. Uses the default user colour
#' (e.g. `"black"`, or `tpar("palette.qualitative")[1]` if a theme is set).
#' 2. Number of `by` groups, i.e. one parameter per group. For example,
#' `tinyplot(mpg ~ wt | factor(cyl), data = mtcars, type = type_hline(h = 21:23))`
#' will give three horizontal lines, with colours matching the user's qualitative
#' palette.
#' 3. Number of `facet` groups, i.e. one parameter per facet panel. For example:
#' `tinyplot(mpg ~ wt, facet = ~am, data = mtcars, type = type_hline(h = c(20,30)))`
#' would give separate horizontal lines per facet, but both using the same
#' default color.
#' 4. Product of `by` and `facet` groups, i.e. one parameter for each unique
#' by-facet combination. Orders over facets first and then, within that, by
#' group. For example:
#' `tinyplot(mpg ~ wt | factor(cyl), facet = ~am, data = mtcars, type = type_hline(h = 21:26))`
#' will give six separate lines, with the first three (`21:23`) coloured by
#' group in the first facet, and second three (`24:26`) coloured by by group
#' in the second facet.
#'
#' Alongside these general rules, we also try to accomodate special cases when
#' other aesthetic parameters like `lwd` or `lty` are invoked by the user. See
#' Examples.
#'
#' @param a,b the intercept (default: `a` = 0) and slope (default: `b` = 1)
#'   terms. Numerics of length 1, or equal to the number of groups or number of
#'   facets (or the product thereof).
#' @examples
#' #
#' ## abline
#'
#' tinyplot(x = -10:10, y = rnorm(21) + -10:10, grid = TRUE)
#' tinyplot_add(type = "abline")
#' # same as...
#' # tinyplot_add(type = type_abline(a = 0, b = 1))
#'
#' # customize by passing bespoke intercept and slope values
#' tinyplot_add(type = type_abline(a = -1, b = -0.5))
#'
#' # note that calling as abline & co. as a base plot layer will still lead to
#' # axes limits that respect the range of the data
#' tinyplot(x = -10:10, y = -10:10, grid = TRUE, type = "abline")
#'
#' #
#' ## hline and vline
#'
#' # Base plot layer
#' tinyplot(mpg ~ hp | cyl, facet = "by", data = mtcars, ylim = c(0, 40))
#'
#' # Add horizontal lines at the (default) 0 y-intercept
#' tinyplot_add(type = "hline", col = "grey")
#'
#' # Note that group+facet aesthetics will be inherited. We can use this to
#' # add customized lines (here: the mean `mpg` for each `cyl` group)
#' tinyplot_add(type = type_hline(with(mtcars, tapply(mpg, cyl, mean))), lty = 2)
#'
#' # Similar idea for vline
#' tinyplot_add(type = type_vline(with(mtcars, tapply(hp, cyl, mean))), lty = 2)
#'
#' #
#' ## Recycling logic
#'
#' # length(h) == no. of groups
#' tinyplot(mpg ~ wt | factor(cyl), data = mtcars, type = type_hline(h = 21:23))
#'
#' # length(h) == no. of facets
#' tinyplot(mpg ~ wt, facet = ~am, data = mtcars, type = type_hline(h = c(20, 30)))
#'
#' # length(h) == no. of groups x no. of facets
#' tinyplot(mpg ~ wt | factor(cyl),
#'   facet = ~am, data = mtcars,
#'   type = type_hline(h = 21:26))
#'
#' # special adjustment case (here: lwd by group)
#' tinyplot(mpg ~ wt | factor(cyl),
#'   facet = ~am, data = mtcars,
#'   type = type_hline(c(20, 30)), lwd = c(21, 14, 7))
#'
#' @export
type_abline = function(a = 0, b = 1) {
  data_abline = function(settings, ...) {
    env2env(settings, environment(), c("datapoints", "lwd", "lty", "col"))
    if (nrow(datapoints) == 0) {
      msg = "`type_abline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    # keep track of unique lty and lwd (needed for group catch / escape hatch
    # later in draw_hline)
    ul_lwd = length(unique(lwd))
    ul_lty = length(unique(lty))
    ul_col = length(unique(col))
    type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col)
    env2env(environment(), settings, "type_info")
  }
  draw_abline = function() {
    fun = function(ifacet, iby, data_facet, icol, ilty, ilwd,
                   ngrps, nfacets, by_continuous, facet_by,
                   type_info,
                   ...) {
      # flag for aesthetics by groups
      grp_aes = type_info[["ul_col"]] == 1 || type_info[["ul_lty"]] == ngrps || type_info[["ul_lwd"]] == ngrps

      if (length(a) != 1) {
        if (!length(a) %in% c(ngrps, nfacets, ngrps * nfacets)) {
          msg = "Length of 'a' must be 1, or equal to the number of facets or number of groups (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (!facet_by && length(a) == nfacets) {
          a = a[ifacet]
          if (!grp_aes && type_info[["ul_col"]] != ngrps) {
            icol = 1
          } else if (by_continuous) {
            icol = 1
          }
        } else if (!by_continuous && length(a) == ngrps * nfacets) {
          a = a[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          a = a[iby]
        }
      } else if (!grp_aes) {
        icol = 1
      }

      if (length(b) != 1) {
        if (!length(b) %in% c(ngrps, nfacets, ngrps * nfacets)) {
          msg = "Length of 'b' must be 1, or equal to the number of facets or number of groups (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (!facet_by && length(b) == nfacets) {
          b = b[ifacet]
          if (!grp_aes && type_info[["ul_col"]] != ngrps) {
            icol = 1
          } else if (by_continuous) {
            icol = 1
          }
        } else if (!by_continuous && length(b) == ngrps * nfacets) {
          b = b[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          b = b[iby]
        }
      } else if (!grp_aes) {
        icol = 1
      }

      if (type_info[["ul_col"]] != 1 && !(type_info[["ul_lty"]] == ngrps || type_info[["ul_lwd"]] == ngrps)) {
        icol = 1
      }

      abline(a = a, b = b, col = icol, lty = ilty, lwd = ilwd)
    }
    return(fun)
  }
  out = list(
    draw = draw_abline(),
    data = data_abline,
    name = "abline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
