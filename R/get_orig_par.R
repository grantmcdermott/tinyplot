#' @title Retrieve the original graphical parameters
#'   
#' @description Convenience function for retrieving the original graphical
#'   parameters (i.e., the full list of `tag = value` pairs held in
#'   \code{\link[graphics]{par}} that were set prior to the most recent
#'   [tinyplot] call.
#'   
#' @details A potential side-effect of [tinyplot] is that it can change a user's
#'   \code{\link[graphics]{par}} settings. For example, it may adjust the inner
#'   and outer plot margins to make space for an automatic legend; see
#'   [draw_legend]. While it is possible to immediately restore the original
#'   \code{\link[graphics]{par}}: settings upon exit via the
#'   `tinyplot(..., restore.par = TRUE)` argument, this is not the default
#'   behaviour. The reason is that we want to preserve the adjusted parameter
#'   settings in case users want to add further graphical annotations to their
#'   plot (e.g., \code{\link[graphics]{abline}}, \code{\link[graphics]{text}},
#'   etc.) Nevertheless, it may still prove desirable to recall and reset these
#'   original graphical parameters after the fact (e.g., once all these extra
#'   annotations have been added). That is the purpose of this [get_orig_par]
#'   function.
#'   
#'   Of course, users may prefer to manually capture and reset graphical
#'   parameters, as per the standard method described in the
#'   \code{\link[graphics]{par}} documentation. For example:
#'   
#'   ```
#'   op = par(no.readonly = TRUE) # save current par settings 
#'   # <do lots of (tiny)plotting>
#'   par(op) # reset original pars
#'   ```
#'   
#'   This standard manual approach may be safer than [get_orig_par] because it
#'   offers more precise control. Specifically, the value of [get_orig_par] 
#'   itself will be reset after ever new [tinyplot] call; i.e. it may inherit an
#'   already-changed set of parameters. Users should bear these trade-offs in
#'   mind when deciding which approach to use. As a general rule,
#'   [get_orig_par] offers the convenience of resetting the original
#'   \code{\link[graphics]{par}} settings even if a user forgot to save them
#'   beforehand. But one should avoid invoking it after a series of consecutive
#'   [tinyplot] calls.
#'   
#'   Finally, note that users can always call \code{\link[grDevices]{dev.off}}
#'   to reset all \code{\link[graphics]{par}} settings to their defaults.
#' 
#' @returns A list of \code{\link[graphics]{par}} settings.
#' 
#' @examples
#' # Contrived example where we draw a grouped scatterplot with a legend and
#' # manually add corresponding best fit lines for each group
#' tinyplot(Sepal.Length ~ Petal.Length | Species, iris)
#' for (s in levels(iris$Species)) {
#'   abline(
#'     lm(Sepal.Length ~ Petal.Length, iris, subset = Species==s),
#'     col = which(levels(iris$Species)==s)
#'   )
#' }
#' 
#' # Reset the original parameters (here: affecting the plot margins) and draw
#' # a regular plot
#' par(get_orig_par())
#' plot(1:10)
#'
#' @export
get_orig_par <- function() {
  return(get(".orig_par", envir = get(".tinyplot_env", envir = parent.env(environment()))))
}

# (non-exported) companion function for setting the original pars
set_orig_par <- function(value) {
  assign(".orig_par", value, envir = get(".tinyplot_env", envir = parent.env(environment())))
}

# simillarly, separate (non-exported) setter and getter functions for
# .last_facet_par
set_last_facet_par <- function(value) {
  assign(".last_facet_par", value, envir = get(".tinyplot_env", envir = parent.env(environment())))
}
get_last_facet_par <- function() {
  return(get(".last_facet_par", envir = get(".tinyplot_env", envir = parent.env(environment()))))
}
