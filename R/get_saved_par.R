#' @title Retrieve the saved graphical parameters
#'   
#' @description Convenience function for retrieving the graphical parameters
#'   (i.e., the full list of `tag = value` pairs held in
#'   \code{\link[graphics]{par}}) from either immediately before or
#'   immediately after the most recent [tinyplot] call.
#'
#' @param when character. From when should the saved parameters be retrieved?
#'   Either "before" (the default) or "after" the preceding `tinyplot` call.
#'   
#' @details A potential side-effect of [tinyplot] is that it can change a user's
#'   \code{\link[graphics]{par}} settings. For example, it may adjust the inner
#'   and outer plot margins to make space for an automatic legend; see
#'   [draw_legend]. While it is possible to immediately restore the original
#'   \code{\link[graphics]{par}} settings upon exit via the
#'   `tinyplot(..., restore.par = TRUE)` argument, this is not the default
#'   behaviour. The reason being that we need to preserve the adjusted parameter
#'   settings in case users want to add further graphical annotations to their
#'   plot (e.g., \code{\link[graphics]{abline}}, \code{\link[graphics]{text}},
#'   etc.) Nevertheless, it may still prove desirable to recall and reset these
#'   original graphical parameters after the fact (e.g., once all these extra
#'   annotations have been added). That is the purpose of this [get_saved_par]
#'   function.
#'   
#'   Of course, users may prefer to manually capture and reset graphical
#'   parameters, as per the standard method described in the
#'   \code{\link[graphics]{par}} documentation. For example:
#'   
#'   ```
#'   op = par(no.readonly = TRUE)  # save current par settings 
#'   # <do lots of (tiny)plotting>
#'   par(op)                       # reset original pars
#'   ```
#'   
#'   This standard manual approach may be safer than [get_saved_par] because it
#'   offers more precise control. Specifically, the value of [get_saved_par] 
#'   itself will be reset after ever new [tinyplot] call; i.e. it may inherit an
#'   already-changed set of parameters. Users should bear these trade-offs in
#'   mind when deciding which approach to use. As a general rule,
#'   [get_saved_par] offers the convenience of resetting the original
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
#' #
#' # Contrived example where we draw a grouped scatterplot with a legend and
#' # manually add corresponding best fit lines for each group...
#' #
#' 
#' # First draw the grouped scatterplot
#' tinyplot(Sepal.Length ~ Petal.Length | Species, iris)
#' 
#' # Preserving adjusted par settings is good for adding elements to our plot
#' for (s in levels(iris$Species)) {
#'   abline(
#'     lm(Sepal.Length ~ Petal.Length, iris, subset = Species==s),
#'     col = which(levels(iris$Species)==s)
#'   )
#' }
#' 
#' # Get saved par from before the preceding tinyplot call (but don't use yet)
#' sp = get_saved_par("before")
#' 
#' # Note the changed margins will affect regular plots too, which is probably
#' # not desirable
#' plot(1:10)
#' 
#' # Reset the original parameters (could use `par(sp)` here)
#' tpar(sp)
#' # Redraw our simple plot with our corrected right margin
#' plot(1:10)
#' 
#' #
#' # Quick example going the other way, "correcting" for par.restore = TRUE...
#' #
#' 
#' tinyplot(Sepal.Length ~ Petal.Length | Species, iris, restore.par = TRUE)
#' # Our added best lines will be wrong b/c of misaligned par
#' for (s in levels(iris$Species)) {
#'   abline(
#'     lm(Sepal.Length ~ Petal.Length, iris, subset = Species==s),
#'     col = which(levels(iris$Species)==s), lty = 2
#'   )
#' }
#' # grab the par settings from the _end_ of the preceding tinyplot call to fix
#' tpar(get_saved_par("after"))
#' # now the best lines are correct
#' for (s in levels(iris$Species)) {
#'   abline(
#'     lm(Sepal.Length ~ Petal.Length, iris, subset = Species==s),
#'     col = which(levels(iris$Species)==s)
#'   )
#' }
#' 
#' # reset again to original saved par settings before exit
#' tpar(sp)
#'
#' @export
get_saved_par <- function(when = c("before", "after")) {
  when = match.arg(when)
  par_env_name = paste0(".saved_par_", when)
  return(get(par_env_name, envir = get(".tinyplot_env", envir = parent.env(environment()))))
}

# (non-exported) companion function(s) for setting the original pars
set_saved_par <- function(when = c("before", "after"), value) {
  when = match.arg(when)
  par_env_name = paste0(".saved_par_", when)
  assign(par_env_name, value, envir = get(".tinyplot_env", envir = parent.env(environment())))
}
