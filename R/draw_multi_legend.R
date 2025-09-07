#' @title Draw multiple legends with automatic positioning
#'   
#' @description Internal function to draw multiple legends (e.g., bubble + color)
#'   with automatic dimension calculation and positioning. This function handles
#'   the internal gymnastics required to determine the individual legend
#'   dimensions, before drawing them in the optimal order and position.
#'   
#' @md
#' @param legend_list A list of legend arguments, where each element is itself a
#'   list of arguments that can be passed on to [draw_legend]. Legends will be
#'   drawn vertically (top to bottom) in the order that they are provided. Note
#'   that we currently only support dual legends, i.e. the top-level list has a
#'   maximum length of 2.
#' @param position String indicating the base keyword position for the
#'   multi-legend. Currently only `"right!"` and `"left!"` are supported.
#' 
#' @returns No return value, called for side effect of drawing multiple legends.
#' 
#' @seealso [draw_legend]
#' 
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' oldmar = par("mar")
#' 
#' # Dual legend example (color + bubble)
#' 
#' l1 = list(
#'   lgnd_labs = c("Red", "Blue", "Green"),
#'   legend_args = list(title = "Colors"),
#'   pch = 16,
#'   col = c("red", "blue", "green"),
#'   type = "p"
#' )
#' 
#' l2 = list(
#'   lgnd_labs = c("Tiny", "Small", "Medium", "Large", "Huge"),
#'   legend_args = list(title = "Size"),
#'   pch = 16,
#'   col = "black",
#'   cex = seq(0.5, 2.5, length.out = 5),
#'   type = "p"
#' )
#' 
#' # Draw together
#' draw_multi_legend(list(l1, l2), position = "right!")
#' 
#' par(mar = oldmar)
#' }
#' 
#' @keywords internal
draw_multi_legend = function(
    legend_list,
    position = "right!"
) {
  
  # Validate inputs
  if (!is.list(legend_list) || length(legend_list) != 2) {
    stop("Currently only 2 legends are supported in multi-legend mode")
  }
  
  # Currently only support right!/left! positioning
  if (!grepl("right!$|left!$", position)) {
    warning(
      '\nMulti-legends currently only work with "right!" or "left!" keyword positioning.\n',
      'Reverting to "right!" default\n'
    )
    position = "right!"
  }
  
  ## FIXME: current logic only works for "right!"/"left!" legend
  # Determine sub-positions based on main position
  if (grepl("right!$", position)) {
    sub_positions = c("bottomright!", "topright!")
  } else if (grepl("left!$", position)) {
    sub_positions = c("bottomleft!", "topleft!")
  }
  
  # Assign positions of individual legends
  for (ll in seq_along(legend_list)) {
    legend_list[[ll]][["legend"]] = sub_positions[ll]
    legend_list[[ll]][["legend_args"]][["x"]] = NULL
  }
  
  #
  ## Step 1: Extract legend dimensions (by drawing fake legends)
  #

  legend_dims = vector("list", length(legend_list))
  for (ll in seq_along(legend_list)) {
    legend_ll = legend_list[[ll]]
    legend_ll$new_plot = ll==1 ## only draw new plot for first legend
    legend_ll$draw = FALSE
    legend_dims[[ll]] = do.call(draw_legend, legend_ll)
  }

  #
  ## Step 2: Calculate sub-positioning based on dimensions
  #

  # Extract dimensions
  lwidths = sapply(legend_dims, function(x) x$rect$w)
  lheights = sapply(legend_dims, function(x) x$rect$h)
  # for inset adjustment, default to 0.5 unless one or more of the two legends
  # is bigger than half the plot height.
  linset = if (any(lheights > 0.5)) lheights[2] / sum(lheights) else 0.5

  #
  ## Step 3: Reposition (via adjusted an `inset` arg) and draw legends
  #
  
  # Note: we draw the legends in ascending order of width (i.e., widest legend
  #   last) in order to correctly set the overall plot dimensions.
  width_order = order(lwidths)

  # quick idx for original order (needed for vertical legend placement)
  for (i in seq_along(legend_list)) legend_list[[i]]$idx = i

  for (o in seq_along(width_order)) {
    io = width_order[o]
    legend_o = legend_list[[io]]
    legend_o$new_plot = FALSE
    legend_o$draw = TRUE
    legend_o$legend_args$inset = c(0, 0)
    legend_o$legend_args$inset[1] = if(o==1) -abs(diff(lwidths))/2 else 0
    legend_o$legend_args$inset[2] = if (legend_o$idx==1) linset + 0.01 else 1 - linset + 0.01
    legend_o$idx = NULL
    do.call(draw_legend, legend_o)
  }
  
  invisible(NULL)
}
