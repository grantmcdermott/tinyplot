#' Force-directed text repelling
#'
#' Resolves overlapping text labels using a two-phase algorithm: (1) push
#' overlapping labels apart symmetrically, (2) spring-pull labels back toward
#' their anchors without reintroducing overlaps.
#'
#' @param x,y Numeric vectors of current label positions.
#' @param widths,heights Numeric vectors of label bounding box dimensions (in
#'   the same units as `x` and `y`).
#' @param anchor_x,anchor_y Numeric vectors of original target positions that
#'   labels are pulled back toward during the spring pass. Defaults to `x`
#'   and `y`.
#' @param min_gap Minimum padding between labels. Default `0`.
#' @param iterations Maximum number of repulsion iterations. Default `20`.
#' @param axis Movement constraint: `"both"` (default), `"x"`, or `"y"`.
#' @return A list with elements `x` and `y` giving adjusted positions.
#' @keywords internal
repel_text = function(x, y, widths, heights,
                        anchor_x = x, anchor_y = y,
                        min_gap = 0, iterations = 20,
                        axis = "both") {
  n = length(x)
  if (n <= 1) return(list(x = x, y = y))

  valid = !is.na(x) & !is.na(y)
  vx = x[valid]
  vy = y[valid]
  ax = anchor_x[valid]
  ay = anchor_y[valid]
  vw = widths[valid]
  vh = heights[valid]
  nv = sum(valid)

  move_x = axis %in% c("both", "x")
  move_y = axis %in% c("both", "y")

  # Phase 1: resolve overlaps by pushing apart (full overlap each step)
  for (iter in seq_len(iterations)) {
    any_overlap = FALSE
    for (i in seq_len(nv - 1)) {
      for (j in (i + 1):nv) {
        ox = (vw[i] + vw[j]) / 2 + min_gap - abs(vx[i] - vx[j])
        oy = (vh[i] + vh[j]) / 2 + min_gap - abs(vy[i] - vy[j])

        overlapping = if (axis == "y") oy > 0
          else if (axis == "x") ox > 0
          else ox > 0 && oy > 0
        if (overlapping) {
          any_overlap = TRUE
          push_y = move_y && (oy <= ox || !move_x)
          if (push_y) {
            push = oy / 2
            if (vy[i] <= vy[j]) {
              vy[i] = vy[i] - push
              vy[j] = vy[j] + push
            } else {
              vy[i] = vy[i] + push
              vy[j] = vy[j] - push
            }
          } else {
            push = ox / 2
            if (vx[i] <= vx[j]) {
              vx[i] = vx[i] - push
              vx[j] = vx[j] + push
            } else {
              vx[i] = vx[i] + push
              vx[j] = vx[j] - push
            }
          }
        }
      }
    }
    if (!any_overlap) break
  }

  # Phase 2: spring pass — pull toward anchors without reintroducing overlaps
  displacements = (vx - ax)^2 + (vy - ay)^2
  for (idx in order(displacements, decreasing = TRUE)) {
    step_x = if (move_x) (ax[idx] - vx[idx]) * 0.5 else 0
    step_y = if (move_y) (ay[idx] - vy[idx]) * 0.5 else 0
    new_x = vx[idx] + step_x
    new_y = vy[idx] + step_y

    overlaps = FALSE
    for (k in seq_len(nv)) {
      if (k == idx) next
      ox = (vw[idx] + vw[k]) / 2 + min_gap - abs(new_x - vx[k])
      oy = (vh[idx] + vh[k]) / 2 + min_gap - abs(new_y - vy[k])
      hit = if (axis == "y") oy > 0
        else if (axis == "x") ox > 0
        else ox > 0 && oy > 0
      if (hit) {
        overlaps = TRUE
        break
      }
    }
    if (!overlaps) {
      vx[idx] = new_x
      vy[idx] = new_y
    }
  }

  x[valid] = vx
  y[valid] = vy
  list(x = x, y = y)
}
