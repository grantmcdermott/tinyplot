#' auxiliary Axis() interface with different parameter combinations based on type
#'
#' @keywords internal
tinyAxis = function(x = NULL, ..., type = "standard", labeller = NULL) {
  type = match.arg(type, c("standard", "none", "labels", "ticks", "axis"))
  if (type == "none") {
    invisible(numeric(0L))
  } else {
    args = list(x = x, ...)
    if (type == "labels") {
      args$tick = FALSE
    } else if (type == "ticks") {
      args$lwd = 0
      if (!("lwd.ticks" %in% names(args))) args$lwd.ticks = 1
    } else if (type == "axis") {
      args$lwd.ticks = 0
    } else {
      args$tick = TRUE
    }
    if (!is.null(labeller)) {
      # if (is.character(labeller)) labeller = labeller_fun((labeller))
      if (!is.null(args$at)) {
        # args$labels = if (!is.null(args$labels)) labeller(args$labels) else labeller(args$at)
        args$labels = if (!is.null(args$labels)) tinylabel(args$labels, labeller) else tinylabel(args$at, labeller)
      } else {
        args$at = axTicks(args$side) # FIXME: log ?
        args$labels = tinylabel(args$at, labeller)
      }
    }
    do.call("Axis", args)
  }
}


# labeller_fun = function(label = c("percent", "comma", "dollar")) {
#   label = match.arg(label)
#   
#   format_percent = function(x) {
#     sprintf("%.0f%%", x * 100)
#   }
#   
#   format_comma = function(x) {
#     prettyNum(x, big.mark = ",", scientific = FALSE)
#   }
#   
#   format_dollar = function(x) {
#     paste0("$", prettyNum(x, big.mark = ",", scientific = FALSE))
#   }
#   
#   switch(
#     label,
#     percent = format_percent,
#     comma  = format_comma,
#     dollar = format_dollar
#   )
# }

