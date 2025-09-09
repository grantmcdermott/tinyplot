#' tinyplot_settings S4 class
#' @keywords internal
setClass("tinyplot_settings",
  slots = list(
    type = "ANY",
    x = "ANY",
    y = "ANY",
    dots = "ANY",
    type_data = "ANY",
    type_draw = "ANY"
  )
)


#' Initialize settings object
#' @keywords internal
setMethod("initialize", "tinyplot_settings", function(.Object, ...) {
  # Get all arguments passed to new()
  args = list(...)

  # Set only the slots needed for sanitize_type()
  .Object@type = args$type
  .Object@x = args$x
  .Object@y = args$y
  .Object@dots = args$dots

  # Initialize output slots with default values
  .Object@type_data = NULL
  .Object@type_draw = NULL

  return(.Object)
})


#' Assign settings slots to environment
#' @keywords internal
settings_to_environment = function(settings, environment) {
  slot_names = slotNames(settings)
  for (slot_name in slot_names) {
    assign(slot_name, slot(settings, slot_name), envir = environment)
  }
  invisible(NULL)
}
