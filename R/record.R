## Recorded tinyplot objects -----
##
## A plain "recordedplot" carries the display list and nothing else, so
## replaying one restores the pixels but not the context that tinyplot_add()
## needs. The result is that
##
##   p = tinyplot(..., record = TRUE); tinyplot(1); p; tinyplot_add(type = "lm")
##
## layers onto tinyplot(1) rather than onto p, because .last_call still points
## at the intervening plot. Wrapping the recording lets us carry that state
## along and put it back when the plot is replayed.


# Plot-scoped entries of .tinyplot_env: everything tinyplot_add() and the
# layering machinery need in order to treat a replayed plot as "the current
# plot". Deliberately excludes package-level config that is not tied to any
# single plot: .base_par_names (a device cache), .registered_themes and
# .tpar_hooks (user/session settings), and .saved_par_first (the session's
# baseline par, which a replay has no business overwriting).
recorded_state_keys = c(
  ".last_call",
  ".saved_par_before",
  ".saved_par_after",
  ".group_offsets",
  ".offsets_axis",
  ".facet_labs",
  ".top_legend_soma",
  "usr_orig",
  "dev_orig",
  "xlabs_orig"
)


# Snapshot the plot-scoped state, to be stashed on a recorded plot.
capture_record_state = function() {
  out = lapply(recorded_state_keys, function(k) .tinyplot_env[[k]])
  names(out) = recorded_state_keys
  # Drop `record` from the stored call. tinyplot_add() rebuilds the last call,
  # so leaving it in would make every layer added after a replay record itself
  # too -- and warn if that device happens not to be recording. Recording
  # describes how one call returned its value, not a property of the plot to be
  # inherited; pass `record = TRUE` to tinyplot_add() explicitly to record a
  # layered plot.
  cal = out[[".last_call"]]
  if (is.call(cal) && "record" %in% names(as.list(cal))) {
    cal[["record"]] = NULL
    out[[".last_call"]] = cal
  }
  return(out)
}


# Put a snapshot back, so that a subsequent tinyplot_add() sees the replayed
# plot as the current one.
restore_record_state = function(state) {
  if (!is.list(state)) return(invisible(NULL))
  for (k in intersect(names(state), recorded_state_keys)) {
    .tinyplot_env[[k]] = state[[k]]
  }
  return(invisible(NULL))
}


# Wrap a recordedplot, stashing the state alongside it. The result still
# inherits from "recordedplot", so replayPlot() and everything else that
# expects a bare recording keep working.
as_recordedtinyplot = function(rec) {
  attr(rec, "tinyplot_state") = capture_record_state()
  class(rec) = c("recordedtinyplot", "recordedplot")
  return(rec)
}


#' @title Recorded tinyplot objects
#'
#' @description Objects of class `recordedtinyplot` are returned by
#' `tinyplot(..., record = TRUE)`. They are a thin wrapper around
#' \code{\link[grDevices]{recordPlot}}---thus conferring the same replay
#' functionality via \code{\link[grDevices]{replayPlot}}---but with the
#' initializing plot call and state added, so that recorded (tiny)plots play
#' nicely with \code{\link{tinyplot_add}()} and friends.
#'
#' @param x a `recordedtinyplot` object, returned by a
#' `tinyplot(..., record = TRUE)` call.
#' @param ... further arguments passed to \code{\link[grDevices]{replayPlot}}.
#'
#' @returns `print()` replays the plot and returns `x` invisibly.
#'
#' @details Recording requires a device whose display list is enabled (see
#' \code{\link[grDevices]{dev.control}}); without one there is nothing to
#' record and the returned plot will replay blank. Interactive devices---the
#' plot pane of an IDE, say---typically enable it by default, whereas
#' file-based devices (`png`, `pdf`, `svg`, ...) do not. `tinyplot()` enables
#' it for any device that it opens itself via the `file` argument, and warns
#' if the current device is not recording.
#'
#' This class is experimental, as is the `record` argument that produces it.
#'
#' @seealso \code{\link[grDevices]{recordPlot}} and
#' \code{\link[grDevices]{replayPlot}}, which this class wraps and defers to
#' for the actual recording and replaying.
#' \code{\link[grDevices]{dev.control}} for enabling a device's display list,
#' without which there is nothing to record.
#'
#' @examples
#' \dontrun{
#' p = tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
#'              record = TRUE)
#' tinyplot(1:10)  # some other plot
#' p               # replay, restoring plot context
#' tinyplot_add(type = "lm")  # layers onto p, not onto the 1:10 plot
#' }
#'
#' @name recordedtinyplot
#' @importFrom grDevices replayPlot
#' @export
print.recordedtinyplot = function(x, ...) {
  # Strip our class before handing off, so we replay as a plain recording
  # rather than recursing back into this method.
  rec = x
  attr(rec, "tinyplot_state") = NULL
  class(rec) = "recordedplot"
  replayPlot(rec, ...)
  # Only now adopt the replayed plot as the current one: replaying redraws it,
  # so tinyplot_add() should target it rather than the previous plot.
  restore_record_state(attr(x, "tinyplot_state"))
  return(invisible(x))
}


#' @rdname recordedtinyplot
#' @param object a `recordedtinyplot` object.
#' @export
str.recordedtinyplot = function(object, ...) {
  # The underlying display list str()s to hundreds of lines of dotted pair
  # lists, which is noise in an object viewer. Report what is useful instead.
  state = attr(object, "tinyplot_state")
  cal = state[[".last_call"]]
  cat("<tinyplot recorded plot>\n")
  if (!is.null(cal)) {
    cat(" call: ", paste(deparse(cal), collapse = " "), "\n", sep = "")
  }
  cat(" display list entries: ", length(object[[1]]), "\n", sep = "")
  cat(" size: ", format(utils::object.size(object), units = "auto"), "\n", sep = "")
  return(invisible(NULL))
}
