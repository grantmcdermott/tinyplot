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


# Plot-scoped entries of .tinyplot_env that tinyplot_add() and the layering
# machinery need in order to treat a replayed plot as "the current plot".
#
# Two kinds of entry are deliberately absent. First, package-level config that
# is not tied to any single plot: .base_par_names (a device cache),
# .registered_themes and .tpar_hooks (session settings). Second -- and less
# obviously -- the saved par/usr/dev state (.saved_par_before, .saved_par_after,
# usr_orig, dev_orig). That state describes the device the plot was *recorded*
# on, which need not be the device it is replayed onto: recording a plot with
# `file = ` captures a file device that is then closed, so restoring its par on
# replay leaves the live device inconsistent and a following tinyplot_add()
# fails with "plot.new has not been called yet". Replaying redraws the plot on
# the current device anyway, so that device's own par is the correct one to
# keep.
recorded_state_keys = c(
  ".last_call",
  ".group_offsets",
  ".offsets_axis",
  ".facet_labs",
  ".top_legend_soma",
  "xlabs_orig"
)


# Snapshot the plot-scoped state, to be stashed on a recorded plot.
capture_record_state = function() {
  out = lapply(recorded_state_keys, function(k) .tinyplot_env[[k]])
  names(out) = recorded_state_keys
  # Drop the per-call output directives from the stored call. tinyplot_add()
  # rebuilds the last call, so leaving these in would make every layer added
  # after a replay repeat them: `record` would re-record each layer (and warn
  # if that device was not recording), while `file`/`width`/`height` would open
  # a fresh device via setup_device() and then fail, because add-mode draws
  # onto a plot that the new device does not have. These arguments describe how
  # one call produced its output, not properties of the plot to be inherited;
  # pass them to tinyplot_add() explicitly if a layer needs them.
  cal = out[[".last_call"]]
  if (is.call(cal)) {
    drop = intersect(c("record", "file", "width", "height"), names(as.list(cal)))
    for (nm in drop) cal[[nm]] = NULL
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
as_recordedtinyplot = function(rec, flip = FALSE) {
  state = capture_record_state()
  # Needed to recompute usr_orig on replay, below.
  state[["flip"]] = isTRUE(flip)
  attr(rec, "tinyplot_state") = state
  class(rec) = c("recordedtinyplot", "recordedplot")
  return(rec)
}


#' @title Recorded tinyplot objects
#'
#' @description Objects of class `recordedtinyplot` are returned by
#' `tinyplot(..., record = TRUE)`. They are a thin wrapper around
#' \code{\link[grDevices]{recordPlot}}---thus conferring the same replay
#' functionality---but with the initializing plot call and state added, so that
#' recorded (tiny)plots play nicely with \code{\link{tinyplot_add}()} and
#' friends.
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
#' Printing a recorded plot---either explicitly with `print()`, or simply by
#' evaluating it at the console---is what restores the initializing call, so
#' that a subsequent \code{\link{tinyplot_add}()} layers onto it. Calling
#' \code{\link[grDevices]{replayPlot}()} on the object instead redraws it
#' just as it would any other recording, but leaves the current plot context
#' untouched; layering after that route targets whichever plot was drawn last.
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
  # replayPlot() re-executes the recorded par (a grDevices property, not
  # ours), which would leave a themed plot's cosmetic par on the live device
  # and leak into later plots. Restored below, once the replay is done. Only
  # the keys a theme can set are read: the coordinate system must be left as
  # the replay draws it, so that tinyplot_add() can still layer onto it.
  pre_par = par(intersect(names(theme_default), base_par_names()))
  replayPlot(rec, ...)
  # Only now adopt the replayed plot as the current one: replaying redraws it,
  # so tinyplot_add() should target it rather than the previous plot.
  state = attr(x, "tinyplot_state")
  restore_record_state(state)
  # align_layer() validates that a layer is being added to the plot it thinks
  # is current, by comparing usr_orig/dev_orig against the live device. Those
  # describe whichever plot was drawn last, which after a replay is the
  # *intervening* plot, so the comparison fails and categorical layers silently
  # skip alignment. Recompute them from the plot we just drew. Note we derive
  # these from the live device rather than restoring the recorded values: the
  # recording may come from a device that no longer exists (`file = `), and
  # restoring its par leaves the current device inconsistent.
  .tinyplot_env[["dev_orig"]] = dev.cur()
  .tinyplot_env[["usr_orig"]] = if (isTRUE(state[["flip"]])) {
    par("usr")[c(3, 4, 1, 2)]
  } else {
    par("usr")
  }
  # Undo the leak noted above, mirroring what an ephemeral theme does on exit.
  par(pre_par)
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
