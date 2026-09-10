## Guards shared by the `*levels` / `*ord` argument pair. Both members control
## the same thing -- the order of a categorical variable's levels -- so these
## are named for that concept rather than for either argument.
##
## Note the pair is resolved by precedence rather than by a guard: `*levels`
## wins and `*ord` is skipped at the call site when both are given. That keeps
## the types free of supplied-vs-default bookkeeping, which would otherwise be
## needed because `type_errorbar()` and `type_pointrange()` default `xord` to
## "asis".

## Warn when `*levels` / `*ord` were supplied for an axis that cannot be
## reordered. Types that coerce their categorical axis to a factor (barplot,
## ridge) never reach this -- there the arguments always apply. For the point
## and line family a numeric `x` is plotted at its own values, so the request
## is silently dropped, which is the failure mode worth surfacing.
##
## `supplied` is passed by the caller rather than inferred, because these
## arguments reach here through a closure: `type_errorbar()` and
## `type_pointrange()` default `xord` to "asis", and warning about a default
## the user never typed would fire on every numeric-x coefficient plot.
warn_ignored_ordering = function(v, xlevels, ord, supplied = TRUE) {
  nms = c(deparse(substitute(xlevels)), deparse(substitute(ord)))
  if (is.factor(v) || !isTRUE(supplied)) {
    return(invisible(NULL))
  }
  given = c(if (!is.null(xlevels)) nms[1L], if (!is.null(ord)) nms[2L])
  if (length(given) == 0L) {
    return(invisible(NULL))
  }
  warning(
    sprintf(
      "ignoring '%s': only categorical (factor or character) variables can be reordered.",
      paste(given, collapse = "' and '")
    ),
    call. = FALSE
  )
  invisible(NULL)
}
