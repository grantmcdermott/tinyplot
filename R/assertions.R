check_dependency = function(library_name) {
  flag = requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
    msg = sprintf("Please install the `%s` package.", library_name)
    return(msg)
  } else {
    return(TRUE)
  }
}

assert_dependency = function(library_name) {
  flag = check_dependency(library_name)
  if (!isTRUE(flag)) stop(flag, call. = FALSE)
  return(invisible())
}

assert_choice = function(x, choice, null.ok = FALSE, name = as.character(substitute(x))) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(TRUE)
  }
  if (is.character(x) && length(x) == 1 && x %in% choice) {
    return(TRUE)
  }
  msg = sprintf(
    "`%s` must be one of: %s",
    name,
    paste(choice, collapse = ", ")
  )
  stop(msg, call. = FALSE)
}

check_true = function(x, null.ok = FALSE) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(invisible(TRUE))
  }
  if (isTRUE(x)) {
    return(invisible(TRUE))
  }
  return(FALSE)
}

assert_true = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  msg = sprintf("`%s` must be true.", name)
  if (!isTRUE(check_true(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}

check_string = function(x, null.ok = FALSE) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(invisible(TRUE))
  }
  if (is.character(x) && length(x) == 1) {
    return(invisible(TRUE))
  }
  return(FALSE)
}

assert_string = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  msg = sprintf("`%s` must be a string.", name)
  if (!isTRUE(check_string(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}

check_flag = function(x, null.ok = FALSE) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(TRUE)
  }
  if (is.logical(x) && length(x) == 1) {
    return(TRUE)
  }
  return(FALSE)
}

assert_flag = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  msg = sprintf("`%s` must be a logical flag.", name)
  if (!isTRUE(check_flag(x, null.ok = null.ok))) {
    stop(msg, call. = FALSE)
  }
}

# facet titles prefix: TRUE/FALSE, or custom name(s) for the facet variable(s).
# The latter can be given as a character vector or as a list of strings (the
# vector is the canonical form, but `labeller` takes a list and there is no
# reason to reject the same container here).
assert_facet_prefix = function(x, name = as.character(substitute(x))) {
  if (is.null(x)) return(invisible(TRUE))
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(invisible(TRUE))
  if ((is.character(x) || is.list(x)) && length(x) >= 1L) {
    if (all(vapply(x, is_string1, logical(1L)))) return(invisible(TRUE))
  }
  stop(
    sprintf(
      "`%s` must be a logical flag, or facet variable name(s) supplied as a character vector or list of strings.",
      name
    ),
    call. = FALSE
  )
}

is_string1 = function(x) {
  isTRUE(check_string(x)) && !is.na(x)
}

# label formatter passed on to tinylabel(): a function, one of its convenience
# strings (e.g. "percent"), or a dictionary. With `list.ok`, several of them --
# as a list or character vector -- are allowed too, e.g. one per facet variable.
#
# A dictionary is only recognised *inside* that list, never as `x` itself: at
# the top level a named vector is read as a per-variable mapping instead, which
# is what makes `list(Species = c(setosa = "SET"))` mean something different
# from a bare `c(setosa = "SET")`. match_facet_vars() enforces the top-level
# reading; this only has to let the nested one through.
assert_labeller = function(x, name = as.character(substitute(x)), list.ok = FALSE) {
  if (is.null(x) || is_labeller(x)) return(invisible(TRUE))
  if (isTRUE(list.ok) && (is.list(x) || is.character(x)) && length(x) >= 1L) {
    ok = vapply(x, function(xi) is_labeller(xi) || is_dict(xi), logical(1L))
    if (all(ok)) return(invisible(TRUE))
  }
  msg = if (isTRUE(list.ok)) {
    "`%s` must be a function, a `tinylabel()` convenience string, or a dictionary of labels -- or a list of them, one per facet variable."
  } else {
    "`%s` must be a function, or a `tinylabel()` convenience string."
  }
  stop(sprintf(msg, name), call. = FALSE)
}

is_labeller = function(x) {
  is.function(x) || (is.character(x) && length(x) == 1L && !is.na(x))
}

# A tinylabel() dictionary: a *named* character vector, or a named list that
# flattens to one, mapping existing labels to their replacements. Deliberately
# mirrors what tinylabel() itself dispatches on (see R/tinylabel.R), so that
# what passes validation here is exactly what it can consume. Note the overlap
# with is_labeller() at length 1, where a one-entry dictionary and a
# convenience string cannot be told apart -- and need not be, since both are
# accepted either way.
is_dict = function(x) {
  if (is.list(x) && !is.null(names(x))) x = unlist(x)
  is.character(x) && !is.null(names(x))
}

assert_length = function(x, len = 1, null.ok = FALSE, name = as.character(substitute(x))) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(invisible(TRUE))
  }
  msg = sprintf("`%s` must be one of these lengths: %s", name, paste(len, collapse = ", "))
  if (!length(x) %in% len) {
    stop(msg, call. = FALSE)
  }
}

assert_grid = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  if (is.null(x) && isTRUE(null.ok)) return(invisible(TRUE))
  if (is.logical(x) && length(x) == 1) return(invisible(TRUE))
  valid_strings = c("x", "y", "X", "Y", "xy", "xY", "Xy", "XY")
  if (is.character(x) && length(x) == 1 && x %in% valid_strings) return(invisible(TRUE))
  msg = sprintf("`%s` must be a logical flag or a character string combining x/X and/or y/Y (e.g., \"xy\", \"XY\", \"xY\")", name)
  stop(msg, call. = FALSE)
}

assert_logical = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(invisible(TRUE))
  }
  msg = sprintf("`%s` must be a logical vector", name)
  if (!is.logical(x)) stop(msg, call. = FALSE)
}


check_integerish = function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(TRUE)
  }
  if (!is.numeric(x)) {
    return(FALSE)
  }
  x = stats::na.omit(x)
  if (!is.null(len) && length(x) != len) {
    return(FALSE)
  }
  if (!is.null(lower) && any(x < lower)) {
    return(FALSE)
  }
  if (!is.null(upper) && any(x > upper)) {
    return(FALSE)
  }
  if (isTRUE(any(abs(x - round(x)) > (.Machine$double.eps)^0.5))) {
    return(FALSE)
  }
  return(TRUE)
}

assert_integerish = function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(invisible())
  }
  msg = sprintf("`%s` must be integer-ish", name)
  if (is.null(x) && !isTRUE(null.ok)) stop(sprintf("%s should not be NULL.", name), call. = FALSE)
  if (!isTRUE(check_integerish(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
    if (!is.numeric(x)) msg = paste0(msg, "; it is not numeric")
    if (!is.null(len) && length(x) != len) msg = paste0(msg, sprintf("; its length must be %s", len))
    if (!is.null(lower) && any(x < lower)) msg = paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
    if (!is.null(upper) && any(x > upper)) msg = paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
    if (isTRUE(any(abs(x - round(x)) > (.Machine$double.eps)^0.5))) msg = paste0(msg, "; all values must be close to integers")
    stop(msg, call. = FALSE)
  }
}

check_numeric = function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(TRUE)
  }
  if (!is.numeric(x)) {
    return(FALSE)
  }
  if (!is.null(len) && length(x) != len) {
    return(FALSE)
  }
  if (!is.null(lower) && any(x < lower)) {
    return(FALSE)
  }
  if (!is.null(upper) && any(x > upper)) {
    return(FALSE)
  }
  return(TRUE)
}

assert_numeric = function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  msg = sprintf("`%s` must be numeric", name)
  if (!isTRUE(check_numeric(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
    if (!is.null(len) && length(x) != len) msg = paste0(msg, sprintf("; its length must be %s", len))
    if (!is.null(lower) && any(x < lower)) msg = paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
    if (!is.null(upper) && any(x > upper)) msg = paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
    stop(msg, call. = FALSE)
  }
}

assert_data_frame = function(x, min_rows = 0, min_cols = 0, name = as.character(substitute(x))) {
  msg = sprintf("`%s` must be a data.frame.", name)
  if (!is.data.frame(x)) stop(msg, call. = FALSE)
  msg = sprintf("Number of rows in `%s` must be at least `%s`", name, min_rows)
  if (nrow(x) < min_rows) stop(msg, call. = FALSE)
  msg = sprintf("Number of columns in `%s` must be at least `%s`", name, min_cols)
  if (ncol(x) < min_cols) stop(msg, call. = FALSE)
}


check_character = function(x, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(TRUE)
  } else if (!is.character(x)) {
    msg = sprintf("`%s` must be character.", name)
    return(msg)
  } else if (!is.null(len) && length(x) != len) {
    msg = sprintf("`%s` must have length %s.", name, len)
    return(msg)
  }
  return(TRUE)
}

assert_character = function(x, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  flag = check_character(x, len = len, null.ok = null.ok, name = name)
  if (!isTRUE(flag)) {
    stop(flag, call. = FALSE)
  } else {
    return(invisible(TRUE))
  }
}

assert_list = function(x, named = FALSE, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.list(x)) stop("Input is not a list.", call. = FALSE)
  if (isTRUE(named)) {
    if (is.null(names(x))) {
      stop(sprintf("`%s` should be named list.", name), call. = FALSE)
    }
  }
  if (!is.null(len)) {
    if (length(x) != len) {
      stop(sprintf("`%s` must be of length %s.", name, len), call. = FALSE)
    }
  }
}

assert_function = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.function(x)) {
    msg = sprintf("`%s` must be a function.", name)
    stop(msg, call. = FALSE)
  }
}

check_atomic_vector = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(invisible(TRUE))
  }
  # doesn't work on glue::glue() output
  # flag = is.atomic(x) && is.vector(x) && !is.list(x)
  flag = is.atomic(x) && is.null(dim(x)) && length(x) > 0 && !is.list(x)
  if (flag) {
    out = TRUE
  } else if (is.factor(x) && is.null(dim(x))) {
    out = TRUE
  } else {
    out = sprintf("`%s` must be an atomic vector.", name)
  }
  return(out)
}

assert_atomic_vector = function(x, null.ok = FALSE, name = as.character(substitute(x))) {
  flag = check_atomic_vector(x, null.ok = null.ok, name = name)
  if (!isTRUE(flag)) {
    stop(flag, call. = FALSE)
  } else {
    return(invisible(TRUE))
  }
}

assert_class = function(x, classname) {
  if (!inherits(x, classname)) {
    msg = sprintf("`x` must be of class `%s`.", classname)
    stop(msg, call. = FALSE)
  }
}


## The complete set of behaviour hints that a type may declare for itself, via
## `settings$type_hints` in its `data_<type>()` function. Single source of truth:
## the Types vignette documents these, and assert_type_hints() validates against
## it. Keep all three in sync when adding a hint.
known_type_hints = c(
  "draws_own_axes",                # draws its own axes despite xaxt/yaxt = "n"
  "framed",                        # the user's real `frame.plot`, where forced
  "has_proportional_lim",          # limits are proportional; don't expand them
  "has_rhs_axis",                  # secondary right-hand axis (reserve margin)
  "legend_border_fg",              # legend swatch border is always par("fg")
  "legend_fills_from_col",         # legend swatch fill comes from `col`
  "legend_fills_from_seq_palette", # ... or from the colour's sequential ramp
  "legend_reversed"                # list the key bottom-up, not top-down
)

## Validate a type's declared hints.
##
## Warns rather than errors: an unrecognised hint is inert (every consumer reads
## hints through isTRUE()), so the plot is still drawn. That inertness is exactly
## why the check is worth having, though -- a misspelled hint behaves identically
## to an absent one, which would otherwise leave a custom-type author with no
## signal at all. Warning also keeps a custom type written against a newer
## tinyplot from hard-failing on an older one.
assert_type_hints = function(x, name = "type_hints") {
  if (is.null(x)) return(invisible(TRUE))
  if (!is.list(x) || (length(x) > 0L && is.null(names(x)))) {
    warning(
      sprintf("`%s` must be a named list. Ignoring.", name),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  unknown = setdiff(names(x), known_type_hints)
  if (length(unknown) > 0L) {
    warning(
      sprintf(
        "Unknown %s: %s.\nKnown hints are: %s.\nUnknown hints are ignored.",
        if (length(unknown) > 1L) "hints" else "hint",
        paste(sprintf('"%s"', unknown), collapse = ", "),
        paste(sprintf('"%s"', known_type_hints), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # Values must be length-1 logicals: the consumers all test with isTRUE(), so
  # anything else silently reads as FALSE.
  bad = names(x)[!vapply(x, check_flag, logical(1L))]
  bad = setdiff(bad, unknown) # don't double-report an unknown hint
  if (length(bad) > 0L) {
    warning(
      sprintf(
        "%s must be a logical flag: %s. Ignoring.",
        if (length(bad) > 1L) "These hints" else "This hint",
        paste(sprintf('"%s"', bad), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(length(unknown) == 0L && length(bad) == 0L)
}

