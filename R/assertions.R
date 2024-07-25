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

assert_length = function(x, len = 1, null.ok = FALSE, name = as.character(substitute(x))) {
  if (is.null(x) && isTRUE(null.ok)) {
    return(invisible(TRUE))
  }
  msg = sprintf("`%s` must be one of these lengths: %s", name, paste(len, collapse = ", "))
  if (!length(x) %in% len) {
    stop(msg, call. = FALSE)
  }
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

