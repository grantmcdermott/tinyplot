# Copied from https://raw.githubusercontent.com/r-lib/evaluate/refs/heads/main/R/hooks.R
# license: MIT + file LICENSE



#' Set and remove hooks
#'
#' This interface wraps the base [setHook()] function to provide a return
#' value that makes it easy to undo.
#'
#' @param hooks a named list of hooks - each hook can either be a function or
#'   a list of functions.
#' @param action `"replace"`, `"append"` or `"prepend"`
#' @keywords internal
set_hooks = function(hooks, action = "append") {
  old = list()
  for (hook_name in names(hooks)) {
    old[[hook_name]] = getHook(hook_name)
    setHook(hook_name, hooks[[hook_name]], action = action)
  }
  invisible(old)
}

#' @rdname set_hooks
#' @keywords internal
remove_hooks = function(hooks) {
  for (hook_name in names(hooks)) {
    hook = getHook(hook_name)
    if (length(hook) > 0) {
      for (fun in unlist(hooks[hook_name])) {
        hook[sapply(hook, identical, fun)] = NULL
      }
    }
    setHook(hook_name, hook, "replace")
  }
}
