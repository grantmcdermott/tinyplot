get_settings <- function(settings, keys) {
  mget(keys, settings)
}


env2env <- function(source_env, target_env, keys = NULL) {
  if (is.null(keys)) {
    keys <- ls(source_env, all.names = TRUE)
  }
  for (nm in keys) {
    assign(nm, source_env[[nm]], envir = target_env)
  }
}
