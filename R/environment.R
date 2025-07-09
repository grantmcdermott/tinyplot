init_environment = function() {
  tnypltptns = parent.env(environment())
  assign(".tinyplot_env", new.env(), envir = tnypltptns)
  .tpar = new.env()
  assign(".tpar", .tpar, envir = tnypltptns)
}

get_environment_variable = function(name) {
  get(name, envir = get(".tinyplot_env", envir = parent.env(environment())))
}

set_environment_variable = function(name, value) {
  assign(name, value, envir = get(".tinyplot_env", envir = parent.env(environment())))
}
