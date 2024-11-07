#' Polypath polygon type
#' 
#' @description Type function for plotting polygons.
#' Arguments are passed to \code{\link[graphics]{polypath}}.
#' 
#' @inheritParams graphics::polypath
#' 
#' @examples
#' # "polypath" type convenience character string
#' tinyplot(
#'     c(.1, .1, .6, .6, NA, .4, .4, .9, .9),
#'     c(.1, .6, .6, .1, NA, .4, .9, .9, .4),
#'     type = "polypath", fill = "grey"
#' )
#' 
#' # Use `type_polypath()` to pass extra arguments for customization
#' tinyplot(
#'     c(.1, .1, .6, .6, NA, .4, .4, .9, .9),
#'     c(.1, .6, .6, .1, NA, .4, .9, .9, .4),
#'     type = type_polypath(rule = "evenodd"), fill = "grey"
#' )
#' @export
type_polypath = function(rule = "winding") {
    draw_polypath = function() {
        fun =  function(ix, iy, icol, ibg, ilty, ilwd, dots, ...) {
            polypath(
                x = ix,
                y = iy,
                border = icol,
                col = ibg,
                lty = ilty,
                lwd = ilwd,
                rule = rule
            )
        }
        return(fun)
    }

    out = list(
        draw = draw_polypath(),
        data = NULL,
        name = "polypath"
    )
    class(out) = "tinyplot_type"
    return(out)
}

