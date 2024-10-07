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

