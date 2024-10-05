sanitize_ribbon.alpha = function(ribbon.alpha) {
    assert_numeric(ribbon.alpha, len = 1, lower = 0, upper = 1, null.ok = TRUE)
    if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
    return(ribbon.alpha)
}



sanitize_type = function(type, x, y) {
    if (inherits(type, "tinyplot_type")) {
        return(type)
    }

    if (is.null(type)) {
        # enforce boxplot type for y ~ factor(x)
        if (!is.null(x) && is.factor(x) && !is.factor(y)) {
            return(type_boxplot())
        } else {
            type = "p"
        }
    } else if (type %in% c("hist", "histogram")) {
        type = "histogram"
    } else if (type %in% c("j", "jitter")) {
        type = return(type_jitter())
    }

    if (identical(type, "points")) {
        return(type_points())
    } else if (identical(type, "polypath")) {
        return(type_polypath())
    } else if (identical(type, "pointrange")) {
        return(type_pointrange())
    } else if (identical(type, "errorbar")) {
        return(type_errorbar())
    } else if (identical(type, "boxplot")) {
        return(type_boxplot())
    } else if (isTRUE(type %in% c("j", "jitter"))) {
        return(type_jitter())
    }

    out = list(draw = NULL, data = NULL, name = type)
    return(out)
}
