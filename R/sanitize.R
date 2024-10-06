sanitize_ribbon.alpha = function(ribbon.alpha) {
    assert_numeric(ribbon.alpha, len = 1, lower = 0, upper = 1, null.ok = TRUE)
    if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
    return(ribbon.alpha)
}



sanitize_type = function(type, x, y) {
    if (inherits(type, "tinyplot_type")) {
        return(type)
    }

    types = c("area", "boxplot", "ribbon", "pointrange", "histogram", "errorbar", "polygon", "polypath", "rect", "segments", "points", "p", "l", "o", "b", "c", "h", "s", "S", "n", "loess", "lm", "glm")
    assert_choice(type, types, null.ok = TRUE)

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
    } else if (identical(type, "segments")) {
        return(type_segments())
    } else if (identical(type, "area")) {
        return(type_area())
    } else if (identical(type, "rect")) {
        return(type_rect())
    } else if (identical(type, "polypath")) {
        return(type_polypath())
    } else if (identical(type, "pointrange")) {
        return(type_pointrange())
    } else if (identical(type, "errorbar")) {
        return(type_errorbar())
    } else if (identical(type, "boxplot")) {
        return(type_boxplot())
    } else if (identical(type, "ribbon")) {
        return(type_ribbon())
    } else if (identical(type, "histogram")) {
        return(type_histogram())
    } else if (isTRUE(type %in% c("j", "jitter"))) {
        return(type_jitter())
    # statistical functions
    } else if (identical(type, "loess")) {
        return(type_loess())
    } else if (identical(type, "glm")) {
        return(type_glm())
    } else if (identical(type, "lm")) {
        return(type_lm())
    }

    out = list(draw = NULL, data = NULL, name = type)
    return(out)
}
