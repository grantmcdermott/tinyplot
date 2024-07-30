sanitize_ribbon.alpha = function(ribbon.alpha) {
    assert_numeric(ribbon.alpha, len = 1, lower = 0, upper = 1, null.ok = TRUE)
    if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
    return(ribbon.alpha)
}




sanitize_type = function(type, x, y) {
    # # enforce boxplot type for y ~ factor(x)
    # if (!is.null(x) && is.factor(x) && !is.factor(y) && !identical(type, "boxplot")) {
    #     type = "boxplot"
    #     warning('The `type` argument was changed to "boxplot" automatically because `x` is a factor but not `y`.', call. = FALSE)
    # }
    if (is.null(type)) {
        # enforce boxplot type for y ~ factor(x)
        if (!is.null(x) && is.factor(x) && !is.factor(y)) {
            type = "boxplot"
        } else {
            type = "p"
        }
    } else if (type %in% c("j", "jitter")) {
        type = "jitter"
    }
    return(type)
}
