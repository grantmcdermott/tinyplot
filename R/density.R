#' @importFrom methods as
#' @importFrom stats update density
#' @rdname tinyplot
#' @export
tinyplot.density = function(
    x = NULL,
    by = NULL,
    facet = NULL,
    facet.args = NULL,
    type = c("l", "area"),
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    pch = NULL,
    col = NULL,
    lty = NULL,
    lwd = NULL,
    bg = NULL,
    fill = NULL,
    restore.par = FALSE,
    ...) {
    type = match.arg(type)
    ## override if bg = "by"
    if (!is.null(bg) || !is.null(fill)) type = "area"

    # catch for facet_grid
    if (!is.null(facet)) {
        facet_attributes = attributes(facet)
        # facet_grid = attr(facet, "facet_grid")
    }

    if (inherits(x, "density")) {
        object = x
        legend_args = list(x = NULL)
        # Grab by label to pass on legend title to tinyplot.default
        legend_args[["title"]] = deparse(substitute(by))
    } else {
        ## An internal catch for non-density objects that were forcibly
        ## passed to tinyplot.density (e.g., via a one-side formula)
        if (anyNA(x)) {
            x = na.omit(x)
            if (!is.null(by)) by = by[-attr(x, "na.action")]
            if (!is.null(facet)) facet = facet[-attr(x, "na.action")]
            x = as.numeric(x)
        }
        object = density(x)
        legend_args = list(...)[["legend_args"]]
    }

    by_names = facet_names = NULL
    if (is.null(by) && is.null(facet)) {
        x = object$x
        y = object$y
    } else {
        x = eval(str2lang(object$data.name), envir = parent.frame())
        if (anyNA(x)) {
            x = na.omit(x)
            if (!is.null(by) && length(by) != length(x)) by = by[-attr(x, "na.action")]
            if (!is.null(facet) && length(facet) != length(x)) facet = facet[-attr(x, "na.action")]
            x = as.numeric(x)
        }
        if (is.null(facet) || identical(by, facet)) {
            split_x = split(x, f = by)
        } else if (is.null(by)) {
            split_x = split(x, f = facet)
        } else {
            split_x = split(x, f = list(by, facet), sep = "::")
        }
        # joint bandwidth
        bw_type = as.list(object$call[-1])[["bw"]]
        if (is.null(bw_type)) bw_type = stats::bw.nrd0 else bw_type = str2lang(paste0("bw.", bw))
        xs_mask = lengths(split_x) > 1
        bws = vapply(split_x[xs_mask], bw_type, numeric(1))
        bw = mean(bws, na.rm = TRUE)
        #
        split_object = lapply(split_x, function(xx) update(object, x = xx, bw = bw))
        by_names = names(split_object)
        if (all(grepl("::", by_names))) {
            by_names = strsplit(by_names, "::")
            facet_names = sapply(by_names, `[[`, 2)
            facet_names = tryCatch(as(facet_names, class(facet)), error = function(e) facet_names)
            by_names = sapply(by_names, `[[`, 1)
        } else if (identical(by, facet)) {
            facet_names = by_names ## yuck
        } else if (is.null(by) && !is.null(facet)) {
            facet_names = names(split_object)
        }
        by_names = tryCatch(as(by_names, class(by)), error = function(e) if (inherits(by, "factor")) as.factor(by_names) else by_names)
        # need to coerce facet variables to factors for faceting to work properly later on
        # if we originally passed a factor, try to preserve this order for grid arrangement
        if (inherits(facet, "factor")) {
            orig_len = nlevels(facet)
            new_len = length(facet_names)
            if (orig_len == new_len) {
                facet_names = levels(facet)
            } else {
                ## need to recycle names if nested in multiple by splits
                facet_names = rep(levels(facet), each = new_len / orig_len)
            }
        } else {
            facet_names = tryCatch(as(facet_names, class(facet)), error = function(e) facet_names)
            facet_names = tryCatch(as.factor(facet_names), error = function(e) facet_names)
        }

        split_object = lapply(seq_along(split_object), function(ii) {
            lst = list(
                x = split_object[[ii]]$x,
                y = split_object[[ii]]$y,
                n = split_object[[ii]]$n
            )
            if (!is.null(by)) {
                lst$by = rep_len(by_names[ii], length.out = length(lst$x))
            } else {
                lst$by = NULL
            }
            if (!is.null(facet)) {
                lst$facet = rep_len(facet_names[ii], length.out = length(lst$x))
            } else {
                lst$facet = NULL
            }
            return(lst)
        })
        ## combine element by element
        res = do.call(Map, c(c, split_object))
        ## now pull out the individual vectors
        x = res[["x"]]
        y = res[["y"]]
        by = res[["by"]]
        facet = res[["facet"]]
        bw = sprintf("%.4g", bw)
        n = res[["n"]]
        if (is.null(xlab)) {
            if (length(by_names) > 3 || length(facet_names) > 3) {
                n = c(n[1:3], "...")
            }
            n = paste0("[", paste(n, collapse = ", "), "]")
            xlab = paste0("N = ", n, "   Joint Bandwidth = ", bw)
        }
    }
    if (type == "area") {
        ymin = rep(0, length(y))
        ymax = y
        # set extra legend params to get bordered boxes with fill
        legend_args[["x.intersp"]] = 1.25
        legend_args[["lty"]] = 0
        legend_args[["pt.lwd"]] = 1
    }

    ## axes range
    if (is.null(xlim)) xlim = range(x)
    if (is.null(ylim)) ylim = range(y)

    ## nice labels and titles
    if (is.null(ylab)) ylab = "Density"
    if (is.null(xlab)) xlab = paste0("N = ", object$n, "   Bandwidth = ", sprintf("%.4g", object$bw))
    if (is.null(main)) main = paste0(paste(object$call, collapse = "(x = "), ")")

    # if (!is.null(facet)) attr(facet, "facet_grid") = facet_grid
    if (!is.null(facet)) {
        if (!is.null(facet_attributes[["levels"]])) {
            facet = factor(facet, levels = facet_attributes[["levels"]])
        } else {
            facet = factor(facet)
        }
        attr(facet, "facet_grid") = facet_attributes[["facet_grid"]]
    }

    tinyplot.default(
        x = x, y = y, by = by, facet = facet, facet.args = facet.args,
        type = type,
        xlim = xlim,
        ylim = ylim,
        # log = "",
        main = main,
        sub = sub,
        xlab = xlab,
        ylab = ylab,
        ann = ann,
        axes = axes,
        frame.plot = frame.plot,
        asp = asp,
        grid = grid,
        legend_args = legend_args,
        pch = pch,
        col = col,
        bg = bg,
        fill = fill,
        lty = lty,
        lwd = lwd,
        restore.par = restore.par,
        ...
    )
}


density_args = function(fargs, dots, by_dep) {
    fargs = utils::modifyList(fargs, dots)

    if (!is.null(fargs[["y"]])) {
        fargs[["y"]] = NULL
        message("\nNote: A `y` argument has been supplied, but will be ignored for density plots.\n")
    }

    fargs$type = "l"

    if (is.null(fargs$main)) fargs$main = NA

    if (is.null(fargs[["legend_args"]][["title"]])) {
        fargs[["legend_args"]][["title"]] = by_dep
    }

    if (!is.null(fargs[["legend"]]) && !is.null(fargs[["legend_args"]])) {
        if (is.atomic(fargs[["legend"]])) {
            fargs[["legend"]] = list(x = fargs[["legend"]])
        } else if (!is.list(fargs[["legend"]])) {
            fargs[["legend"]] = as.list(fargs[["legend"]])
        }

        if (is.null(names(fargs[["legend"]])[1]) || names(fargs[["legend"]])[1] == "") {
            names(fargs[["legend"]])[1] = "x"
        }

        fargs[["legend_args"]] = modifyList(fargs[["legend"]], fargs[["legend_args"]])
        fargs[["legend"]] = NULL
    }

    fargs$y = fargs$ymin = fargs$ymax = fargs$ylab = fargs$xlab = NULL
    return(fargs)
}
