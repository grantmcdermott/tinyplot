#' Violin and sina plot types
#' 
#' @md
#' @description Type functions for violin plots, which are an alternative to box
#'   plots for visualizing continuous distributions (by group) in the form of
#'   mirrored densities. `type_violin()` draws the smooth outline of each
#'   density, while `type_sina()` scatters the underlying observations as points
#'   within the same outline (similar to a beeswarm plot).
#' @inheritParams type_density
#' @param  trim logical indicating whether the densities should be trimmed to
#'   the range of the data. Default is `FALSE`. For `type_sina()` this only
#'   affects the envelope that bounds the displacement, so it matters mainly
#'   for exact alignment with a `type_violin()` layer drawn the same way.
#' @param width numeric (ideally in the range `[0, 1]`, although this isn't
#'   enforced) giving the normalized width of the individual violins. For
#'   `type_sina()` this is the width of the (undrawn) violin that the points
#'   are scattered inside.
#' @param lighten logical. Should the fills use a lighter, opaque tint of the
#'   series colour(s)? Default is `TRUE`, which keeps single- and multi-group
#'   displays consistent and lets the fill read cleanly over grid lines. Set to
#'   `FALSE` to use the fully-saturated palette colour(s) instead. Only
#'   applies to `type_violin()`, since `type_sina()` has no fill of its own.
#' @param singletons character string indicating what to do with singleton
#'   groups, i.e. combinations of `x`, `by`, and `facet` that consist of only 1
#'   row. Both types accept `"warn"`, which removes any singleton cases and
#'   emits a warning reporting how many there were, and `"drop"`, which does
#'   the same thing quietly. In either case the dropped groups may still be
#'   represented as empty violins or facets in your plot.
#'
#'   The remaining option differs by type, as does the default. `type_violin()`
#'   defaults to `"warn"` and also accepts `"none"`, which skips all singleton
#'   checks and retains the affected groups; possibly leading to an error. Note
#'   that singletons then require a numeric `bw`, since the data-driven
#'   bandwidth rules need at least 2 observations.
#'
#'   `type_sina()` instead defaults to `"keep"`, which draws the lone
#'   observation on its group's tick. No density can be estimated from a single
#'   point, but the point itself is still worth showing, and discarding an
#'   observation from what is fundamentally a scatter plot is worse than
#'   discarding an unrenderable violin. There is no `"none"` for this type,
#'   since `"keep"` already retains these cases without error.
#' @inherit stats::density details
#' @details See [`type_density`] for more details and considerations related to
#'   bandwidth selection and kernel types.
#'
#'   A sina plot (Sidiropoulos et al., 2018) is closely related to a beeswarm
#'   plot, but is arguably the more principled of the two. Both spread a
#'   group's observations sideways to expose its shape. A beeswarm does so by
#'   packing points until they no longer collide, which makes its width an
#'   artefact of the *rendering*: change the symbol size or the device and the
#'   swarm changes shape. A sina instead displaces each point by the kernel
#'   density at its own `y` value, so its width is a property of the *data*,
#'   stable across devices and comparable between groups. The trade-off is
#'   occlusion: a beeswarm guarantees that no point hides another, whereas a
#'   sina accepts the occasional overlap. If you need collision-free packing,
#'   use a dedicated package such as \CRANpkg{beeswarm}.
#' @references
#' Sidiropoulos, N., Sohi, S. H., Pedersen, T. L., Porse, B. T., Winther, O.,
#' Rapin, N., and Bagger, F. O. (2018). \cite{SinaPlot: An Enhanced Chart for
#' Simple and Truthful Representation of Single Observations Over Multiple
#' Classes}. Journal of Computational and Graphical Statistics, 27(3), 673-676.
#' Available: https://doi.org/10.1080/10618600.2017.1366914
#' @seealso [type_boxplot], [type_density] and [type_ridge] for the other ways
#'   of displaying a distribution by group, and [type_jitter] for displacing
#'   points without reference to a density.
#'   
#' @examples
#' # "violin" type convenience string
#' tinyplot(weight ~ feed, data = chickwts, type = "violin")
#' 
#' # to match the defaults of `ggplot2::geom_violin()`, use `trim = TRUE` and
#' # `joint.bw = FALSE`
#' tinyplot(
#'   weight ~ feed, data = chickwts,
#'   # type = type_violin(trim = TRUE, joint.bw = FALSE) # same but see final ex.
#'   type = "violin", trim = TRUE, joint.bw = FALSE
#' )
#' 
#' # For flipped violin plots, it's usually better to use a dynamic theme to
#' # accommodate (horizontal) y-axis labels
#' tinyplot(
#'   weight ~ feed, data = chickwts, type = "violin", flip = TRUE,
#'   theme = "dynamic" # or "clean(2)", "classic", "minimal", etc.
#' )
#' 
#' # you can group by the x var to add colour (here with the original orientation)
#' tinyplot(weight ~ feed | feed, data = chickwts, type = "violin", legend = FALSE)
#' 
#' # dodged grouped violin plot example (different dataset)
#' tinyplot(len ~ dose | supp, data = ToothGrowth, type = "violin")
#' 
#' # the "sina" type shows the observations themselves, rather than a smooth
#' # outline drawn around them
#' tinyplot(weight ~ feed, data = chickwts, type = "sina")
#' 
#' # layering a sina on top of a violin lines up exactly, since the points are
#' # displaced by the violin's own half-width
#' tinyplot(weight ~ feed, data = chickwts, type = "violin")
#' tinyplot_add(type = "sina", pch = 16, col = "black")
#' 
#' # unlike `type_violin()`, `type_sina()` supports a continuous `by` variable;
#' # it colours the points rather than splitting them into groups
#' tinyplot(
#'   Sepal.Length ~ Species | Petal.Width, data = iris,
#'   type = "sina", pch = 16
#' )
#' 
#' # note: above we relied on `...` argument passing alongside the type
#' # convenience strings. But this won't work for `width`, since it will
#' # clash with the top-level `tinyplot(..., width = <width>)` arg. To ensure
#' # correct arg passing, it's safer to use the functional type.
#' tinyplot(
#'   len ~ dose | supp, data = ToothGrowth,
#'   type = type_violin(width = 0.75)
#' )
#' 
#' @importFrom stats density weighted.mean
#' @importFrom stats bw.SJ bw.bcv bw.nrd bw.nrd0 bw.ucv 
#' @order 1
#' @export
type_violin = function(
        bw = "nrd0",
        joint.bw =  c("mean", "full", "none"),
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
        n = 512,
        # more args from density here?
        trim = FALSE,
        width = 0.9,
        lighten = TRUE,
        singletons = c("warn", "drop", "none")
    ) {
    kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
    singletons = match.arg(singletons, c("warn", "drop", "none"))
    if (is.logical(joint.bw)) {
        joint.bw = ifelse(joint.bw, "mean", "none")
    }
    joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
    out = list(
        data = data_violin(bw = bw, adjust = adjust, kernel = kernel, n = n,
                            joint.bw = joint.bw, trim = trim, width = width,
                            lighten = lighten, singletons = singletons),
        # draw = NULL,
        # name = "polygon"
        draw = draw_polygon(density = NULL),
        name = "violin"
    )
    class(out) = "tinyplot_type"
    return(out)
}

data_violin = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                        joint.bw = "none", trim = FALSE, width = 0.9,
                        lighten = TRUE, singletons = "warn") {
    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "by", "null_palette", "facet", "ylab", "col", "bg", "log", "null_by", "null_facet"))
        settings[["lighten"]] = lighten

        specials = dist_specials(datapoints, null_by, null_facet)

        # FIXME (once we add support for gradient fill to draw_polygon)
        if (specials[["y_by"]]) {
            warning("\n`y` == `by` is not currently supported for `type_violin`. We hope to support this in a future release, but for now `y` grouping will be turned off automatically.\n")
            by = NULL
            datapoints$by = ""
            null_by = TRUE
            specials = dist_specials(datapoints, null_by, null_facet)
        }

        if (length(unique(datapoints[["by"]])) == 1 && null_palette) {
            # With a theme palette active, leave bg = NULL so the fill tracks
            # the resolved border colour (see by_bg). Otherwise use neutral grey.
            if (is.null(bg) && is.null(get_tpar("palette.qualitative", default = NULL))) bg = "lightgray"
        } else if (is.null(bg)) {
            bg = "by"
        }

        prep = dist_prep(
            datapoints, null_by = null_by, null_facet = null_facet,
            specials = specials, bw = bw, joint.bw = joint.bw, width = width,
            singletons = singletons
        )
        cells = prep[["cells"]]
        dens_bw = prep[["dens_bw"]]
        xwidth = prep[["xwidth"]]
        group_offsets = prep[["group_offsets"]]
        offsets_axis = prep[["offsets_axis"]]
        xlabs = prep[["xlabs"]]

        datapoints = lapply(cells, function(dat) {
            if (trim) {
                yrng = range(dat$y)
                dens = density(dat$y, bw = dens_bw, adjust = adjust, kernel = kernel, n = n, from = yrng[1], to = yrng[2])
            } else {
                dens = density(dat$y, bw = dens_bw, adjust = adjust, kernel = kernel, n = n)
            }

            x = dens$y
            y = dens$x

            if (log %in% c("y", "xy")) {
                if (x[1] <= 0) {
                    warning("\nNon-positive density values have been trimmed as part of the logarthmic transformation.\n")
                    xidx = x > 0
                    x = x[xidx]
                    y = y[xidx]
                }
            }

            # mirror the density about the category position
            x = c(x, rev(-x))
            y = c(y, rev(y))

            xcat = dat$x[1]
            x = rescale_num(x, to = c(0, xwidth)) + xcat - xwidth / 2
            if (prep[["dodged"]]) x = x + group_offsets[dat$by[1]]

            x = c(x, NA)
            y = c(y, NA)

            out = data.frame(
                by = dat$by[1], # already split
                facet = dat$facet[1], # already split
                y = y,
                x = x
            )
            # `x` traces the density outline, so it no longer sits on its own
            # tick; carry the category position. See cat_axis_codes()
            if (facet_drop_levels_on(settings[["facet.args"]])) out[[".xcat"]] = xcat
            return(out)
        })
        datapoints = do.call(rbind, datapoints)
        datapoints = datapoints[1:(nrow(datapoints)-1), ]

        by = if (length(unique(datapoints$by)) == 1) by else datapoints$by
        facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet

        # legend customizations
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

        env2env(environment(), settings, c(
            "datapoints",
            "by",
            "facet",
            "ylab",
            "xlabs",
            "col",
            "bg",
            "group_offsets",
            "offsets_axis"
        ))
    }
    return(fun)
}


## Shared setup for the categorical distribution types, below.
##
## `type_violin()` and `type_sina()` both plot a categorical `x` against a
## numeric `y`, estimate one density per (x, by, facet) cell, and lay the
## result out at integer x positions with the `by` groups dodged around each
## tick. Everything up to the density is common; only what each type does
## *with* it differs -- violin traces the outline, sina scatters the
## observations inside it. That common part lives here, since `type_violin()`
## is also the home of the help page the two types share. `type_sina()` lives
## in type_sina.R and calls into these.


## Detect the "special" cases where `by` duplicates another aesthetic. Callers
## need these before any of the setup below, since e.g. `type_violin()` has to
## resolve a `y`-valued `by` (which it cannot render) before the split.
dist_specials = function(datapoints, null_by, null_facet) {
    out = list(x_by = FALSE, y_by = FALSE, facet_by = FALSE)
    if (isTRUE(null_by)) return(out)
    out[["x_by"]] = identical(datapoints[["x"]], datapoints[["by"]])
    out[["y_by"]] = identical(datapoints[["y"]], datapoints[["by"]])
    if (isFALSE(null_facet)) {
        out[["facet_by"]] = identical(datapoints[["facet"]], datapoints[["by"]])
    }
    return(out)
}


## Coerce `x` to consecutive integer positions, split the data into density
## cells, pick a bandwidth, and work out the dodge offsets.
##
## `gradient` says the caller can render a continuous `by` (sina colours its
## points by it). Splitting on such a `by` would put nearly every observation
## in a cell of its own, so those cells are keyed on (x, facet) alone and the
## groups are left undodged.
##
## Returns the pieces the caller needs; type-specific concerns (fills, legend
## keys) stay with the caller.
dist_prep = function(
        datapoints,
        null_by,
        null_facet,
        specials,
        bw = "nrd0",
        joint.bw = "none",
        width = 0.9,
        singletons = "warn",
        gradient = FALSE
    ) {
    x_by = specials[["x_by"]]
    facet_by = specials[["facet_by"]]

    by_continuous = isTRUE(gradient) && isFALSE(null_by) &&
        inherits(datapoints[["by"]], c("numeric", "integer"))

    ngrps = if (null_by || by_continuous) 1L else length(unique(datapoints[["by"]]))
    nfacets = if (null_facet) 1L else length(unique(datapoints[["facet"]]))

    ## Convert x to consecutive integer positions, keeping the labels for the axis
    datapoints[["x"]] = as.factor(datapoints[["x"]])
    if (x_by) datapoints[["by"]] = datapoints[["x"]]
    xlvls = levels(datapoints[["x"]])
    xlabs = seq_along(xlvls)
    names(xlabs) = xlvls
    datapoints[["x"]] = as.integer(datapoints[["x"]])

    if (null_by && null_facet) {
        xord = order(datapoints[["x"]])
    } else if (null_facet) {
        xord = order(datapoints[["by"]], datapoints[["x"]])
    } else if (null_by) {
        xord = order(datapoints[["facet"]], datapoints[["x"]])
    } else {
        xord = order(datapoints[["by"]], datapoints[["facet"]], datapoints[["x"]])
    }
    datapoints = datapoints[xord, ]

    ## A continuous `by` is a colour scale rather than a grouping, so it must
    ## not key the split; every cell would hold a single observation.
    if (by_continuous) {
        cells = split(datapoints, list(datapoints[["x"]], datapoints[["facet"]]))
    } else {
        cells = split(
            datapoints,
            list(datapoints[["x"]], datapoints[["by"]], datapoints[["facet"]])
        )
    }
    ## "keep" leaves the 1-row cells in place for the caller to draw as-is; it
    ## still has to shed the 0-row cells that split() invents, which is what
    ## "none" does.
    cells = drop_singletons(cells, if (singletons == "keep") "none" else singletons)

    ## Bandwidth rules need at least 2 observations, so a singleton cell kept
    ## under "keep" must not feed into them (the caller skips its density
    ## anyway). Under "none" it deliberately does, since that mode's contract
    ## is to run the checks the user opted out of and let them fail.
    smoothable = if (singletons == "keep") {
        cells[vapply(cells, nrow, integer(1)) > 1L]
    } else {
        cells
    }
    if (joint.bw == "none" || is.numeric(bw)) {
        dens_bw = bw
    } else if (joint.bw == "mean") {
        # Use weighted mean of subgroup bandwidths
        bws = sapply(smoothable, function(dat) bw_fun(kernel = bw, dat[["y"]]))
        ws = sapply(smoothable, nrow)
        dens_bw = weighted.mean(bws, ws)
    } else {
        dens_bw = bw_fun(kernel = bw, unlist(sapply(smoothable, `[[`, "y")))
    }

    ## Dodge: groups share a tick, so each gets a narrower slot beside it
    dodged = ngrps > 1 && isFALSE(x_by) && isFALSE(facet_by)
    if (dodged) {
        xwidth = width / ngrps - 0.01
        group_offsets = seq(
            -((width - xwidth) / 2),
            ((width - xwidth) / 2),
            length.out = ngrps
        )
    } else {
        xwidth = width
        group_offsets = rep(0, max(ngrps, 1))
    }

    list(
        cells = cells,
        xlabs = xlabs,
        dens_bw = dens_bw,
        group_offsets = group_offsets,
        offsets_axis = "x",
        ngrps = ngrps,
        nfacets = nfacets,
        dodged = dodged,
        xwidth = xwidth,
        by_continuous = by_continuous
    )
}
