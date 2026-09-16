#' @rdname type_violin
#' @param method character string giving how `type_sina()` spreads points
#'   across the available width at each `y` value. `"quasirandom"` (the
#'   default) walks a low-discrepancy sequence, which fills the width more
#'   evenly than chance does and---unlike [`type_jitter`]---is deterministic,
#'   so repeated calls give the same plot without setting a seed. `"random"`
#'   draws the displacements uniformly at random instead.
#' @importFrom stats approx density runif weighted.mean
#' @order 2
#' @export
type_sina = function(
        bw = "nrd0",
        joint.bw = c("mean", "full", "none"),
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
        n = 512,
        trim = FALSE,
        width = 0.9,
        method = c("quasirandom", "random"),
        singletons = c("keep", "warn", "drop")
    ) {
    kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
    method = match.arg(method, c("quasirandom", "random"))
    singletons = match.arg(singletons, c("keep", "warn", "drop"))
    if (is.logical(joint.bw)) {
        joint.bw = ifelse(joint.bw, "mean", "none")
    }
    joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
    out = list(
        data = data_sina(bw = bw, adjust = adjust, kernel = kernel, n = n,
                         joint.bw = joint.bw, trim = trim, width = width,
                         method = method, singletons = singletons),
        draw = draw_points(),
        # points, as far as the rest of the package is concerned: this gates
        # `pch`, `cex` and the bubble/legend handling. See by_pch().
        name = "p"
    )
    class(out) = "tinyplot_type"
    return(out)
}


## The first `n` van der Corput values, mapped onto [-1, 1] and centred: a
## deterministic stand-in for runif(-1, 1) that spreads successive values evenly
## rather than independently, so points fill the available width instead of
## clumping and leaving gaps.
##
## The raw sequence only balances about the midpoint at n = 2^k - 1; at every
## other n it leans left (n = 2 gives c(0, -0.5), so neither point sits right of
## the tick). Centring fixes the lean, and can overshoot the envelope by ~0.1%
## in the process, so clamp it back -- points escaping the density bounds would
## defeat the whole point of the type.
centred_van_der_corput = function(n) {
    u = 2 * van_der_corput(n) - 1
    u = u - mean(u)
    pmax(pmin(u, 1), -1)
}


van_der_corput = function(n, base = 2) {
    vapply(
        seq_len(n),
        function(i) {
            out = 0
            f = 1 / base
            while (i > 0) {
                out = out + f * (i %% base)
                i = i %/% base
                f = f / base
            }
            out
        },
        numeric(1)
    )
}


data_sina = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                     joint.bw = "none", trim = FALSE, width = 0.9,
                     method = "quasirandom", singletons = "keep") {
    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "by", "facet", "ylab", "col", "bg", "null_by", "null_facet"))

        specials = dist_specials(datapoints, null_by, null_facet)

        prep = dist_prep(
            datapoints, null_by = null_by, null_facet = null_facet,
            specials = specials, bw = bw, joint.bw = joint.bw, width = width,
            singletons = singletons, gradient = TRUE
        )
        cells = prep[["cells"]]
        dens_bw = prep[["dens_bw"]]
        xwidth = prep[["xwidth"]]
        group_offsets = prep[["group_offsets"]]
        grp_levels = prep[["grp_levels"]]
        offsets_axis = prep[["offsets_axis"]]
        xlabs = prep[["xlabs"]]

        datapoints = lapply(cells, function(dat) {
            nobs = nrow(dat)
            xcat = dat[["x"]][1]
            dodge = if (prep[["dodged"]]) {
                group_offsets[match(dat[["by"]][1], grp_levels)]
            } else {
                0
            }

            # a lone observation has no density to be displaced by, so it just
            # sits on its group's tick
            if (nobs < 2L) {
                dat[["x"]] = xcat + dodge
                if (facet_drop_levels_on(settings[["facet.args"]])) dat[[".xcat"]] = xcat
                return(dat)
            }

            if (trim) {
                yrng = range(dat[["y"]])
                dens = density(dat[["y"]], bw = dens_bw, adjust = adjust,
                               kernel = kernel, n = n, from = yrng[1], to = yrng[2])
            } else {
                dens = density(dat[["y"]], bw = dens_bw, adjust = adjust,
                               kernel = kernel, n = n)
            }

            # the violin's half-width at each observation, normalized to [0, 1]
            halfwidth = approx(dens[["x"]], dens[["y"]], xout = dat[["y"]], rule = 2)[["y"]]
            halfwidth = halfwidth / max(dens[["y"]])

            if (method == "random") {
                u = runif(nobs, -1, 1)
            } else {
                # walk the sequence in y order, so that neighbouring points
                # (the ones at risk of overlapping) land far apart
                u = numeric(nobs)
                u[order(dat[["y"]])] = centred_van_der_corput(nobs)
            }

            dat[["x"]] = xcat + u * halfwidth * xwidth / 2 + dodge
            # the displacement moves points off their own tick; carry the
            # category position. See cat_axis_codes()
            if (facet_drop_levels_on(settings[["facet.args"]])) dat[[".xcat"]] = xcat
            return(dat)
        })
        datapoints = do.call(rbind, datapoints)

        by = if (length(unique(datapoints[["by"]])) == 1) by else datapoints[["by"]]
        facet = if (length(unique(datapoints[["facet"]])) == 1) facet else datapoints[["facet"]]

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
