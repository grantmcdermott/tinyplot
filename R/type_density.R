#' Density plot type
#' 
#' @md
#' @description Type function for density plots.
#' @inheritParams stats::density
#' @param kernel a character string giving the smoothing kernel to be used. This
#'   must partially match one of `"gaussian"`, `"rectangular"`, `"triangular"`,
#'   `"epanechnikov"`, `"biweight"`, `"cosine"` or `"optcosine"`, with default
#'   `"gaussian"`, and may be abbreviated to a unique prefix (single letter).
#'
#'   `"cosine"` is smoother than `"optcosine"`, which is the usual 'cosine'
#'   kernel in the literature and almost MSE-efficient. However, `"cosine"` is
#'   the version used by S.
#' @inheritParams type_ribbon
#' @param joint.bw character string indicating whether (and how) the smoothing
#'   bandwidth should be computed from the joint data distribution, in case
#'   there are multiple subgroups (from `by` or `facet`). The default of
#'   `"mean"` will compute the joint bandwidth as the mean of the individual
#'   subgroup bandwidths (weighted by their number of observations). Choosing `"full"` will result in a
#'   joint bandwidth computed from the full distribution (merging all subgroups).
#'   For `"none"` the individual bandwidth will be computed independently for
#'   each subgroup. When no grouped or faceted densities are visualized, the
#'   `joint.bw` argument is irrelevant.
#' @inherit stats::density details
#' @section Titles: This tinyplot method for density plots differs from the base
#'   \code{\link[stats]{plot.density}} function in its treatment of titles. The
#'   x-axis title displays only the variable name, omitting details about the
#'   number of observations and smoothing bandwidth. Additionally, the main
#'   title is left blank by default for a cleaner appearance.
#' @section Bandwidth selection: When not only a single but multiple densities
#'   are drawn simultaneously (e.g., for subgroups with respect to `by` or `facet`)
#'   then it matters whether the same joint smoothing bandwidth is used for all
#'   subgroups or whether each group has its own separate bandwidth. The latter is
#'   able to adapt more flexibly when the distributions in all groups differ
#'   substantially with respect to location and/or scale. However, it may make the
#'   multiple densities harder to compare visually because it may highlight small
#'   random variations too much. Hence, it is often useful to employ the same joint
#'   bandwidth across all subgroups. The following strategies are available via the
#'   `joint.bw` argument.
#' 
#'   The default `joint.bw = "mean"` first computes the individual bandwidths for
#'   each group but then computes their mean, weighted by the number of observations
#'   in each group. This will work well when all groups have similar amounts of
#'   scatter (similar variances), even when they have potentially rather different
#'   locations. The weighted averaging stabilizes potential fluctuations in the
#'   individual bandwidths, especially when some subgroups are rather small.
#' 
#'   Alternatively, `joint.bw = "full"` can be used to compute the joint bandwidth
#'   from the full joint distribution (merging all groups). This will yield an
#'   even more robust bandwidth, especially when the groups overlap substantially
#'   (i.e., have similar locations and scales). However, it may lead to too large
#'   bandwidths and thus too much smoothing, espeically when the locations of the
#'   groups differ substantially.
#' 
#'   Finally, `joint.bw = "none"` disables the joint bandwidth so that each group
#'   just employs its individual bandwidth. This is often the best choice if the
#'   amounts of scatter differ substantially between the groups, thus necessitating
#'   different amounts of smoothing.
#' @examples
#' # "density" type convenience string
#' tinyplot(~Sepal.Length, data = iris, type = "density")
#' 
#' # grouped density example
#' tinyplot(~Sepal.Length | Species, data = iris, type = "density")
#' 
#' # use `bg = "by"` (or, equivalent `fill = "by"`) to get filled densities
#' tinyplot(~Sepal.Length | Species, data = iris, type = "density", fill = "by")
#' 
#' # use `type_density()` to pass extra arguments for customization
#' tinyplot(
#'   ~Sepal.Length | Species, data = iris,
#'   type = type_density(bw = "SJ"),
#'   main = "Bandwidth computed using method of Sheather & Jones (1991)"
#' )
#' 
#' # The default for grouped density plots is to use the mean of the
#' # individual subgroup bandwidths (weighted by group size) as the
#' # joint bandwidth. Alternatively, the bandwidth from the "full"
#' # data or separate individual bandwidths ("none") can be used.
#' tinyplot(~Sepal.Length | Species, data = iris, type = "density") # mean (default)
#' tinyplot_add(joint.bw = "full", lty = 2)                         # full data
#' tinyplot_add(joint.bw = "none", lty = 3)                         # none (individual)
#' legend("topright", c("Mean", "Full", "None"), lty = 1:3, bty = "n", title = "Joint BW")
#' 
#' @importFrom stats density weighted.mean
#' @importFrom stats bw.SJ bw.bcv bw.nrd bw.nrd0 bw.ucv 
#' @export
type_density = function(
        bw = "nrd0",
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
        n = 512,
        # more args from density here?
        joint.bw =  c("mean", "full", "none"),
        alpha = NULL
    ) {
    kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
    joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
    out = list(
        data = data_density(bw = bw, adjust = adjust, kernel = kernel, n = n,
                            joint.bw = joint.bw, alpha = alpha),
        draw = NULL,
        name = "density"
    )
    class(out) = "tinyplot_type"
    return(out)
}

data_density = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                        joint.bw = "none", alpha = NULL) {
    fun = function(by, facet, ylab, col, bg, ribbon.alpha, datapoints,  ...) {
        
        ribbon.alpha = if (is.null(alpha)) .tpar[["ribbon.alpha"]] else (alpha)
        
        if (is.null(ylab)) ylab = "Density"
        
        datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        
        if (joint.bw == "none" || is.numeric(bw)) {
            dens_bw = bw
        } else {
            # Use weighted mean of subgroup bandwidths
            # Define a function that uses switch() to call the appropriate bandwidth function
            bw_fun = function(kernel, data) {
                kernel = tolower(kernel)
                switch(
                    kernel,
                    nrd0 = bw.nrd0(data),
                    nrd  = bw.nrd(data),
                    ucv  = bw.ucv(data),
                    bcv  = bw.bcv(data),
                    sj   = bw.SJ(data),
                    stop("Invalid `bw` string. Choose from 'nrd0', 'nrd', 'ucv', 'bcv', or 'SJ'.")
                )
            }
            if (joint.bw == "full") {
                dens_bw = bw_fun(kernel = bw, unlist(sapply(datapoints, `[[`, "x")))
            } else if (joint.bw == "mean") {
                bws = sapply(datapoints, function(dat) bw_fun(kernel = bw, dat$x))
                ws = sapply(datapoints, nrow)
                dens_bw = weighted.mean(bws, ws)
            }
        }
        
        datapoints = lapply(datapoints, function(dat) {
            d = density(dat$x, bw = dens_bw, kernel = kernel, n = n)
            out = data.frame(
                by = dat$by[1], # already split
                facet = dat$facet[1], # already split
                y = d$y,
                x = d$x
            )
            return(out)
        })
        datapoints = do.call(rbind, datapoints)
        datapoints$ymax = datapoints$y
        datapoints$ymin = rep.int(0, nrow(datapoints))
        
        # flags for legend and fill
        dtype = if (!is.null(bg)) "ribbon" else "l"
        dwas_area_type = !is.null(bg)
        
        out = list(
            ylab = ylab,
            type = dtype,
            was_area_type = dwas_area_type,
            ribbon.alpha = ribbon.alpha,
            datapoints = datapoints,
            by = if (length(unique(datapoints$by)) == 1) by else datapoints$by, 
            facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet
        )
        return(out)
    }
    return(fun)
}

