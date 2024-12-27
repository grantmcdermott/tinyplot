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
#' @param indiv.bw Should the smoothing bandwidth be computed individually
#'   (independently) for each data subgroup? Defaults to `FALSE`, in which case
#'   a common bandwidth is obtained from the full dataset and then applied
#'   across all subgroups. Only relevant for grouped density plots, i.e. those
#'   called with a valid `by` (`|`)  argument.
#' @inherit stats::density details
#' @section Titles: This tinyplot method for density plots differs from the base
#'   \code{\link[stats]{plot.density}} function in its treatment of titles. The
#'   x-axis title displays only the variable name, omitting details about the
#'   number of observations and smoothing bandwidth. Additionally, the main
#'   title is left blank by default for a cleaner appearance.
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
#'   type = type_density(kernel = "cosine"), main = "cosine kernel"
#' )
#' 
#' # The default for grouped density plots is to use a common bandwidth,
#' # computed on the full dataset. Invoke the `indiv.bw` argument to override.
#' tinyplot(
#'   ~Sepal.Length | Species, data = iris,
#'   type = type_density(indiv.bw = TRUE), main = "Individual bandwidths"
#' )
#' 
#' @importFrom stats density
#' @export
type_density = function(
        bw = "nrd0",
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        n = 512,
        # more args from density here?
        indiv.bw = FALSE,
        alpha = NULL
    ) {
    kernel = match.arg(kernel)
    out = list(
        data = data_density(bw = bw, adjust = adjust, kernel = kernel, n = n,
                            indiv.bw = indiv.bw, alpha = alpha),
        draw = NULL,
        name = "density"
    )
    class(out) = "tinyplot_type"
    return(out)
}

data_density = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                        indiv.bw = FALSE, alpha = NULL) {
    fun = function(by, facet, ylab, col, bg, ribbon.alpha, datapoints,  ...) {
        
        ribbon.alpha = if (is.null(alpha)) .tpar[["ribbon.alpha"]] else (alpha)
        
        if (is.null(ylab)) ylab = "Density"
        # if (is.null(by) && is.null(palette)) {
        #     if (is.null(col)) col = par("fg")
        #     if (is.null(bg)) bg = "lightgray"
        # } else {
        #     if (is.null(bg)) bg = ribbon.alpha
        # }
        # if (!is.null(bg) && bg=="by") bg = ribbon.alpha
        
        assert_logical(indiv.bw)
        if (indiv.bw) {
            dens_bw = bw
        } else {
            dens_full = density(datapoints$x, bw = bw, adjust = adjust, kernel = kernel, n = n)
            dens_bw = dens_full$bw
        }
        
        datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        
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
            xlim = range(datapoints$x),
            ylim = range(datapoints$y),
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

