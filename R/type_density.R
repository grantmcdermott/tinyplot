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
#'   bandwidth should be computed from the joint data distribution. The default
#'   value of `"none"` means that bandwidths will be computed independently for
#'   each data subgroup. Choosing `"full"` will result in a joint bandwidth
#'   computed from the full distribution. Similarly, `"owm"` will compute the
#'   joint bandwidth as the observation-weighted mean of the individual subgroup
#'   bandwidths. Note that the `joint.bw` argument is only relevant for grouped
#'   or faceted density plots.
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
#'   type = type_density(bw = "SJ"),
#'   main = "Bandwidth computed using method of Sheather & Jones (1991)"
#' )
#' 
#' # The default for grouped density plots is to compute bandwidths
#' # independently for each subgroup. To override, specify the type of joint
#' # bandwidth computation
#' tinyplot(~Sepal.Length | Species, data = iris, type = "density") # "none" (default)
#' tinyplot_add(type = type_density(joint.bw = "full"), lty = 2)    # full dataset
#' tinyplot_add(type = type_density(joint.bw = "owm"), lty = 3)     # obs-weighted mean
#' legend("topright", c("None", "Full", "OWM"), lty = 1:3, title = "Joint BW")
#' 
#' @importFrom stats density
#' @export
type_density = function(
        bw = "nrd0",
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
        n = 512,
        # more args from density here?
        joint.bw =  c("none", "full", "owm"),
        alpha = NULL
    ) {
    kernel = match.arg(kernel)
    joint.bw = match.arg(joint.bw)
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
            } else if (joint.bw == "owm") {
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

