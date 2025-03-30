#' Violin plot type
#' 
#' @md
#' @description Type function for violin plots, which are an alternative to box
#'   plots for visualizing continuous distributions (by group) in the form of
#'   mirrored densities.
#' @inheritParams type_density
#' @param  trim logical indicating whether the violins should be trimmed to the
#'   range of the data. Default is `FALSE`.
#' @inherit stats::density details
#' @details See [`type_density`] for more details and considerations related to
#'   bandwidth selection and kernel types.
#'   
#' @examples
#' # "violin" type convenience string
#' tinyplot(count ~ spray, data = InsectSprays, type = "violin")
#' 
#' # aside: to match the defaults of `ggplot2::geom_violin()`, use `trim = TRUE`
#' # and `joint.bw = FALSE`
#' tinyplot(count ~ spray, data = InsectSprays, type = "violin",
#'     trim = TRUE, joint.bw = FALSE)
#' 
#' # use flip = TRUE to reorient the axes
#' tinyplot(count ~ spray, data = InsectSprays, type = "violin", flip = TRUE)
#' 
#' # for flipped plots with long group labels, it's better to use a theme for
#' # dynamic plot resizing
#' tinytheme("clean")
#' tinyplot(weight ~ feed, chickwts, type = "violin", flip = TRUE)
#' tinytheme()
#' 
#' @importFrom stats density weighted.mean
#' @importFrom stats bw.SJ bw.bcv bw.nrd bw.nrd0 bw.ucv 
#' @export
type_violin = function(
        bw = "nrd0",
        joint.bw =  c("mean", "full", "none"),
        adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
        n = 512,
        # more args from density here?
        trim = FALSE,
        alpha = NULL
    ) {
    kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
    if (is.logical(joint.bw)) {
        joint.bw = ifelse(joint.bw, "mean", "none")
    }
    joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
    out = list(
        data = data_violin(bw = bw, adjust = adjust, kernel = kernel, n = n,
                            joint.bw = joint.bw, alpha = alpha, trim = trim),
        # draw = NULL,
        # name = "polygon"
        draw = draw_polygon(density = NULL),
        name = "violin"
    )
    class(out) = "tinyplot_type"
    return(out)
}

data_violin = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                        joint.bw = "none", alpha = NULL, trim = FALSE) {
    fun = function(datapoints,  by, facet, ylab, col, bg, palette, ...) {
        
        # Convert x to factor if it's not already
        datapoints$x = as.factor(datapoints$x)

        # Handle factor levels and maintain order
        xlvls = levels(datapoints$x)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        # xlabs = levels(datapoints$x)
        datapoints$x = as.integer(datapoints$x)
        
        # Handle ordering based on by and facet variables
        null_by = length(unique(datapoints$by)) == 1
        null_facet = length(unique(datapoints$facet)) == 1

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        if (length(unique(datapoints[["by"]])) == 1 && is.null(palette)) {
            if (is.null(col)) col = par("fg")
            if (is.null(bg)) bg = "lightgray"
        } else if (is.null(bg)) {
            bg = "by"
        }

        # Reorder x, y, ymin, and ymax based on the order determined
        datapoints = datapoints[xord,]

        
        datapoints = split(datapoints, list(datapoints$x, datapoints$by, datapoints$facet))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        
        if (joint.bw == "none" || is.numeric(bw)) {
            dens_bw = bw
        } else {
            if (joint.bw == "mean") {
                # Use weighted mean of subgroup bandwidths
                bws = sapply(datapoints, function(dat) bw_fun(kernel = bw, dat$y))
                ws = sapply(datapoints, nrow)
                dens_bw = weighted.mean(bws, ws)
            } else if (joint.bw == "full") {
                dens_bw = bw_fun(kernel = bw, unlist(sapply(datapoints, `[[`, "x")))
            }
        }
        
        datapoints = lapply(seq_along(datapoints), function(d) {
            dat = datapoints[[d]]
            dens = density(dat$y, bw = dens_bw, kernel = kernel, n = n)
            
            x = dens$y
            y = dens$x
            
            if (isTRUE(trim)) {
                yrng = range(dat$y)
                yridx = y >= yrng[1] & y <= yrng[2] 
                x = x[yridx]
                y = y[yridx]
            }
    
            x = c(x, rev(-x))
            y = c(y, rev(y))
            
            x = (x-min(x)) / max(x-min(x) * 1.1)
            x = x + d - .5
            
            x = c(x, NA)
            y = c(y, NA)
            
            out = data.frame(
                by = dat$by[1], # already split
                facet = dat$facet[1], # already split
                y = y,
                x = x
            )
            return(out)
        })
        datapoints = do.call(rbind, datapoints)
        datapoints = datapoints[1:(nrow(datapoints)-1), ]
        # datapoints$ymax = datapoints$y
        # datapoints$ymin = rep.int(0, nrow(datapoints))
        
        out = list(
            datapoints = datapoints,
            by = if (length(unique(datapoints$by)) == 1) by else datapoints$by, 
            facet = if (length(unique(datapoints$facet)) == 1) facet else datapoints$facet,
            ylab = ylab,
            xlabs = xlabs,
            col = col,
            bg = bg
        )
        return(out)
    }
    return(fun)
}

