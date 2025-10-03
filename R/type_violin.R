#' Violin plot type
#' 
#' @md
#' @description Type function for violin plots, which are an alternative to box
#'   plots for visualizing continuous distributions (by group) in the form of
#'   mirrored densities.
#' @inheritParams type_density
#' @param  trim logical indicating whether the violins should be trimmed to the
#'   range of the data. Default is `FALSE`.
#' @param width numeric (ideally in the range `[0, 1]`, although this isn't
#'   enforced) giving the normalized width of the individual violins.
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
#' tinyplot(weight ~ feed, data = chickwts, type = "violin", flip = TRUE)
#' 
#' # you can group by the x var to add colour (here with the original orientation)
#' tinyplot(weight ~ feed | feed, data = chickwts, type = "violin", legend = FALSE)
#' 
#' # dodged grouped violin plot example (different dataset)
#' tinyplot(len ~ dose | supp, data = ToothGrowth, type = "violin", fill = 0.2)
#' 
#' # note: above we relied on `...` argument passing alongside the "violin"
#' # type convenience string. But this won't work for `width`, since it will
#' # clash with the top-level `tinyplot(..., width = <width>)` arg. To ensure
#' # correct arg passing, it's safer to use the formal `type_violin()` option.
#' tinyplot(len ~ dose | supp, data = ToothGrowth, fill = 0.2,
#'     type = type_violin(width = 0.8))
#' 
#' # reset theme
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
        width = 0.9
    ) {
    kernel = match.arg(kernel, c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
    if (is.logical(joint.bw)) {
        joint.bw = ifelse(joint.bw, "mean", "none")
    }
    joint.bw = match.arg(joint.bw, c("mean", "full", "none"))
    out = list(
        data = data_violin(bw = bw, adjust = adjust, kernel = kernel, n = n,
                            joint.bw = joint.bw, trim = trim, width = width),
        # draw = NULL,
        # name = "polygon"
        draw = draw_polygon(density = NULL),
        name = "violin"
    )
    class(out) = "tinyplot_type"
    return(out)
}

data_violin = function(bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512,
                        joint.bw = "none", trim = FALSE, width = 0.9) {
    fun = function(settings, ...) {
        list2env(settings[c("datapoints", "by", "raw_input", "facet", "ylab", "col", "bg", "log", "null_by", "null_facet")], 
            environment())

        
        # Handle ordering based on by and facet variables
        ngrps = if (null_by) 1 else length(unique(datapoints$by))
        nfacets = if (null_facet) 1 else length(unique(datapoints$facet))
        
        #  catch for special cases
        x_by = y_by = facet_by = FALSE
        if (!null_by) {
            x_by = identical(datapoints$x, datapoints$by)
            y_by = identical(datapoints$y, datapoints$by)
            if (!null_facet) facet_by = identical(datapoints$facet, datapoints$by)
        }
        
        # FIXME (once we add support for gradient fill to draw_polygon)
        if (y_by) {
            warning("\n`y` == `by` is not currently supported for `type_violin`. We hope to support this in a future release, but for now `y` grouping will be turned off automatically.\n")
            by = NULL
            datapoints$by = ""
            ngrps = 1
            null_by = TRUE
        }
        
        # Convert x to factor if it's not already
        datapoints$x = as.factor(datapoints$x)
        if (x_by) datapoints$by = datapoints$x

        # Handle factor levels and maintain order
        xlvls = levels(datapoints$x)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        # xlabs = levels(datapoints$x)
        datapoints$x = as.integer(datapoints$x)

        if (null_by && null_facet) {
            xord = order(datapoints$x)
        } else if (null_facet) {
            xord = order(datapoints$by, datapoints$x)
        } else if (null_by) {
            xord = order(datapoints$facet, datapoints$x)
        } else {
            xord = order(datapoints$by, datapoints$facet, datapoints$x)
        }

        if (length(unique(datapoints[["by"]])) == 1 && is.null(raw_input$palette)) {
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
            if (trim) {
                yrng = range(dat$y)
                dens = density(dat$y, bw = dens_bw, kernel = kernel, n = n, from = yrng[1], to = yrng[2])
            } else {
                dens = density(dat$y, bw = dens_bw, kernel = kernel, n = n)
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
            
            x = c(x, rev(-x))
            y = c(y, rev(y))
            
            xwidth = xwidth_orig = width
            # dodge groups (if any)
            if ((ngrps > 1)  && isFALSE(x_by) && isFALSE(facet_by)) {
                xwidth = xwidth_orig / ngrps - 0.01
                x = rescale_num(x, to = c(0, xwidth))
                x = x + as.numeric(sub("^([0-9]+)\\..*", "\\1", names(datapoints)[d])) - xwidth/2
                x = x + seq(-((xwidth_orig - xwidth) / 2), ((xwidth_orig - xwidth) / 2), length.out = ngrps)[dat$by[1]]
            } else if (nfacets > 1) {
                x = rescale_num(x, to = c(0, xwidth))
                x = x + as.numeric(sub("^([0-9]+)\\..*", "\\1", names(datapoints)[d])) - xwidth/2
            } else {
                x = rescale_num(x, to = c(0, xwidth))
                x = x + d - xwidth/2
            }
            
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

