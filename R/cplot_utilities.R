# function to check factors
check_factors <- function(object, data, xvar, dx) {
    # check factors
    classes <- attributes(terms(object))[["dataClasses"]][-1]
    classes <- classes[names(classes) != "(weights)"]
    classes[classes == "character"] <- "factor"
    
    varslist <- find_terms_in_model(model = object)
    
    c(list(classes = classes),
      varslist,
      x_is_factor = xvar %in% varslist$fnames,
      dx_is_factor = dx %in% varslist$fnames,
      list(data = data[, c(varslist$nnames, varslist$fnames), drop = FALSE]))
}

# what is the grid of the plot? (values of x and z over which to plot mfx)
cplot_grid <- function(object,
                       data,
                       dx,
                       level,
                       xvar,
                       zvar,
                       xvals,
                       zvals,
                       at,
                       n,
                       ...) {

    # handle factors and subset data
    data <- force(data)
    f <- check_factors(object = object, data = data, xvar = xvar, dx = dx)
    x_is_factor <-  f[["x_is_factor"]]
    dx_is_factor <- f[["dx_is_factor"]]
    dat <- f[["data"]]

    # setup xvals (based on whether factor)
    if (is.null(xvals)) {
        if (isTRUE(x_is_factor)) {
            if (is.factor(dat[[xvar]])) {
                xvals <- as.character(levels(dat[[clean_terms(xvar)]]))
            } else {
                xvals <- as.character(unique(dat[[clean_terms(xvar)]]))
            }
        } else {
            xvals <- prediction::seq_range(data[[xvar]], n = n)
        } 
    }

    # at argument
    at <- setNames(list(xvals), xvar)

    # 3rd dimension
    z_valid <- !is.null(zvar) & !is.null(zvals)
    if (z_valid) {
        at[[zvar]] <- zvals
    } else {
        zvals <- NULL
    }

    # output
    out <- list('dat' = dat, 
                'xvals' = xvals, 
                'zvals' = zvals,
                'at' = at)
    return(out)
}
