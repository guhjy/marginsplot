#' Generic extracts model information for use by `cplot`
#'
#' @rdname cplot_extract
#' @inheritParams cplot
#' @param xvar The name of the variable to show on the x-axis
#' @param zvar name of the third dimension variable over which quantities should
#'   be plotted (as facets).
#' @export
cplot_extract <- function(object, ...) {
    UseMethod("cplot_extract")
}

#' Internal function to extract data for `cplot`
#'
#' @rdname cplot_extract
#' @inheritParams cplot 
#' @inheritParams cplot_extract
cplot_extract.default <- function(object, 
                                  data, 
                                  dx, 
                                  level, 
                                  xvar, 
                                  zvar,
                                  xvals,
                                  zvals,
                                  at,
                                  n,
                                  type, 
                                  vcov,
                                  what,
                                  ...) {

    # confidence level
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # plot grid (values of x and z to draw)
    cg <- cplot_grid(object = object, data = data, dx = dx, level = level, xvar = xvar, 
                     zvar = zvar, xvals = xvals, zvals = zvals, at = at, n = n)
    for (v in seq_along(cg)) assign(names(cg)[v], cg[[v]])
    
    # extract predicted values
    if (what == "prediction") {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], prediction::mean_or_mode)

        # data.frame with all combinations of xvals, zvals, and mean/mode values 
        tmpdat[[xvar]] <- xvals
        if (!is.null(zvals)) {
            tmpdat[[zvar]] <- zvals
        }
        tmpdat <- expand.grid(tmpdat, stringsAsFactors = FALSE)

        # predicted values
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level, vcov = vcov)

        # output
        out <- structure(list(xvals = outdat[[xvar]],
                              yvals = outdat[["fitted"]],
                              upper = outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]]),
                              lower = outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]])),
                         class = "data.frame", row.names = seq_along(outdat[["fitted"]]))
        if (!is.null(zvals)) {
            out[['zvals']] <- outdat[[zvar]]
        }

    # extract marginal effects
    } else if (what == "effect") {

        if (is.factor(dat[[dx]]) && nlevels(data[[dx]]) > 2L) {
            stop("Displaying effect of a factor variable with > 2 levels is not currently supported!")
        }

        marg <- margins(model = object, data = data, at = at, type = type, vcov = vcov)

        if (is.null(zvals)) {
            out <- summary(marg, level = level)[ , c(xvar, "AME", "upper", "lower", "factor"), drop = FALSE]
            out <- setNames(out[out[["factor"]] == dx, , drop = FALSE], 
                            c("xvals", "yvals", "upper", "lower", "factor"))
        } else {
            out <- summary(marg, level = level)[ , c(xvar, zvar, "AME", "upper", "lower", "factor"), drop = FALSE]
            out <- setNames(out[out[["factor"]] == dx, , drop = FALSE], 
                            c("xvals", "zvals", "yvals", "upper", "lower", "factor"))
        }

    }

    # output
    return(out)
}
