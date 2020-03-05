#' @rdname cplot_extract
#' @export
cplot_extract.polr <-
function(object, 
         data,
         dx,
         level, 
         xvar,
         at,
         n,
         type, 
         xvals,
         vcov, 
         what = c("prediction", "classprediction", "stackedprediction", "effect"), 
         zvar,
         zvals,
         ...) {

    # NOTE: dx is not actually used by this function

    # sanity checks
    if (!is.null(zvar) | !is.null(zvals)) {                                       
        stop('The `zvar` and `zvals` arguments are not supported for this type of model.')     
    }  
    
    if (what == "effect") {
        stop("Displaying marginal effects is not currently supported for 'polr' models.")
    }
        
    # confidence interval
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # plot grid (values of x and z to draw)
    cg <- cplot_grid(object = object, data = data, dx = dx, level = level, xvar = xvar, 
                     zvar = zvar, xvals = xvals, zvals = zvals, at = at, n = n)
    for (v in seq_along(cg)) assign(names(cg)[v], cg[[v]])


    # extract predictions
    if (what %in% c("prediction", "classprediction", "stackedprediction")) {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, level = level)

        if (what == 'prediction') {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted"]]))

        } else if (what == "classprediction") {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted.class"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted.class"]]))

        } else if (what == "stackedprediction"){ 
            out <- list()
            for (i in grep('^Pr\\(', names(outdat))) {
                out[[i]] <- data.frame(xvals = xvals,
                                       yvals = outdat[[i]],
                                       zvals = names(outdat)[i],
                                       stringsAsFactors = FALSE)
            }
            out <- do.call('rbind', out)
        }

    } 

    # output
    return(out)
}

#' @rdname cplot_extract
#' @export
cplot_extract.multinom <- cplot_extract.polr
