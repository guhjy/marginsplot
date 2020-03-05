#' @rdname cplot_extract
#' @export
cplot_extract.clm <-
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
         what,
         zvar,
         zvals,
         ...) {

    if (!is.null(zvar) | !is.null(zvals)) {
        stop('The `zvar` and `zvals` arguments are not supported for this type of model.')
    }

    if (what == "effect") {
        stop("Displaying marginal effects is not currently supported for 'clm' models!")
    }

    # confidence level
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # plot grid (values of x and z to draw)
    cg <- cplot_grid(object = object, data = data, dx = dx, level = level, xvar = xvar, 
                     zvar = zvar, xvals = xvals, zvals = zvals, at = at, n = n)
    for (v in seq_along(cg)) assign(names(cg)[v], cg[[v]])

    # setup `outdat` data
    if (what %in% c("prediction", "classprediction", "stackedprediction")) {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], prediction::mean_or_mode)

        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, level = level)

        if (what == "classprediction") {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted.class"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted.class"]]))
        } else {
            out <- list()
            preds <- grep("Pr", names(outdat))
            for (i in preds) {
                if (what == "stackedprediction" && i != preds[1L]) {
                    outdat[[i]] <- outdat[[i]] + outdat[[i - 1L]]
                }
                out[[i - preds[1] + 1]] <- structure(list(xvals = xvals,
                                                          yvals = outdat[[i]],
                                                          zvals = names(outdat)[i]),
                                      class = "data.frame", 
                                      row.names = seq_along(outdat[["fitted"]]))
            }
            out <- do.call("rbind", out)
        }
    } 

    # output
    return(out)

}
