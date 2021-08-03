
## 
## FUNCTION simil() for simple matching counting of data frame coocurrences
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.3 (03-08-2021)
##
## Parameters
## x         (data frame or list object with vectors to compare)
## vars      (vector with column(s) in x representing attribute variables)
## uniq      (optional for only unique elements?)
## diag.incl (optional and logical include entries in diagonal?)


simil <-
function (x, vars, uniq, diag.incl) 
{
    ifelse(is.data.frame(x) == FALSE, x <- as.data.frame(do.call(rbind, 
        x)), NA)
    if (missing(vars) == TRUE) {
        vars <- seq_len(ncol(x))
    }
    else {
        if (is.vector(vars) == FALSE) 
            stop("\"vars\" must be a vector.")
    }
    ifelse(missing(uniq) == FALSE && isTRUE(uniq == FALSE) == 
        TRUE, NA, x <- unique(x))
    ifelse(is.null(x$ID) == TRUE, mat <- matrix(0L, nrow = nrow(x), 
        ncol = nrow(x), dimnames = list(unlist(x[, 1]), unlist(x[, 
            1]))), mat <- matrix(0L, nrow = nrow(x), ncol = nrow(x), 
        dimnames = list(x$ID, x$ID)))
    for (at in vars) {
        ccat <- unlist(unique(x[, at]))
        for (i in seq_len(length(ccat))) {
            slc <- which(x[, at] == ccat[i])
            mat[slc, slc] <- mat[slc, slc] + 1L
        }
        rm(i)
    }
    rm(at)
    ifelse(missing(diag.incl) == FALSE && isTRUE(diag.incl == 
        TRUE) == TRUE, NA, diag(mat) <- 0)
    return(mat)
}
