
## 
## FUNCTION simil() similarity based on data frame coocurrences
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1 (16-04-2020)
##
## Parameters
## x  a data frame or a list object with vectors to compare
## att (vector) column(s) in x representing attributes
## null (optional) include NA or NULLs?
## uniq (optional) remove duplicates?
## diag.incl (optional) include entries in diagonal?


simil <-
function (x, att, null, uniq, diag.incl) 
{
    ifelse(is.data.frame(x) == FALSE, x <- as.data.frame(do.call(rbind, 
        x)), NA)
    if (missing(att) == TRUE) {
        att <- seq_len(ncol(x))
    }
    else {
        ifelse(is.vector(att) == TRUE, NA, stop("\"att\" must be a vector."))
    }
    ifelse(missing(uniq) == FALSE && isTRUE(uniq == FALSE) == 
        TRUE, NA, x <- unique(x))
    ifelse(is.null(x$ID) == TRUE, mat <- matrix(0L, nrow = nrow(x), 
        ncol = nrow(x), dimnames = list(unlist(x[, 1]), unlist(x[, 
            1]))), mat <- matrix(0L, nrow = nrow(x), ncol = nrow(x), 
        dimnames = list(x$ID, x$ID)))
    for (at in att) {
        ccat <- unlist(unique(x[, at]))
        ifelse(missing(null) == FALSE && isTRUE(null == TRUE) == 
            TRUE, NA, ccat <- ccat[which(ccat != "NULL")])
        for (i in seq_len(length(ccat))) {
            mat[which(x[, at] == ccat[i]), which(x[, at] == ccat[i])] <- mat[which(x[, 
                at] == ccat[i]), which(x[, at] == ccat[i])] + 
                1L
        }
        rm(i)
    }
    rm(at)
    ifelse(missing(diag.incl) == FALSE && isTRUE(diag.incl == 
        TRUE) == TRUE, NA, diag(mat) <- 0)
    return(mat)
}
