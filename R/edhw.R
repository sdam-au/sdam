edhw <-
function (vars, as = c("list", "df"), limit, id, na.rm, ...) 
{
    if (!(exists("EDH"))) {
        utils::data("EDH", package = "sdam", envir = environment())
        EDH <- get("EDH", envir = environment())
    }
    else {
        NA
    }
    EDH$cite <- NULL
    if (missing(id) == FALSE) {
        edhlm <- list()
        for (i in id) {
            edhlm[length(edhlm) + 1L] <- EDH[as.numeric(which(unlist(lapply(EDH, 
                `[`, "ID")) == sprintf("%06d", as.numeric(i))))]
        }
        rm(i)
    }
    else {
        if (missing(limit) == FALSE) {
            ifelse(isTRUE(length(limit) == 1L) == TRUE, edhlm <- EDH[seq_len(limit)], 
                edhlm <- EDH[limit])
        }
        else {
            edhlm <- EDH
        }
    }
    edhl <- lapply(edhlm, `[`, vars)
    if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == TRUE) {
        edhl0 <- list()
        for (n in seq_len(length(edhl))) {
            ifelse(any(is.na(attr(edhl[[n]], "names"))) == FALSE, 
                edhl0[[length(edhl0) + 1L]] <- edhl[[n]], NA)
        }
        rm(n)
    }
    else {
        edhl0 <- edhl
    }
    if (match.arg(as) == "df") {
        return(as.data.frame(do.call(rbind, edhl0)))
    }
    else {
        return(edhl0)
    }
}
