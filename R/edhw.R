edhw <-
function (vars, x = NULL, as = c("list", "df"), limit, id, na.rm, 
    ...) 
{
    if (is.null(x) == TRUE) {
        if (!(exists("EDH"))) {
            utils::data("EDH", package = "sdam", envir = environment())
            EDH <- get("EDH", envir = environment())
        }
        else {
            NA
        }
        EDH$cite <- NULL
        x <- EDH
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    if (missing(id) == FALSE) {
        edhlm <- list()
        for (i in id) {
            edhlm[length(edhlm) + 1L] <- x[as.numeric(which(unlist(lapply(x, 
                `[`, "ID")) == sprintf("%06d", as.numeric(i))))]
        }
        rm(i)
    }
    else {
        if (missing(limit) == FALSE) {
            ifelse(isTRUE(length(limit) == 1L) == TRUE, edhlm <- x[seq_len(limit)], 
                edhlm <- x[limit])
        }
        else {
            edhlm <- x
        }
    }
    if (missing(vars) == FALSE && isTRUE(is.vector(vars) == TRUE) == 
        TRUE) {
        edhl <- lapply(edhlm, `[`, vars)
    }
    else if (missing(vars) == TRUE) {
        edhl <- edhlm
    }
    else {
        stop("Argument 'vars' should be a vector.")
    }
    if (match.arg(as) == "df") {
        warning("With this option, component 'people' is ignored and variables are sorted.")
        pnames <- lapply(edhl, "names")
        edhl0 <- edhl
        for (n in seq_len(length(edhl))) {
            edhl0[[n]][which(pnames[[n]] == "people")] <- NULL
        }
        rm(n)
        vlbs <- sort(unique(unlist(lapply(edhl0, "names"))))
        xdf <- data.frame(matrix(ncol = length(vlbs), nrow = length(edhl0)))
        colnames(xdf) <- vlbs
        for (i in seq_len(length(edhl0))) {
            edhl0[[i]] <- edhl0[[i]][order(names(edhl0[[i]]))]
            xdf[i, which((vlbs %in% attr(edhl0[[i]], "names")))] <- as.vector(unlist(edhl0[[i]]))
        }
        rm(i)
        ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
            TRUE, return(stats::na.omit(xdf)), return(xdf))
    }
    else if (match.arg(as) == "list") {
        if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
            TRUE) {
            edhl0 <- list()
            for (n in seq_len(length(edhl))) {
                ifelse(any(is.na(attr(edhl[[n]], "names"))) == 
                  FALSE, edhl0[[length(edhl0) + 1L]] <- edhl[[n]], 
                  NA)
            }
            rm(n)
            return(edhl0)
        }
        else {
            return(edhl)
        }
    }
    else {
        NA
    }
}
