edhw <-
function (vars, x = NULL, as = c("list", "df"), limit, id, na.rm, 
    ...) 
{
    flgdf <- FALSE
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
    else if (isTRUE(is.data.frame(x) == TRUE) == TRUE) {
        flgdf <- TRUE
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    if (all(vars %in% unique(names(unlist(x)))) == FALSE) {
        warning(paste("Variable(s)", vars[which(!(vars %in% unique(names(unlist(x)))))], 
            "is/are disregarded", sep = " "))
        vars <- vars[which(vars %in% unique(names(unlist(x))))]
    }
    else {
        NA
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
            if (isTRUE(flgdf == TRUE) == TRUE && match.arg(as) == 
                "df") {
                edhlm <- split(x, seq(nrow(x)))
                edhlv <- lapply(edhlm, `[`, vars)
                edhl <- vector("list", length(edhlv))
                for (k in seq_len(length(edhlv))) {
                  edhlv[[k]] <- apply(edhlv[[k]], 2, function(x) suppressWarnings(levels(x) <- sub("NULL", 
                    NA, x)))
                  edhlv[[k]][edhlv[[k]] == ""] <- NA
                  edhlv[[k]][edhlv[[k]] == "list()"] <- NA
                  edhll <- vector("list", length(vars))
                  attr(edhll, "names") <- vars
                  for (i in seq_len(length(vars))) {
                    edhll[[i]] <- as.list(edhlv[[k]])
                  }
                  rm(i)
                  edhl[[k]] <- edhll
                }
                rm(k)
                rm(edhll)
            }
            else if (isTRUE(flgdf == TRUE) == TRUE && match.arg(as) == 
                "list") {
                edhl <- list()
                for (k in seq_len(dim(x)[1])) {
                  edhll <- vector("list", length(vars))
                  attr(edhll, "names") <- vars
                  for (i in seq_len(length(vars))) {
                    ifelse(isTRUE(length(x[[which(attr(x, "names") == 
                      vars[i])]][[k]]) == 0) == TRUE, edhll[i] <- NA, 
                      edhll[i] <- x[[which(attr(x, "names") == 
                        vars[i])]][[k]])
                  }
                  rm(i)
                  edhl[[k]] <- edhll
                }
                rm(k)
                rm(edhll)
            }
        }
    }
    flgp <- FALSE
    if (isTRUE(flgdf == TRUE) == FALSE) {
        edhlm <- x
        if (missing(vars) == FALSE && isTRUE(is.vector(vars) == 
            TRUE) == TRUE) {
            ifelse(isTRUE(vars == "people") == TRUE, flgp <- TRUE, 
                NA)
            edhl <- lapply(edhlm, `[`, vars)
            if (isTRUE(length(unique(names(unlist(edhl)))) != 
                length(vars)) == FALSE) {
                for (k in seq_len(length(edhl))) {
                  ifelse(any(is.na(names(edhl[[k]]))) == FALSE, 
                    NA, names(edhl[[k]])[which(is.na(names(edhl[[k]])))] <- vars[which(!(vars %in% 
                      names(edhl[[k]])))])
                }
                rm(k)
            }
            else {
                NA
            }
        }
        else if (missing(vars) == TRUE) {
            edhl <- edhlm
        }
        else {
            stop("Argument 'vars' should be a vector.")
        }
    }
    else {
        NA
    }
    if (match.arg(as) == "df") {
        if (isTRUE(flgp == FALSE) == TRUE) {
            warning("With the \"data frame\" option, component \"people\" is ignored.")
            pnames <- lapply(edhl, "names")
            edhl0 <- edhl
            for (n in seq_len(length(edhl))) {
                edhl0[[n]][which(pnames[[n]] == "people")] <- NULL
            }
            rm(n)
        }
        else if (isTRUE(flgp == TRUE) == TRUE) {
            pnames <- lapply(edhl, "names")
            for (n in seq_len(length(edhl))) {
                edhl0[[n]][which(pnames[[n]] != "people")] <- NULL
            }
            rm(n)
        }
        ifelse(isTRUE(flgdf == TRUE) == TRUE, vlbs <- (unique(unlist(lapply(edhl0, 
            "names")))), vlbs <- sort(unique(unlist(lapply(edhl0, 
            "names")))))
        xdf <- data.frame(matrix(ncol = length(vlbs), nrow = length(edhl0)))
        colnames(xdf) <- vlbs
        for (i in seq_len(length(edhl0))) {
            edhl0[[i]] <- edhl0[[i]][order(names(edhl0[[i]]))]
            edhl0[[i]][lengths(edhl0[[i]]) == 0L] <- NA
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
