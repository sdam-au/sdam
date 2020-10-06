edhw <-
function (vars, x = NULL, as = c("list", "df"), addID, limit, 
    id, na.rm, ...) 
{
    flgx <- TRUE
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
        flgx <- FALSE
        x <- EDH
    }
    else if (isTRUE(is.data.frame(x) == TRUE) == TRUE) {
        flgdf <- TRUE
        ifelse(isTRUE(is.list(x) == TRUE) == TRUE, x <- as.data.frame(x), 
            NA)
        ifelse((match.arg(as) == "df")%%(class(x) %in% c("tbl_df", 
            "tbl") == FALSE), x[x == "list()"] <- NA, NA)
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    ifelse(missing(addID) == FALSE && isTRUE(addID == TRUE) == 
        FALSE, addID <- FALSE, addID <- TRUE)
    if (missing(vars) == TRUE) {
        if (match.arg(as) == "list") {
            ifelse(isTRUE(flgdf == TRUE) == TRUE, return(as.list(x)), 
                return(x))
        }
        else if (match.arg(as) == "df") {
            ifelse(isTRUE(flgdf == FALSE) == TRUE, vars <- unique(names(unlist(x))), 
                return(x))
        }
    }
    else {
        NA
    }
    ifelse(isTRUE(flgdf == FALSE) == TRUE, xvars <- unique(names(unlist(x))), 
        xvars <- colnames(x))
    if (isTRUE(vars != "people") == TRUE && all(vars %in% xvars) == 
        FALSE) {
        warning(paste("Variable(s)", vars[which(!(vars %in% xvars))], 
            "is/are not present and disregarded.", sep = " "))
        vars <- vars[which(vars %in% xvars)]
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
            edhlm <- x
        }
    }
    if (isTRUE(flgdf == TRUE) == TRUE) {
        warning("When \"x\" is a data frame, argument \"limit\" is not available.")
        if (match.arg(as) == "df") {
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
        else if (match.arg(as) == "list") {
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
    else {
        NA
    }
    flgp <- FALSE
    if (isTRUE(flgdf == TRUE) == FALSE) {
        if (missing(vars) == FALSE && isTRUE(is.vector(vars) == 
            TRUE) == TRUE) {
            edhl <- lapply(edhlm, `[`, vars)
            ifelse(isTRUE(vars == "people") == TRUE, flgp <- TRUE, 
                NA)
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
            pnames <- lapply(edhl, "names")
            edhl0 <- edhl
            for (n in seq_len(length(edhl))) {
                edhl0[[n]][which(pnames[[n]] == "people")] <- NULL
            }
            rm(n)
        }
        else if (isTRUE(flgp == TRUE) == TRUE) {
            if (isTRUE(flgx == TRUE) == TRUE) {
                pnames <- lapply(edhl, "names")
                edhlp <- edhl[which(!(is.na(unlist(pnames))))]
                plbs <- unique(attr(unlist(edhlp), "names"))
                plbs <- sort(unique(unlist(strsplit(plbs, split = "people."))))
                ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                  NA)
            }
            else {
                edhlp <- list()
                for (i in seq_len(length(edhl))) {
                  if (is.na(names(edhl[[i]])) == FALSE) {
                    edhlp[[length(edhlp) + 1L]] <- edhl[[i]]
                  }
                  else {
                    NA
                  }
                }
                rm(i)
                plbs <- rev(sort(unique(names(unlist(edhl)))))
                plbs <- sort(unique(unlist(strsplit(plbs, split = "people."))))
                ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                  NA)
            }
            xdfp <- data.frame(matrix(ncol = length(plbs), nrow = 0))
            colnames(xdfp) <- plbs
            for (k in seq_len(length(edhlp))) {
                tmpdf <- data.frame(matrix(ncol = length(plbs), 
                  nrow = 0))
                colnames(tmpdf) <- plbs
                for (i in seq_len(length(edhlp[[k]]$people))) {
                  edhlp[[k]]$people[[i]] <- edhlp[[k]]$people[[i]][order(names(edhlp[[k]]$people[[i]]))]
                  edhlp[[k]]$people[[i]][lengths(edhlp[[k]]$people[[i]]) == 
                    0L] <- NA
                  tmpdf[i, which((plbs %in% attr(edhlp[[k]]$people[[i]], 
                    "names")))] <- as.vector(unlist(edhlp[[k]]$people[[i]]))
                }
                rm(i)
                ids <- vector()
                for (i in seq_len(length(edhl))) {
                  ids <- append(ids, edhlm[[i]]$id)
                }
                rm(i)
                ifelse(isTRUE(addID == TRUE) == TRUE, tmpdf[, 
                  1] <- ids[k], NA)
                xdfp <- rbind(xdfp, tmpdf)
            }
            rm(k)
            ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                TRUE) == TRUE, warning("Argument \"na.rm\" is deactivated for variable \"people\"."), 
                NA)
            return(xdfp)
        }
        if (isTRUE(flgp == FALSE) == TRUE) {
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
            ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                TRUE) == TRUE, return(stats::na.omit(xdf)), return(xdf))
        }
    }
    else if (match.arg(as) == "list") {
        if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
            TRUE) {
            edhl0 <- list()
            for (n in seq_len(length(edhl))) {
                if (isTRUE(flgp == TRUE) == TRUE) {
                  if (isTRUE(addID == TRUE) == TRUE) {
                    ifelse(is.null(edhl[[n]]$people) == FALSE, 
                      edhl0[[length(edhl0) + 1L]] <- list(edhlm[[n]]$id, 
                        edhl[[n]]$people), NA)
                  }
                  else {
                    ifelse(is.null(edhl[[n]]$people) == FALSE, 
                      edhl0[[length(edhl0) + 1L]] <- edhl[[n]]$people, 
                      NA)
                  }
                }
                else {
                  ifelse(any(is.na(attr(edhl[[n]], "names"))) == 
                    FALSE, edhl0[[length(edhl0) + 1L]] <- edhl[[n]], 
                    NA)
                }
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
