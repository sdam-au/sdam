
## 
## FUNCTION edhw() to manipulate data API from Epigraphic Database Heidelberg EDH
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.4.1 (27-10-2020)
##
## PARAMETERS
##
## vars   (variables or attributes to be chosen)
## as     (type of output, lists or data frames)
##
## OPTIONAL PARAMETERS
##
## x     (typically fragments of EDH dataset)
## addID (logical, add "HD id" to output?)
## limit (integers, vector with HD nr records to limit output)
## id    (integer or character, select only the hd_nr id)
## na.rm (remove data entries with <NA>?)
## wide  (logical, use wide format in data frame?)
## ...   (optional parameters)
##


edhw <-
function (vars, x = NULL, as = c("list", "df"), addID, limit, 
    id, na.rm, wide, ...) 
{
    flgdf <- FALSE
    if (is.null(x) == TRUE) {
        warning("\"x\" is NULL and dataset \"EDH\" is taken if available.")
        if (!(exists("EDH"))) {
            utils::data("EDH", package = "sdam", envir = environment())
            EDH <- get("EDH", envir = environment())
            EDH$cite <- NULL
        }
        else {
            NA
        }
        x <- EDH
    }
    else if (isTRUE(is.data.frame(x) == TRUE) == TRUE) {
        flgdf <- TRUE
        ifelse(isTRUE(is.list(x) == TRUE) == TRUE, x <- as.data.frame(x), 
            NA)
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    if (missing(addID) == FALSE && isTRUE(addID == FALSE) == 
        TRUE) {
        ifelse(isTRUE("id" %in% vars) == TRUE, addID <- TRUE, 
            addID <- FALSE)
    }
    else {
        addID <- TRUE
    }
    ifelse(missing(wide) == FALSE && isTRUE(wide == TRUE) == 
        TRUE, wide <- TRUE, wide <- FALSE)
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
        ifelse(isTRUE("id" %in% vars) == TRUE, vars <- vars[which(!(vars == 
            "id"))], NA)
    }
    ifelse(isTRUE(flgdf == FALSE) == TRUE, xvars <- unique(names(unlist(x))), 
        xvars <- colnames(x))
    ifelse(isTRUE("people" %in% vars) == TRUE, flgp <- TRUE, 
        flgp <- FALSE)
    if (isTRUE(vars != "people") == TRUE && all(vars %in% xvars) == 
        FALSE) {
        warning(paste("Variable(s)", vars[which(!(vars %in% xvars))], 
            "is/are not present in \"x\" and may be disregarded.", 
            sep = " "))
        vars <- vars[which(vars %in% xvars)]
    }
    else if (isTRUE(vars %in% xvars) == FALSE && isTRUE(length(unlist(strsplit(xvars, 
        split = "people."))) > length(xvars)) == FALSE) {
        warning(paste("Variable(s)", vars[which(!(vars %in% xvars))], 
            "is/are not present in input data.", sep = " "))
        return(NULL)
    }
    else if (all(vars %in% xvars) == FALSE) {
        if (isTRUE(length(vars[which(!(vars %in% xvars))][vars[which(!(vars %in% 
            xvars))] != "people"]) > 0) == TRUE) {
            warning(paste("Variable(s)", paste(vars[which(!(vars %in% 
                xvars))][vars[which(!(vars %in% xvars))] != "people"], 
                collapse = ", "), "is(are) not present in \"x\" and might been disregarded.", 
                sep = " "))
        }
        npvars <- vars[(vars %in% xvars)]
        ifelse(isTRUE(flgp == TRUE) == TRUE, vars <- c("people", 
            npvars), vars <- npvars)
        rm(npvars)
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
            return(x)
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
    if (isTRUE(flgdf == TRUE) == FALSE) {
        if (missing(vars) == FALSE && isTRUE(is.vector(vars) == 
            TRUE) == TRUE) {
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
            pnames <- lapply(edhl, "names")
            edhl0 <- edhl
            for (n in seq_len(length(edhl))) {
                edhl0[[n]][which(pnames[[n]] == "people")] <- NULL
            }
            rm(n)
        }
        else if (isTRUE(flgp == TRUE) == TRUE) {
            ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                TRUE) == TRUE, warning("Argument \"na.rm\" is deactivated for variable \"people\"."), 
                NA)
            pnames <- lapply(edhl, "names")
            if (all(is.na(edhl)) == FALSE) {
                if (isTRUE(vars == "people") == TRUE) {
                  edhlp <- edhl[which(!(is.na(unlist(pnames))))]
                  if (isTRUE(length(edhlp) > 0) == TRUE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    return(NULL)
                  }
                }
                else {
                  edhlp <- lapply(edhlm, `[`, "people")
                  edhlq <- lapply(edhlm, `[`, vars[which(vars != 
                    "people")])
                  if (isTRUE(length(edhlp) > 0) == TRUE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    NA
                  }
                  if (isTRUE(length(edhlq) > 0) == TRUE) {
                    qlbs <- sort(unique(unlist(lapply(edhlq, 
                      "names"))))
                  }
                  else {
                    NA
                  }
                  ifelse(isTRUE(addID == TRUE) == TRUE, qlbs <- append("id", 
                    qlbs), NA)
                }
            }
            else {
                return(as.data.frame(edhl))
            }
            ids <- vector()
            for (i in seq_len(length(edhl))) {
                ids <- append(ids, edhlm[[i]]$id)
            }
            rm(i)
            ids <- ids[which(is.na(pnames) == FALSE)]
            if (isTRUE(wide == TRUE) == FALSE) {
                xdfp <- data.frame(matrix(ncol = length(plbs), 
                  nrow = 0))
                colnames(xdfp) <- plbs
                for (k in seq_len(length(edhlp))) {
                  if (is.null(unlist(edhlp[[k]])) == FALSE) {
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
                    ifelse(isTRUE(addID == TRUE) == TRUE, tmpdf[, 
                      1] <- ids[k], NA)
                    xdfp <- rbind(xdfp, tmpdf)
                  }
                  else {
                    NA
                  }
                }
                rm(k)
                if (isTRUE(vars == "people") == TRUE) {
                  return(xdfp)
                }
                else {
                  if (isTRUE(any(is.na(qlbs)) == TRUE)) {
                    xdfq <- data.frame(matrix(ncol = length(as.vector(stats::na.omit(qlbs))), 
                      nrow = length(edhlq)))
                    colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                    for (i in seq_len(length(edhlq))) {
                      edhlq[[i]] <- edhlq[[i]][order(names(edhlq[[i]]))]
                      edhlq[[i]][lengths(edhlq[[i]]) == 0L] <- NA
                      xdfq[i, 2:ncol(xdfq)] <- as.vector(unlist(edhlq[[i]]))
                    }
                    rm(i)
                  }
                  else {
                    xdfq <- data.frame(matrix(ncol = length(qlbs), 
                      nrow = length(edhlq)))
                    colnames(xdfq) <- qlbs
                    for (i in seq_len(length(edhlq))) {
                      edhlq[[i]] <- edhlq[[i]][order(names(edhlq[[i]]))]
                      edhlq[[i]][lengths(edhlq[[i]]) == 0L] <- NA
                      xdfq[i, which((qlbs %in% attr(edhlq[[i]], 
                        "names")))] <- as.vector(unlist(edhlq[[i]]))
                    }
                    rm(i)
                  }
                  ifelse(isTRUE(addID == TRUE) == TRUE, xdfq[, 
                    1] <- ids, NA)
                  xdfpq <- merge(xdfp, xdfq, all.x = TRUE)
                  return(xdfpq)
                }
            }
            else if (isTRUE(wide == TRUE) == TRUE) {
                pp <- max(as.numeric(unlist(edhlm)[which(attr(unlist(edhlm), 
                  "names") == "people.person_id")]))
                plbss <- vector()
                for (i in seq_len(pp)) {
                  plbss <- append(plbss, paste(plbs[2:length(plbs)], 
                    i, sep = ""))
                }
                rm(i)
                ifelse(isTRUE(addID == TRUE) == TRUE, plbss <- append("id", 
                  plbss), NA)
                options(stringsAsFactors = FALSE)
                xdfpp <- data.frame(matrix(ncol = length(plbss), 
                  nrow = 0))
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
                  if (isTRUE(nrow(tmpdf) > 1L) == TRUE) {
                    vecp <- vector()
                    for (i in seq_len(nrow(tmpdf))) {
                      vecp <- append(vecp, unlist(tmpdf[i, 2:ncol(tmpdf)], 
                        use.names = FALSE))
                    }
                    rm(i)
                    ifelse(isTRUE(dim(tmpdf)[1] == pp) == TRUE, 
                      vecpp <- c(ids[k], vecp), vecpp <- c(ids[k], 
                        vecp, rep(NA, (length(plbss) - length(vecp) - 
                          1L))))
                    xdfpp <- (rbind(xdfpp, vecpp))
                  }
                  else if (isTRUE(nrow(tmpdf) == 1L) == TRUE) {
                    if (isTRUE(dim(xdfpp)[1] == 0) == TRUE) {
                      xdfpp <- rbind(as.vector(unlist(xdfpp)), 
                        c(ids[k], as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                          rep(NA, (length(plbss) - ncol(tmpdf)))))
                    }
                    else {
                      xdfpp <- (rbind(xdfpp, c(ids[k], as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                        rep(NA, (length(plbss) - ncol(tmpdf))))))
                    }
                  }
                }
                rm(k)
                xdfpp <- as.data.frame(xdfpp)
                colnames(xdfpp) <- plbss
                rownames(xdfpp) <- NULL
                if (isTRUE(vars == "people") == TRUE) {
                  return(xdfpp)
                }
                else {
                  xdfq <- data.frame(matrix(ncol = length(as.vector(stats::na.omit(qlbs))), 
                    nrow = length(edhlq)))
                  colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                  for (i in seq_len(length(edhlq))) {
                    edhlq[[i]] <- edhlq[[i]][order(names(edhlq[[i]]))]
                    edhlq[[i]][lengths(edhlq[[i]]) == 0L] <- NA
                    xdfq[i, 2:ncol(xdfq)] <- as.vector(unlist(edhlq[[i]]))
                  }
                  rm(i)
                  ifelse(isTRUE(addID == TRUE) == TRUE, xdfq[, 
                    1] <- ids, NA)
                  xdfpq <- merge(xdfpp, xdfq, all.x = TRUE)
                  return(xdfpq)
                }
            }
        }
        if (isTRUE(flgp == FALSE) == TRUE) {
            ids <- vector()
            for (i in seq_len(length(edhl0))) {
                ids <- append(ids, edhlm[[i]]$id)
            }
            rm(i)
            ids <- ids[which(is.na(pnames) == FALSE)]
            ifelse(isTRUE(flgdf == TRUE) == TRUE, vlbs <- (unique(unlist(lapply(edhl0, 
                "names")))), vlbs <- sort(unique(unlist(lapply(edhl0, 
                "names")))))
            ifelse(isTRUE(addID == TRUE) == TRUE, vlbs <- append("id", 
                vlbs), NA)
            xdf <- data.frame(matrix(ncol = length(vlbs), nrow = length(edhl0)))
            colnames(xdf) <- vlbs
            for (i in seq_len(length(edhl0))) {
                edhl0[[i]] <- edhl0[[i]][order(names(edhl0[[i]]))]
                edhl0[[i]][lengths(edhl0[[i]]) == 0L] <- NA
                xdf[i, which((vlbs %in% attr(edhl0[[i]], "names")))] <- as.vector(unlist(edhl0[[i]]))
            }
            rm(i)
            ifelse(isTRUE(addID == TRUE) == TRUE, xdf[, 1] <- ids, 
                NA)
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
                      edhl0[[length(edhl0) + 1L]] <- c(edhlm[[n]]$id, 
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
            edhl1 <- edhl
            for (n in seq_len(length(edhl))) {
                ifelse(any(is.na(attr(edhl[[n]], "names"))) == 
                  FALSE, NA, attr(edhl1[[n]], "names")[which(is.na(attr(edhl[[n]], 
                  "names")))] <- vars[which(is.na(attr(edhl[[n]], 
                  "names")))])
            }
            rm(n)
            return(edhl1)
        }
    }
    else {
        NA
    }
}
