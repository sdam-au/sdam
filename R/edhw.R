
## 
## FUNCTION edhw() to manipulate data API from the EDH dataset
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.8.0 (12-03-2021)
##
## PARAMETERS
##
## vars   (vector, variables or attributes to be chosen from x)
## as     (output format: lists or data frames)
## type   (data frame type: long or wide or narrow)
##
## OPTIONAL PARAMETERS
##
## x      (list, typically fragments of EDH dataset or database API)
## split  (logical, divide the data into groups by id?)
## select (vector, people variables to select)
## addID  (logical, add "HD id" to output?)
## limit  (integers, vector with # records to limit output, offset supported)
## id     (integer or character, select only hd_nr records)
## na.rm  (logical, remove data entries with <NA>?)
## clean  (optional for lists only, clean x?)
##


edhw <-
function (x = NULL, vars, as = c("df", "list"), type = c("long", 
    "wide", "narrow"), split, select, addID, limit, id, na.rm, 
    clean, province, gender, ...) 
{
    flgdf <- FALSE
    if (is.null(x) == TRUE) {
        warning("\"x\" is NULL and dataset \"EDH\" is taken if available.")
        if (!(exists("EDH"))) {
            utils::data("EDH", package = "sdam", envir = environment())
            EDH <- get("EDH", envir = environment())
        }
        else {
            NA
        }
        x <- EDH
        class(x) <- NULL
        comment(x) <- NULL
    }
    else if (isTRUE(is.data.frame(x) == TRUE) == TRUE) {
        flgdf <- TRUE
        ifelse(isTRUE(is.list(x) == TRUE) == TRUE, x <- as.data.frame(x), 
            NA)
        if (missing(province) == FALSE) {
            xp <- x[x$province == unlist(rp[which(names(rp) == 
                province)], use.names = FALSE), ]
        }
        else {
            xp <- x
        }
        if (missing(gender) == FALSE) {
            xp <- xp[-which(is.na(xp$gender)), ]
            return(xp[-which(xp$gender != gender), ])
        }
        else {
            return(xp)
        }
    }
    else if (isTRUE(is.list(x) == TRUE) == TRUE) {
        if (missing(clean) == FALSE && isTRUE(clean == TRUE) == 
            TRUE) {
            tmp <- x
            x <- lapply(tmp, function(x) {
                x[sapply(x, is.null)] <- NA
                x[x == "NULL"] <- NA
                x[x == "list()"] <- NA
                return(x)
            })
            rm(tmp)
        }
        else {
            NA
        }
    }
    else {
        ifelse(isTRUE(is.character(x) == TRUE) == TRUE, x <- eval(parse(text = x)), 
            NA)
    }
    if (missing(vars) == TRUE) {
        flgv <- FALSE
        if (match.arg(as) == "list") {
            if (missing(id) == TRUE && missing(limit) == TRUE) {
                ifelse(isTRUE(flgdf == TRUE) == TRUE, return(as.list(x)), 
                  return(x))
            }
            else {
                if (missing(id) == FALSE) {
                  ifelse(isTRUE(flgdf == TRUE) == TRUE, return(as.list(x[id])), 
                    return(x[id]))
                }
                else if (missing(id) == TRUE) {
                  NA
                }
            }
        }
        else if (match.arg(as) == "df") {
            ifelse(isTRUE(flgdf == FALSE) == TRUE, vars <- unique(names(unlist(x))), 
                return(x))
        }
    }
    else {
        if (is.character(vars) == FALSE) {
            if (isTRUE(isTRUE(is.list(vars) == TRUE) == TRUE) || 
                isTRUE(is.vector(vars) == FALSE) == TRUE) 
                stop("'vars' should be a vector or character.")
        }
        flgv <- TRUE
        ifelse(isTRUE("id" %in% vars) == TRUE, vars <- vars[which(!(vars == 
            "id"))], NA)
    }
    ifelse(isTRUE("people" %in% vars) == TRUE || any(grepl("people.", 
        vars, fixed = TRUE)) == TRUE, flgp <- TRUE, flgp <- FALSE)
    if (isTRUE(flgdf == TRUE) == TRUE) {
        xvars <- colnames(x)
    }
    else {
        ifelse(is.null(names(x)) == TRUE || isTRUE(unique(names(x)) == 
            "") == TRUE, xvars <- unique(names(unlist(x))), xvars <- unique(names(x)))
    }
    ifelse(isTRUE(typeof(x[[1]]) == "list") == FALSE, x <- list(x), 
        NA)
    if (isTRUE(flgv == TRUE) == TRUE) {
        if (isTRUE(vars != "people") == TRUE && all(vars %in% 
            xvars) == FALSE) {
            warning(paste("Variable(s)", vars[which(!(vars %in% 
                xvars))], "is/are not present in \"x\" and may be disregarded.", 
                sep = " "))
            vars <- vars[which(vars %in% xvars)]
        }
        else if (all(vars %in% xvars) == FALSE && isTRUE(length(unlist(strsplit(xvars, 
            split = "people."))) > length(xvars)) == FALSE) {
            warning(paste("Variable(s)", vars[which(!(vars %in% 
                xvars))], "is/are not present in input data.", 
                sep = " "))
        }
        else if (all(vars %in% xvars) == FALSE) {
            if (isTRUE(length(vars[which(!(vars %in% xvars))][vars[which(!(vars %in% 
                xvars))] != "people"]) > 0) == TRUE) {
                warning(paste("Variable(s)", paste(vars[which(!(vars %in% 
                  xvars))][vars[which(!(vars %in% xvars))] != 
                  "people"], collapse = ", "), "is(are) not present in \"x\" and might been disregarded.", 
                  sep = " "))
            }
        }
        else {
            NA
        }
    }
    if (missing(addID) == FALSE && isTRUE(addID == FALSE) == 
        TRUE) {
        if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
            TRUE) {
            warning("'addID' is set to TRUE for 'na.rm'.")
            addID <- TRUE
        }
        else {
            ifelse(isTRUE("id" %in% vars) == TRUE, addID <- TRUE, 
                addID <- FALSE)
        }
    }
    else {
        addID <- TRUE
    }
    if (isTRUE(flgdf == FALSE) == TRUE) {
        if (missing(id) == FALSE) {
            xn <- x
            xn[sapply(lapply(xn, function(x) {
                x$id
            }), is.null)] <- NULL
            edhlm <- list()
            for (i in id) {
                if (isTRUE(length(which(as.vector(unlist(lapply(xn, 
                  `[`, "id"))) == sprintf("HD%06d", as.numeric(i)))) > 
                  0) == TRUE) {
                  if ((xn[which(as.vector(unlist(lapply(xn, `[`, 
                    "id"))) == sprintf("HD%06d", as.numeric(i)))][[1]]$id == 
                    sprintf("HD%06d", as.numeric(i))) == FALSE) {
                    edhlm[length(edhlm) + 1L] <- xn[i]
                  }
                  else {
                    edhlm[length(edhlm) + 1L] <- xn[as.numeric(which(unlist(lapply(xn, 
                      `[`, "id")) == sprintf("HD%06d", as.numeric(i))))]
                  }
                }
                else {
                  ifelse(isTRUE(length(id) == 1L) == TRUE, return(NULL), 
                    NA)
                }
            }
            rm(i)
        }
        else if (missing(id) == TRUE) {
            if (missing(limit) == TRUE || (missing(limit) == 
                FALSE && isTRUE(length(x) < limit) == TRUE)) {
                edhlm <- x
            }
            else if (missing(limit) == FALSE) {
                if (isTRUE(length(limit) == 1L) == TRUE) {
                  edhlm <- x[seq_len(limit)]
                }
                else {
                  edhlm <- x[limit]
                }
            }
        }
    }
    if (isTRUE(flgdf == TRUE) == TRUE) {
        if (match.arg(as) == "df") {
            if (missing(vars) == FALSE) {
                x <- x[which(colnames(x) %in% vars)]
            }
            else {
                NA
            }
            if (missing(id) == FALSE) {
                tmp <- x
                if (any(is.na(do.call(c, lapply(tmp$id, (function(x) {
                  if (is.null(x) | length(x) == 0) {
                    NA
                  } else {
                    x
                  }
                }))))) == TRUE) {
                  tmpx <- tmp[!(sapply(tmp$id, is.null)), ]
                  pck <- which(unlist(lapply(tmpx$id, function(x) {
                    (as.numeric(paste(strsplit(x, "")[[1]][3:8], 
                      collapse = "")))
                  })) %in% id)
                  return(tmpx[pck, ])
                }
                else {
                  return(tmp[which(unlist(lapply(tmp$id, function(x) {
                    (as.numeric(paste(strsplit(x, "")[[1]][3:8], 
                      collapse = "")))
                  })) %in% id), ])
                }
            }
            else if (missing(limit) == FALSE) {
                if (isTRUE(length(limit) == 1L) == TRUE) {
                  return(head(x, limit))
                }
                else {
                  return(x[limit, ])
                }
            }
            else {
                return(x)
            }
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
    if (isTRUE(flgdf == TRUE) == FALSE) {
        if (isTRUE(flgv == TRUE) == TRUE && isTRUE(is.vector(vars) == 
            TRUE) == TRUE) {
            edhl <- lapply(edhlm, `[`, vars)
            ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                FALSE) == TRUE, valids <- seq_len(length(edhl)), 
                valids <- which(as.vector(unlist(lapply(edhl, 
                  function(x) {
                    all(is.na(as.vector(unlist(x))))
                  }))) == FALSE))
            if (missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
                TRUE) {
                edhrm <- lapply(lapply(lapply(edhl, names), is.na), 
                  all)
                if (isTRUE(length(which(unlist(edhrm) == TRUE)) > 
                  0) == TRUE) {
                  edhlm <- edhlm[-which(unlist(edhrm) == TRUE)]
                  edhl <- edhl[-which(unlist(edhrm) == TRUE)]
                }
                else {
                  NA
                }
                valids <- which(as.vector(unlist(lapply(edhl, 
                  function(x) {
                    all(is.na(as.vector(unlist(x))))
                  }))) == FALSE)
            }
            else {
                if (is.null(unlist(edhl)) == FALSE) {
                  if (isTRUE(length(unique(names(unlist(edhl)))) != 
                    length(vars)) == FALSE || any(is.na(unique(unlist(lapply(edhl, 
                    "names"))))) == TRUE) {
                    for (k in seq_len(length(edhl))) {
                      ifelse(any(is.na(names(edhl[[k]]))) == 
                        FALSE, NA, names(edhl[[k]])[which(is.na(names(edhl[[k]])))] <- vars[which(!(vars %in% 
                        names(edhl[[k]])))])
                    }
                    rm(k)
                  }
                  else {
                    NA
                  }
                  ifelse(lapply(edhl, function(z) {
                    length(z[sapply(z, is.null)])
                  })[[1]] > 0, flgn <- TRUE, flgn <- FALSE)
                  edhl <- lapply(edhl, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                }
                else {
                  return(NULL)
                }
            }
        }
        else if (isTRUE(flgv == FALSE) == TRUE) {
            edhl <- edhlm
            valids <- which(as.vector(unlist(lapply(edhl, function(x) {
                all(is.na(as.vector(unlist(x))))
            }))) == FALSE)
        }
        else {
            stop("Argument 'vars' should be a vector.")
        }
    }
    else {
        NA
    }
    ids <- lapply(edhlm, function(x) {
        x$id
    })
    if (match.arg(as) == "df") {
        ifelse(missing(split) == FALSE && isTRUE(split == TRUE) == 
            TRUE, split <- TRUE, split <- FALSE)
        if (isTRUE(flgp == TRUE) == TRUE) {
            pnames <- lapply(edhl, "names")
            if (all(is.na(edhl)) == FALSE) {
                if (isTRUE(vars == "people") == TRUE) {
                  edhlp <- edhl[which(!(is.na(unlist(pnames))))]
                  if (isTRUE(length(edhlp) > 0) == TRUE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    plbs <- plbs[which(plbs != "people")]
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    return(NULL)
                  }
                }
                else {
                  edhlp <- lapply(edhlm, `[`, "people")
                  npvars <- vars[(vars %in% xvars)]
                  ifelse(isTRUE(flgp == TRUE) == TRUE && isTRUE(vars == 
                    "people") == FALSE, vars <- c("people", npvars), 
                    vars <- npvars)
                  ifelse(isTRUE(flgv == FALSE) == TRUE, npvars <- npvars[which(grepl("people.", 
                    npvars, fixed = TRUE) == FALSE)], NA)
                  edhlp <- lapply(edhlp, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                  if (isTRUE(flgv == TRUE) == TRUE) {
                    edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                      "people")]))
                  }
                  else {
                    edhlq <- lapply(edhlm, `[`, npvars)
                    edhlq <- lapply(edhlq, setNames, npvars)
                    edhlq <- lapply(edhlq, function(x) {
                      x[sapply(x, is.null)] <- NA
                      return(x)
                    })
                  }
                  if (is.null(unlist(edhlq)) == FALSE) {
                    if (isTRUE(length(unique(names(unlist(edhlq)))) != 
                      length(npvars)) == FALSE || any(is.na(unique(unlist(lapply(edhlq, 
                      "names"))))) == TRUE) {
                      for (k in seq_len(length(edhlq))) {
                        ifelse(any(is.na(names(edhlq[[k]]))) == 
                          FALSE, NA, names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- npvars[which(!(npvars %in% 
                          names(edhlq[[k]])))])
                      }
                      rm(k)
                    }
                    else {
                      NA
                    }
                    edhlq <- lapply(edhlq, function(x) {
                      x[sapply(x, is.null)] <- NA
                      return(x)
                    })
                  }
                  if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                    FALSE) {
                    plbs <- unique(attr(unlist(edhlp), "names"))
                    plbs <- sort(unique(unlist(strsplit(plbs, 
                      split = "people."))))
                    ifelse(isTRUE(addID == TRUE) == TRUE, plbs[1] <- "id", 
                      plbs <- plbs[2:length(plbs)])
                  }
                  else {
                    plbs <- vector()
                  }
                  if (isTRUE(length(edhlq) > 0) == TRUE && is.null(unlist(edhlq)) == 
                    FALSE) {
                    qlbs <- sort(unique(unlist(lapply(edhlq, 
                      "names"))), na.last = TRUE)
                    ifelse(isTRUE(addID == TRUE) == TRUE, qlbs <- append("id", 
                      qlbs), NA)
                  }
                  else {
                    qlbs <- vector()
                  }
                }
            }
            else {
                return(as.data.frame(edhl))
            }
            ids <- ids[which(is.na(pnames) == FALSE)]
            if (match.arg(type) == "long") {
                if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                  FALSE) {
                  xdfp <- data.frame(matrix(ncol = length(plbs), 
                    nrow = 0))
                  colnames(xdfp) <- plbs
                  for (k in seq_len(length(edhlp))[valids]) {
                    if (is.null(unlist(edhlp[[k]])) == FALSE && 
                      is.na(unlist(edhlp[[k]])) == FALSE) {
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
                  if (missing(select) == FALSE) {
                    xdfp <- subset(xdfp, select = c("id", select[which(select %in% 
                      plbs)]))
                  }
                  else {
                    NA
                  }
                }
                else {
                  ifelse(isTRUE(vars == "people") == TRUE, return(NULL), 
                    NA)
                }
                if (isTRUE(vars == "people") == TRUE) {
                  if (missing(na.rm) == FALSE && isTRUE(na.rm == 
                    TRUE) == TRUE) {
                    if (isTRUE(nrow(xdfp) != 0) == TRUE && isTRUE(addID == 
                      TRUE) == TRUE) {
                      xdfp <- xdfp[which(apply(xdfp[2:ncol(xdfp)], 
                        1, function(x) {
                          all(is.na(x))
                        }) == FALSE), ]
                    }
                    else {
                      return(NULL)
                    }
                  }
                  else {
                    NA
                  }
                  if (isTRUE(split == TRUE) == TRUE) {
                    return(split(xdfp[, -1], xdfp$id))
                  }
                  else {
                    return(xdfp)
                  }
                }
                else {
                  if (isTRUE(length(edhlq) > 0) == TRUE) {
                    if (isTRUE(length(valids) == 1) == TRUE) {
                      edhlq <- as.list(unlist(edhlq[valids]))
                      xdfq <- data.frame(matrix(unlist(edhlq), 
                        ncol = length(edhlq), byrow = TRUE))
                      colnames(xdfq) <- names(edhlq)
                    }
                    else {
                      edhlq <- edhlq[valids]
                      xdfq <- data.frame(matrix(unlist(edhlq), 
                        ncol = max(lengths(edhlq)), byrow = TRUE))
                      qlbs <- sort(unique(unlist(lapply(edhlq, 
                        "names"))), na.last = TRUE)
                      colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                    }
                    if (isTRUE(addID == TRUE) == TRUE) {
                      xdfq$id <- ids[valids]
                      ifelse(isTRUE(flgv == TRUE) == TRUE, xdfq <- rev(xdfq), 
                        NA)
                    }
                    else {
                      NA
                    }
                  }
                  else {
                    ifelse(isTRUE("people" %in% vars == FALSE) == 
                      TRUE, return(NULL), NA)
                  }
                  if (isTRUE(length(edhlq) > 0) == TRUE && (isTRUE(length(edhlp) > 
                    0) == TRUE && is.null(unlist(edhlp)) == FALSE)) {
                    tmpxdfp <- xdfp
                    tmpxdfp$id <- sub("[[:alpha:]]+", "", xdfp$id)
                    tmpxdfq <- xdfq
                    tmpxdfq$id <- sub("[[:alpha:]]+", "", xdfq$id)
                    xdfpq <- merge(tmpxdfp, tmpxdfq, all = TRUE)
                    xdfpq$id <- paste("HD", xdfpq$id, sep = "")
                    if (missing(na.rm) == FALSE && isTRUE(na.rm == 
                      TRUE) == TRUE) {
                      if (isTRUE(nrow(xdfpq) != 0) == TRUE && 
                        isTRUE(addID == TRUE) == TRUE) {
                        xdfpq <- xdfpq[which(apply(xdfpq[2:ncol(xdfpq)], 
                          1, function(x) {
                            all(is.na(x))
                          }) == FALSE), ]
                      }
                      else {
                        return(NULL)
                      }
                    }
                    else {
                      NA
                    }
                    if (missing(na.rm) == FALSE && isTRUE(na.rm == 
                      TRUE) == TRUE) {
                      if (isTRUE(nrow(xdfq) != 0) == TRUE && 
                        isTRUE(addID == TRUE) == TRUE) {
                        xdfq <- xdfq[which(apply(xdfq[2:ncol(xdfq)], 
                          1, function(x) {
                            all(is.na(x))
                          }) == FALSE), ]
                      }
                      else {
                        return(NULL)
                      }
                    }
                    else {
                      NA
                    }
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpq[, -1], xdfpq$id))
                    }
                    else {
                      return(xdfpq)
                    }
                  }
                  else {
                    if (isTRUE(length(plbs) > 0) == TRUE) {
                      if (isTRUE(split == TRUE) == TRUE) {
                        return(split(xdfp[, -1], xdfp$id))
                      }
                      else {
                        return(xdfp)
                      }
                    }
                    else if (isTRUE(length(qlbs) > 0) == TRUE) {
                      if (isTRUE(split == TRUE) == TRUE) {
                        return(split(xdfq[, -1], xdfq$id))
                      }
                      else {
                        return(xdfq)
                      }
                    }
                    else {
                      return(NULL)
                    }
                  }
                }
            }
            else if (match.arg(type) == "wide") {
                ifelse(isTRUE(flgp == FALSE) == TRUE, NA, edhlp <- edhlp[valids])
                ifelse(isTRUE(vars == "people") == TRUE, NA, 
                  edhlq <- edhlq[valids])
                ifelse(isTRUE(addID == FALSE) == TRUE, NA, ids <- ids[valids])
                if (isTRUE(length(edhlp) > 0) == TRUE && is.null(unlist(edhlp)) == 
                  FALSE) {
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
                    if (isTRUE(is.na(edhlp[[k]]$people) == TRUE) == 
                      TRUE) {
                      NA
                    }
                    else {
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
                          vecp <- append(vecp, unlist(tmpdf[i, 
                            2:ncol(tmpdf)], use.names = FALSE))
                        }
                        rm(i)
                        ifelse(isTRUE(dim(tmpdf)[1] == pp) == 
                          TRUE, vecpp <- c(as.vector(unlist(ids))[k], 
                          vecp), vecpp <- c(as.vector(unlist(ids))[k], 
                          vecp, rep(NA, (length(plbss) - length(vecp) - 
                            1L))))
                        xdfpp <- (rbind(xdfpp, vecpp))
                      }
                      else if (isTRUE(nrow(tmpdf) == 1L) == TRUE) {
                        if (isTRUE(dim(xdfpp)[1] == 0) == TRUE) {
                          xdfpp <- rbind(as.vector(unlist(xdfpp)), 
                            c(as.vector(unlist(ids))[k], as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                              rep(NA, (length(plbss) - ncol(tmpdf)))))
                        }
                        else {
                          xdfpp <- (rbind(xdfpp, c(as.vector(unlist(ids))[k], 
                            as.vector(unlist(tmpdf[2:ncol(tmpdf)])), 
                            rep(NA, (length(plbss) - ncol(tmpdf))))))
                        }
                      }
                    }
                  }
                  rm(k)
                  xdfpp <- as.data.frame(xdfpp)
                  colnames(xdfpp) <- plbss
                  rownames(xdfpp) <- NULL
                  if (missing(select) == FALSE) {
                    colnames(xdfpp) <- c("id", sub("[[:digit:]]+$", 
                      "", plbss[2:length(plbss)]))
                    xdfpp <- xdfpp[, c(1, which(colnames(xdfpp) %in% 
                      select))]
                  }
                  else {
                    NA
                  }
                }
                else {
                  ifelse(isTRUE(vars == "people") == TRUE, return(NULL), 
                    NA)
                }
                if (isTRUE(vars == "people") == TRUE) {
                  if (isTRUE(split == TRUE) == TRUE) {
                    return(split(xdfpp[, -1], xdfpp$id))
                  }
                  else {
                    return(xdfpp)
                  }
                }
                else {
                  if (isTRUE(length(edhlq) > 0) == TRUE) {
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
                      1] <- as.vector(unlist(ids)), NA)
                  }
                  if (isTRUE(length(plbs) > 0) == TRUE && isTRUE(length(qlbs) > 
                    0) == TRUE) {
                    tmpxdfpp <- xdfpp
                    tmpxdfpp$id <- sub("[[:alpha:]]+", "", xdfpp$id)
                    tmpxdfq <- xdfq
                    tmpxdfq$id <- sub("[[:alpha:]]+", "", xdfq$id)
                    xdfpq <- merge(tmpxdfpp, tmpxdfq, all = TRUE)
                    xdfpq$id <- paste("HD", xdfpq$id, sep = "")
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpq[, -1], xdfpq$id))
                    }
                    else {
                      return(xdfpq)
                    }
                  }
                  else if (isTRUE(length(plbs) > 0) == TRUE) {
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfpp[, -1], xdfpp$id))
                    }
                    else {
                      return(xdfpp)
                    }
                  }
                  else if (isTRUE(length(qlbs) > 0) == TRUE) {
                    if (isTRUE(split == TRUE) == TRUE) {
                      return(split(xdfq[, -1], xdfq$id))
                    }
                    else {
                      return(xdfq)
                    }
                  }
                  else {
                    print("w")
                  }
                }
            }
            else if (match.arg(type) == "narrow") {
                print("Argument 'type' *narrow* is not yet implemented.")
            }
        }
        if (isTRUE(flgp == FALSE) == TRUE) {
            ids <- lapply(edhlm, function(x) {
                x$id
            })
            edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                "people")]))
            ifelse(lapply(edhlq, function(z) {
                length(z[sapply(z, is.null)])
            })[[1]] > 0, flgn <- TRUE, flgn <- FALSE)
            if (isTRUE(flgn == FALSE) == TRUE) {
                edhlq <- lapply(edhlq, function(x) {
                  x[sapply(x, is.null)] <- NA
                  return(x)
                })
            }
            else {
                NA
            }
            for (k in seq_len(length(edhlq))) {
                ifelse(any(is.na(names(edhlq[[k]]))) == FALSE, 
                  NA, names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- vars[which(!(vars %in% 
                    names(edhlq[[k]])))])
            }
            rm(k)
            if (isTRUE(length(edhlq) > 0) == TRUE) {
                if (isTRUE(flgn == TRUE) == TRUE) {
                  tmpq <- lapply(edhlq, function(z) {
                    (z[!(sapply(z, is.null))])
                  })
                  xdfq <- data.frame(matrix(unlist(tmpq), ncol = max(length(tmpq)), 
                    dimnames = list(NULL, names(tmpq[[1]]))))
                }
                else {
                  xdfq <- data.frame(matrix(unlist(edhlq), ncol = max(lengths(edhlq)), 
                    byrow = TRUE))
                  qlbs <- sort(unique(unlist(lapply(edhlq, "names"))), 
                    na.last = TRUE)
                  colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
                }
                if (isTRUE(addID == TRUE) == TRUE) {
                  xdfq$id <- as.vector(unlist(ids))
                  ifelse(isTRUE(flgv == TRUE) == TRUE, xdfq <- rev(xdfq), 
                    NA)
                }
                else {
                  NA
                }
            }
            else {
                return(NULL)
            }
            if (isTRUE(split == TRUE) == TRUE) {
                return(split(xdfq[, -1], xdfq$id))
            }
            else {
                return(xdfq)
            }
        }
    }
    else if (match.arg(as) == "list") {
        if (isTRUE(flgp == TRUE) == TRUE) {
            if (missing(select) == FALSE) {
                edhlp <- lapply(edhlm, `[`, "people")
                if (missing(na.rm) == FALSE && isTRUE(na.rm == 
                  TRUE) == TRUE) {
                  ids <- ids[which(as.vector(unlist(lapply(edhlp, 
                    function(x) all(is.na(x))))) == FALSE)]
                  edhlp0 <- Filter(function(x) !all(is.na(x)), 
                    edhlp)
                }
                else {
                  edhlp0 <- edhlp
                }
                edhl1 <- vector("list", length = length(edhlp0))
                for (k in seq_len(length(edhlp0))) {
                  tmpsl <- lapply(edhlp0[[k]]$people, `[`, select)
                  for (j in seq_len(length(tmpsl))) {
                    ifelse(any(is.na(names(tmpsl[[j]]))) == FALSE, 
                      NA, names(tmpsl[[j]])[which(is.na(names(tmpsl[[j]])))] <- select[which(!(select %in% 
                        names(tmpsl[[j]])))])
                  }
                  rm(j)
                  tmpsl <- lapply(tmpsl, function(x) {
                    x[sapply(x, is.null)] <- NA
                    return(x)
                  })
                  edhl1[[k]] <- tmpsl
                }
                rm(k)
            }
            else {
                edhl1 <- lapply(edhlm, `[`, "people")
            }
            if (isTRUE(vars == "people") == TRUE) {
                edhl2 <- Map(c, people = edhl1)
            }
            else if (isTRUE("people" %in% vars) == TRUE) {
                edhlq <- lapply(edhlm, `[`, sort(vars[which(vars != 
                  "people")]))
                ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                  TRUE) == TRUE, edhlq <- Filter(function(x) !all(is.na(x)), 
                  edhlq), NA)
                edhl2 <- suppressWarnings(Map(c, people = edhl1, 
                  edhlq))
            }
            else {
                edhl2 <- lapply(edhlm, `[`, sort(vars[which(vars != 
                  "people")]))
                ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                  TRUE) == TRUE, edhl2 <- Filter(function(x) !all(is.na(x)), 
                  edhl2), NA)
            }
        }
        if (isTRUE(addID == TRUE) == TRUE) {
            ifelse(isTRUE(flgp == FALSE) == TRUE, return(Map(c, 
                id = ids, edhl)), return(Map(c, id = ids, edhl2)))
        }
        else {
            ifelse(isTRUE(flgp == FALSE) == TRUE, return(edhl), 
                return(edhl2))
        }
    }
    else {
        NA
    }
}
