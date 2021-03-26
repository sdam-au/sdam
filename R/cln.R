
## 
## FUNCTION cln() to re-encode Greek characters
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.7 (26-03-2021)
##
## PARAMETERS
## x        (scalar or vector, with character to clean)
## level    (clean level, either 0, 1 or 2)
## na.rm    (logical, remove NA?)


cln <-
function (x, level = 1, na.rm) 
{
    if (is.data.frame(x) == TRUE) 
        warning("\"cln()\" does not support yet data frames.")
    ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == FALSE) == 
        TRUE, invisible(NA), x <- Filter(function(y) !all(is.na(y)), 
        x))
    if (isTRUE(length(x) == 1) == TRUE) {
        x1 <- as.vector(x)
        if (is.na(x1) == TRUE) 
            return(x)
        ifelse(isTRUE(level == 0) == TRUE, invisible(NA), x1 <- paste(strsplit(x1, 
            "")[[1]][which(!(strsplit(x1, "")[[1]] == "?"))], 
            collapse = ""))
        if (isTRUE(level > 1) == TRUE) {
            x1 <- gsub("\\s*\\([^\\)]\\)", "", x1)
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == "?"))], collapse = "")
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == "+"))], collapse = "")
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == "*"))], collapse = "")
        }
        else if (isTRUE(level == 1) == TRUE) {
            x1 <- gsub("\\s*\\([^\\)]\\)", "", x1)
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == "?"))], collapse = "")
        }
        else {
            invisible(NA)
        }
        gs1 <- which(as.raw(utf8ToInt(x1)) %in% c("e2", "e4", 
            "f6", "fc"))
        gs2 <- which(as.raw(utf8ToInt(x1)) %in% c("cf", "ce"))
        gs2a <- which(as.raw(utf8ToInt(x1)) %in% c("c2", "c3", 
            "c4", "c5", "c8"))
        gs3 <- which(as.raw(utf8ToInt(x1)) %in% c("e1"))
        if (isTRUE(length(gs3) > 0) == TRUE) {
            invisible(NA)
        }
        else {
            gs3 <- NULL
        }
        if (isTRUE(length(c(gs1, gs2, gs2a, gs3)) == 0) == TRUE) 
            return(x1)
        xx <- strsplit(rawToChar(as.raw(utf8ToInt(x1))), "")[[1]]
        ifelse(isTRUE(tail(xx, 1) == "+") == TRUE, flgp <- TRUE, 
            flgp <- FALSE)
        ifelse(isTRUE(tail(xx, 1) == "*") == TRUE, flga <- TRUE, 
            flga <- FALSE)
        if (isTRUE(length(c(gs2, gs2a, gs3)) == 0) == TRUE) {
            res <- paste(xx, collapse = "")
            names(res) <- x1
            return(res)
        }
        else {
            gsx <- sort(c(gs2, gs2a, gs3))
        }
        res <- vector()
        if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
            res <- append(res, xx[1:(min(gsx) - 1L)])
        }
        else {
            invisible(NA)
        }
        late <- c(c("e2", "e4", "f6", "fc"), c("cf", "ce"), c("c2", 
            "c3", "c4", "c5", "c8"), c("e1"))
        for (j in seq_along(gsx)) {
            i <- gsx[j]
            if (isTRUE(i %in% c(gs2, gs2a)) == TRUE) {
                tmp <- paste(xx[i:(i + 1L)], collapse = "")
                res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                  to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                  to = "UTF-8"))
                if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                  collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                  "c3"), c("e1")))) == FALSE && suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                  0) {
                  if (isTRUE(i < (length(xx) - 1L)) == TRUE) {
                    ifelse(isTRUE(j == length(gsx)) == TRUE, 
                      res <- append(res, xx[(i + 2L):length(xx)]), 
                      res <- append(res, xx[(i + 2L):(gsx[j + 
                        1L] - 1L)]))
                  }
                  else {
                    invisible(NA)
                  }
                }
            }
            else if (isTRUE(i %in% gs3) == TRUE) {
                tmp <- paste(xx[i:(i + 2L)], collapse = "")
                res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                  to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                  to = "UTF-8"))
                if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                  collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                  "c3"), c("e1")))) == FALSE && suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                  0) {
                  if (isTRUE(i < (length(xx) - 1L)) == TRUE) {
                    ifelse(isTRUE(j == length(gsx)) == TRUE, 
                      res <- append(res, xx[(i + 3L):length(xx)]), 
                      res <- append(res, xx[(i + 3L):(gsx[j + 
                        1L] - 1L)]))
                  }
                  else {
                    invisible(NA)
                  }
                }
            }
        }
        rm(j)
        if ("+" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
            1)]) {
            res <- append(res, "+")
        }
        else if ("*" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
            1)]) {
            res <- append(res, "*")
        }
        if (isTRUE(length(gs2a) == 0) == TRUE || suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
            collapse = ""))) %in% c(c("cf", "ce"), c("c2", "c3"), 
            c("e1"))) == FALSE)) {
            if (all(c(" ", "/", " ") %in% strsplit(x, "")[[1]]) == 
                TRUE) {
                if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
                  tmpr <- NULL
                  resl <- as.list(paste(c(tmpr, res), collapse = ""))
                  if (isTRUE(level < 2) == TRUE) {
                    ifelse(isTRUE(flgp == TRUE) == TRUE, resl <- as.list(paste(c(resl, 
                      "+"), collapse = "")), NA)
                    ifelse(isTRUE(flga == TRUE) == TRUE, resl <- as.list(paste(c(resl, 
                      "*"), collapse = "")), NA)
                  }
                }
                else {
                  tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                    c(" ", "/", " "))) - 2):length(xx)], collapse = ""), 
                    from = "UTF-8", to = "UTF-16LE", toRaw = TRUE), 
                    from = "UTF-16LE", to = "UTF-8")
                  resl <- as.list(paste(c(res, tmpr), collapse = ""))
                }
            }
            else {
                resl <- as.list(paste(res, collapse = ""))
                if (isTRUE(level < 2) == TRUE) {
                  ifelse(isTRUE(flgp == TRUE) == TRUE, resl <- as.list(paste(c(resl, 
                    "+"), collapse = "")), NA)
                  ifelse(isTRUE(flga == TRUE) == TRUE, resl <- as.list(paste(c(resl, 
                    "*"), collapse = "")), NA)
                }
            }
            names(resl) <- x
            return(resl)
        }
        else {
            x1p <- paste(res, collapse = "")
            gs2p <- which(suppressWarnings(as.raw(utf8ToInt(x1p))) %in% 
                c("cf", "ce"))
            gs3p <- which(suppressWarnings(as.raw(utf8ToInt(x1p))) %in% 
                c("e1"))
            if (isTRUE(length(gs3p) > 0) == TRUE) {
                gs3p <- gs3p[which(gs3p%%2 != 0)]
            }
            else {
                gs3p <- NULL
            }
            xxp <- strsplit(x1p, "")[[1]]
            xxpp <- xxp[min(c(gs2p, gs3p)):length(xxp)]
            x1pp <- paste(xxpp, collapse = "")
            gs2pp <- which(as.raw(utf8ToInt(x1pp)) %in% c("cf", 
                "ce"))
            gs3pp <- which(as.raw(utf8ToInt(x1pp)) %in% c("e1"))
            if (isTRUE(length(gs3pp) > 0) == TRUE) {
                gs3pp <- gs3pp[which(gs3pp%%2 != 0)]
            }
            else {
                gs3pp <- NULL
            }
            x3 <- strsplit(rawToChar(as.raw(utf8ToInt(x1pp))), 
                "")[[1]]
            resp <- vector()
            for (i in sort(c(gs2pp, gs3pp))) {
                if (isTRUE(i %in% c(gs2pp)) == TRUE) {
                  tmp <- paste(x3[i:(i + 1L)], collapse = "")
                  resp <- append(resp, iconv(iconv(tmp, from = "UTF-8", 
                    to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                    to = "UTF-8"))
                }
                else if (isTRUE(i %in% gs3pp) == TRUE) {
                  tmp <- paste(x3[i:(i + 2L)], collapse = "")
                  resp <- append(resp, iconv(iconv(tmp, from = "UTF-8", 
                    to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                    to = "UTF-8"))
                }
            }
            rm(i)
            if (all(c(" ", "/", " ") %in% strsplit(x, "")[[1]]) == 
                TRUE) {
                if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
                  tmpr <- iconv(iconv(paste(xx[seq_len(min(c(gs2, 
                    gs2a, gs3)) - 1)], collapse = ""), from = "UTF-8", 
                    to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                    to = "UTF-8")
                  respl <- as.list(paste(c(tmpr, resp), collapse = ""))
                  if (isTRUE(level < 2) == TRUE) {
                    ifelse(isTRUE(flgp == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                      "+"), collapse = "")), NA)
                    ifelse(isTRUE(flga == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                      "*"), collapse = "")), NA)
                  }
                }
                else {
                  tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                    c(" ", "/", " "))) - 2):length(xx)], collapse = ""), 
                    from = "UTF-8", to = "UTF-16LE", toRaw = TRUE), 
                    from = "UTF-16LE", to = "UTF-8")
                  respl <- as.list(paste(c(resp, tmpr), collapse = ""))
                }
            }
            else {
                respl <- as.list(paste(resp, collapse = ""))
                if (isTRUE(level < 2) == TRUE) {
                  ifelse(isTRUE(flgp == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                    "+"), collapse = "")), NA)
                  ifelse(isTRUE(flga == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                    "*"), collapse = "")), NA)
                }
            }
            names(respl) <- x
            return(respl)
        }
    }
    else if (isTRUE(length(x) > 1) == TRUE) {
        if (isTRUE(typeof(x) == "list") == TRUE) {
            if (isTRUE("people" %in% names(x)) == TRUE) 
                warning("\"people\" and list of lists are not yet fully supported")
            x1 <- unlist(x, use.names = FALSE)
        }
        else {
            x1 <- as.vector(x)
        }
        n <- length(x1)
        resl <- vector("list", length = n)
        for (k in seq_len(n)) {
            xi <- x1[k]
            if (isTRUE(level > 1) == TRUE) {
                xi <- gsub("\\s*\\([^\\)]\\)", "", xi)
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "?"))], collapse = "")
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "+"))], collapse = "")
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "*"))], collapse = "")
            }
            else if (isTRUE(level == 1) == TRUE) {
                xi <- gsub("\\s*\\([^\\)]\\)", "", xi)
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "?"))], collapse = "")
            }
            else {
                invisible(NA)
            }
            if (is.na(xi) == TRUE) {
                resl[[k]] <- xi
            }
            else {
                gs2 <- which(suppressWarnings(as.raw(utf8ToInt(xi)) %in% 
                  c("cf", "ce")))
                gs2a <- which(suppressWarnings(as.raw(utf8ToInt(xi)) %in% 
                  c("c2", "c3")))
                gs3 <- which(suppressWarnings(as.raw(utf8ToInt(xi)) %in% 
                  c("e1")))
                if (isTRUE(length(gs3) > 0) == TRUE) {
                  invisible(NA)
                }
                else {
                  gs3 <- NULL
                }
                if (isTRUE(length(c(gs2, gs2a, gs3)) == 0) == 
                  TRUE) {
                  resl[[k]] <- xi
                }
                else {
                  xx <- strsplit(rawToChar(as.raw(utf8ToInt(xi))), 
                    "")[[1]]
                  ifelse(isTRUE(tail(xx, 1) == "+") == TRUE, 
                    flgp <- TRUE, flgp <- FALSE)
                  ifelse(isTRUE(tail(xx, 1) == "*") == TRUE, 
                    flga <- TRUE, flga <- FALSE)
                  gsx <- sort(c(gs2, gs2a, gs3))
                  res <- vector()
                  if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
                    res <- append(res, xx[1:(min(gsx) - 1)])
                  }
                  else {
                    invisible(NA)
                  }
                  for (j in seq_along(gsx)) {
                    i <- gsx[j]
                    if (isTRUE(i %in% c(gs2, gs2a)) == TRUE) {
                      tmp <- paste(xx[i:(i + 1L)], collapse = "")
                      res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                        to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                        to = "UTF-8"))
                      if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                        collapse = ""))) %in% c(c("cf", "ce"), 
                        c("c2", "c3"), c("e1")))) == FALSE && 
                        suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                          0) {
                        if (isTRUE(i < (length(xx) - 1L)) == 
                          TRUE) {
                          ifelse(isTRUE(j == length(gsx)) == 
                            TRUE, res <- append(res, xx[(i + 
                            2L):length(xx)]), res <- append(res, 
                            xx[(i + 2L):(gsx[j + 1L] - 1L)]))
                        }
                        else {
                          invisible(NA)
                        }
                      }
                    }
                    else if (isTRUE(i %in% gs3) == TRUE) {
                      tmp <- paste(xx[i:(i + 2L)], collapse = "")
                      res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                        to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                        to = "UTF-8"))
                      if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                        collapse = ""))) %in% c(c("cf", "ce"), 
                        c("c2", "c3"), c("e1")))) == FALSE && 
                        suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                          0) {
                        if (isTRUE(i < (length(xx) - 1L)) == 
                          TRUE) {
                          ifelse(isTRUE(j == length(gsx)) == 
                            TRUE, res <- append(res, xx[(i + 
                            3L):length(xx)]), res <- append(res, 
                            xx[(i + 3L):(gsx[j + 1L] - 1L)]))
                        }
                        else {
                          invisible(NA)
                        }
                      }
                    }
                  }
                  rm(j)
                  if ("+" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
                    1)]) {
                    res <- append(res, "+")
                  }
                  else if ("*" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
                    1)]) {
                    res <- append(res, "*")
                  }
                  if (isTRUE(length(gs2a) > 0) == TRUE && suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                    collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                    "c3"), c("e1"))) == TRUE)) {
                    xj <- paste(res, collapse = "")
                    gs2p <- which(suppressWarnings(as.raw(utf8ToInt(xj))) %in% 
                      c("cf", "ce"))
                    gs3p <- which(suppressWarnings(as.raw(utf8ToInt(xj))) %in% 
                      c("e1"))
                    if (isTRUE(length(gs3p) > 0) == TRUE) {
                      gs3p <- gs3p[which(gs3p%%2 != 0)]
                    }
                    else {
                      gs3p <- NULL
                    }
                    xxp <- strsplit(xj, "")[[1]]
                    xxpp <- xxp[min(c(gs2p, gs3p)):length(xxp)]
                    x1pp <- paste(xxpp, collapse = "")
                    gs2pp <- which(as.raw(utf8ToInt(x1pp)) %in% 
                      c("cf", "ce"))
                    gs3pp <- which(as.raw(utf8ToInt(x1pp)) %in% 
                      c("e1"))
                    if (isTRUE(length(gs3pp) > 0) == TRUE) {
                      gs3pp <- gs3pp[which(gs3pp%%2 != 0)]
                    }
                    else {
                      gs3pp <- NULL
                    }
                    if (isTRUE(length(c(gs2pp, gs3pp)) > 0) == 
                      TRUE) {
                      x3 <- strsplit(rawToChar(as.raw(utf8ToInt(x1pp))), 
                        "")[[1]]
                      res <- vector()
                      for (i in sort(c(gs2pp, gs3pp))) {
                        if (isTRUE(i %in% c(gs2pp)) == TRUE) {
                          tmp <- paste(x3[i:(i + 1L)], collapse = "")
                          res <- append(res, iconv(iconv(tmp, 
                            from = "UTF-8", to = "UTF-16LE", 
                            toRaw = TRUE), from = "UTF-16LE", 
                            to = "UTF-8"))
                        }
                        else if (isTRUE(i %in% gs3p) == TRUE) {
                          tmp <- paste(x3[i:(i + 2L)], collapse = "")
                          res <- append(res, iconv(iconv(tmp, 
                            from = "UTF-8", to = "UTF-16LE", 
                            toRaw = TRUE), from = "UTF-16LE", 
                            to = "UTF-8"))
                        }
                      }
                      rm(i)
                      if (all(c(" ", "/", " ") %in% strsplit(xi, 
                        "")[[1]]) == TRUE) {
                        if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == 
                          TRUE) {
                          tmpr <- NULL
                          if (isTRUE(flgp == TRUE) == TRUE && 
                            isTRUE(level < 2) == TRUE) {
                            resl[[k]] <- paste(c(tmpr, res, "+"), 
                              collapse = "")
                          }
                          else if (isTRUE(flga == TRUE) == TRUE && 
                            isTRUE(level < 2) == TRUE) {
                            resl[[k]] <- paste(c(tmpr, res, "*"), 
                              collapse = "")
                          }
                          else {
                            resl[[k]] <- paste(c(tmpr, res), 
                              collapse = "")
                          }
                        }
                        else {
                          tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                            c(" ", "/", " "))) - 2):length(xx)], 
                            collapse = ""), from = "UTF-8", to = "UTF-16LE", 
                            toRaw = TRUE), from = "UTF-16LE", 
                            to = "UTF-8")
                          if (isTRUE(flgp == TRUE) == TRUE && 
                            isTRUE(level < 2) == TRUE) {
                            resl[[k]] <- paste(c(res, tmpr, "+"), 
                              collapse = "")
                          }
                          else if (isTRUE(flga == TRUE) == TRUE && 
                            isTRUE(level < 2) == TRUE) {
                            resl[[k]] <- paste(c(res, tmpr, "*"), 
                              collapse = "")
                          }
                          else {
                            resl[[k]] <- paste(c(res, tmpr), 
                              collapse = "")
                          }
                        }
                      }
                      else {
                        if (isTRUE(flgp == TRUE) == TRUE && isTRUE(level < 
                          2) == TRUE) {
                          resl[[k]] <- paste(c(res, "+"), collapse = "")
                        }
                        else if (isTRUE(flga == TRUE && isTRUE(level < 
                          2) == TRUE) == TRUE) {
                          resl[[k]] <- paste(c(res, "*"), collapse = "")
                        }
                        else {
                          resl[[k]] <- paste(res, collapse = "")
                        }
                      }
                    }
                    else {
                      invisible(NA)
                    }
                  }
                  else {
                    if (all(c(" ", "/", " ") %in% strsplit(xi, 
                      "")[[1]]) == TRUE) {
                      if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == 
                        TRUE) {
                        tmpr <- NULL
                        if (isTRUE(flgp == TRUE) == TRUE && isTRUE(level < 
                          2) == TRUE) {
                          resl[[k]] <- paste(c(tmpr, res, "+"), 
                            collapse = "")
                        }
                        else if (isTRUE(flga == TRUE && isTRUE(level < 
                          2) == TRUE) == TRUE) {
                          resl[[k]] <- paste(c(tmpr, res, "*"), 
                            collapse = "")
                        }
                        else {
                          resl[[k]] <- paste(c(tmpr, res), collapse = "")
                        }
                      }
                      else {
                        tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                          c(" ", "/", " "))) - 2):length(xx)], 
                          collapse = ""), from = "UTF-8", to = "UTF-16LE", 
                          toRaw = TRUE), from = "UTF-16LE", to = "UTF-8")
                        resl[[k]] <- paste(c(res, tmpr), collapse = "")
                      }
                    }
                    else {
                      if (isTRUE(flgp == TRUE) == TRUE && isTRUE(level < 
                        2) == TRUE) {
                        resl[[k]] <- paste(c(res, "+"), collapse = "")
                      }
                      else if (isTRUE(flga == TRUE) == TRUE && 
                        isTRUE(level < 2) == TRUE) {
                        resl[[k]] <- paste(c(res, "*"), collapse = "")
                      }
                      else {
                        resl[[k]] <- paste(res, collapse = "")
                      }
                    }
                  }
                }
            }
        }
        names(resl) <- x1
        if (missing(na.rm) == FALSE && isTRUE(na.rm == FALSE) == 
            TRUE) {
            return(resl)
        }
        else {
            return(Filter(function(y) !all(is.na(y)), resl))
        }
    }
}
