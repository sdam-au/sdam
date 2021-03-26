
## 
## FUNCTION dts() to convert date formats into numerical
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.6 (24-03-2021)
##
## PARAMETERS
## x        (scalar or vector,  with character dates)
## cent     (optional and logiucal, centuries?)
## sep      (separator, works only for 'cent')


dts <-
function (x, cent, sep) 
{
    if (is.numeric(x) == TRUE) {
        x
    }
    else {
        if (missing(cent) == FALSE && isTRUE(cent == TRUE) == 
            TRUE) {
            x <- as.vector(x)
            cnt <- which(suppressWarnings(grepl(c("t", "h"), 
                tolower(x))) == TRUE)
            xcnt <- x[cnt]
            tmp0 <- sapply(xcnt, function(z) {
                paste(strsplit(z, "")[[1]][which(!(strsplit(z, 
                  "")[[1]] %in% c("T", "H", "t", "h", " ", "C", 
                  "c", "e", "n", "u", "r", "y", ".", "A", "D", 
                  "a", "d")))], collapse = "")
            })
            tmp <- sapply(tmp0, function(y) {
                z <- suppressWarnings(sapply(y, function(z) as.numeric(paste(strsplit(z, 
                  "")[[1]][which(!(strsplit(z, "")[[1]] %in% 
                  c("?", " ", "B")))], collapse = "")) * (-1L)))
                ifelse(isTRUE(suppressWarnings(as.numeric(y)) > 
                  0) == TRUE, z <- z * (-1L), NA)
                return(z)
            })
            ifelse(missing(sep) == TRUE, sep <- " to ", NA)
            vec <- as.vector(tmp)
            for (k in seq_len(length(vec))) {
                if (isTRUE(any(strsplit(names(tmp), "")[[k]] %in% 
                  c(".", "c")) == TRUE) == TRUE) {
                  vec[k] <- paste(head(seq(to = tmp[k] * 100L, 
                    from = ((tmp[k] - 1L) * 100L) + 1L), 1), 
                    tail(seq(to = tmp[k] * 100L, from = ((tmp[k] - 
                      1L) * 100L) + 1L), 1), sep = sep)
                }
                else {
                  NA
                }
            }
            rm(k)
        }
        else {
            ifelse(isTRUE(length(x) > 1) == TRUE && is.na(suppressWarnings(as.numeric(x))) == 
                FALSE, return(as.numeric(x)), NA)
        }
        ifelse(missing(cent) == FALSE && isTRUE(cent == TRUE) == 
            TRUE, x0 <- x[which(!(seq_along(x) %in% cnt))], x0 <- x)
        if (any(c("E", "e") %in% strsplit(x0, "")[[1]]) == TRUE) {
            xn <- sub("e", "", sub("[.].*", "", sub("or.*", "", 
                x0, ignore.case = TRUE)), ignore.case = TRUE)
        }
        else {
            xn <- sub("or.*", "", x0, ignore.case = TRUE)
        }
        pck <- which(gsub("[^-]", "", xn) %in% "-")
        if (isTRUE(length(pck) > 0) == TRUE) {
            xn[pck] <- gsub("\\s+", "", paste0(sub("[[:alpha:]]+", 
                "", gsub("-.*", "\\1", xn[pck])), " ", gsub("([^A-Za-z])+", 
                "", gsub(".*-", "\\1", xn[pck]))))
        }
        else {
            NA
        }
        xn2 <- toupper(sub("C", "", sub("[.].*", "", xn), ignore.case = TRUE))
        xnd <- sapply(xn2, function(y) {
            tmp <- suppressWarnings(sapply(y, function(z) as.numeric(paste(strsplit(z, 
                "")[[1]][which(!(strsplit(z, "")[[1]] %in% c("?", 
                " ", "B")))], collapse = "")) * (-1L)))
            ifelse(isTRUE(suppressWarnings(as.numeric(y)) > 0) == 
                TRUE, tmp <- tmp * (-1L), NA)
            mmd <- vector(length = length(y))
            mmd[which(!(is.na(tmp)))] <- as.vector(tmp)[which(!(is.na(tmp)))]
            mmd[which(is.na(tmp))] <- suppressWarnings(sapply(y[which(is.na(tmp))], 
                function(z) as.numeric(paste(strsplit(z, "")[[1]][which(!(strsplit(z, 
                  "")[[1]] %in% c("?", " ", "A", "D")))], collapse = "")) * 
                  (+1L)))
            names(mmd) <- NULL
            rm(tmp)
            unlist(mmd)
        })
        if (missing(cent) == FALSE && isTRUE(cent == TRUE) == 
            TRUE) {
            ndts <- vector(length = length(x))
            ndts[cnt] <- vec
            ndts[which(ndts == "FALSE")] <- as.numeric(xnd)
        }
        else {
            ndts <- xnd
        }
        names(ndts) <- x
        return(ndts)
    }
}
