
## 
## FUNCTION prex() to compute probability of existence ot time events
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.5 (03-12-2020)
##
## PARAMETERS
## x        (list or data frame object from EDH database)
## bins     (bin periods)
## cp       (chronological periods)
## aoristic (aoristic sum)
## weight   (weight to observations)
## DF       (data frame in output?)
## plot     (plot results?)
## main     (plot's main title)


prex <-
function (x, vars, bins = NULL, cp = c("bin5", "bin8"), aoristic = TRUE, 
    weight = 1, DF, plot = FALSE, main = NULL, ...) 
{
    if (missing(vars) == TRUE) {
        if (all(c("not_before", "not_after") %in% colnames(x)) == 
            TRUE) {
            vars <- c("not_before", "not_after")
        }
        else {
            stop("'vars' needs to be specified.")
        }
    }
    else {
        ifelse(isTRUE(length(vars) == 1L) == TRUE, stop("'vars' needs two values."), 
            vars <- vars[1:2])
    }
    ifelse(is.data.frame(x) == TRUE, xdf <- as.data.frame(x), 
        xdf <- suppressWarnings(edhw(x = x, vars = vars, as = "df", 
            ...)))
    flgb <- TRUE
    if (is.null(bins) == TRUE) {
        flgb <- FALSE
        if (match.arg(cp) == "bin8") {
            bins <- list(Arch = rev(seq(from = -500, to = -700)), 
                Class = rev(seq(from = -325, to = -500)), Hell = rev(seq(from = -325, 
                  to = 0)), ERom = rev(seq(from = 0, to = 200)), 
                MRom = rev(seq(from = 200, to = 350)), LRom = rev(seq(from = 350, 
                  to = 650)), EByz = rev(seq(from = 650, to = 900)), 
                LByz = rev(seq(from = 900, to = 1200)))
        }
        else if (match.arg(cp) == "bin5") {
            bins <- list(Arch = rev(seq(from = -500, to = -700)), 
                Class = rev(seq(from = -325, to = -500)), Hell = rev(seq(from = -325, 
                  to = 0)), Rom = rev(seq(from = 0, to = 650)), 
                Byz = rev(seq(from = 650, to = 1200)))
        }
        else {
            stop("only chronological periods 'bin5' and 'bin8' supported.")
        }
    }
    else if (is.numeric(bins) == TRUE && isTRUE(length(bins) == 
        1L) == TRUE) {
        if (isTRUE(bins > 1000L) == TRUE) {
            bins <- 1000L
            warning("'bins' value too large, set to 1000.")
        }
        else {
            NA
        }
        pbins <- pretty(min(as.numeric(as.vector(xdf[, which(colnames(xdf) == 
            vars[1])])), na.rm = TRUE):max(as.numeric(as.vector(xdf[, 
            which(colnames(xdf) == vars[2])])), na.rm = TRUE))
        brks <- seq(from = range(pbins)[1], to = range(pbins)[2], 
            by = bins)
        ifelse(isTRUE(max(brks) < max(pbins)) == TRUE, brks <- append(brks, 
            max(brks) + bins), NA)
        obin <- bins
        bins <- list()
        for (i in seq_len(length(brks) - 1L)) {
            bins[[i]] <- rev(seq(from = brks[i + 1L], to = brks[i]))
        }
        rm(i)
    }
    else {
        NA
    }
    breaks <- vector()
    for (k in seq_len(length(bins))) {
        breaks <- append(breaks, min(bins[[k]]))
    }
    rm(k)
    breaks <- append(breaks, max(bins[[length(bins)]]))
    if (isTRUE(flgb == TRUE) == TRUE) {
        lbs <- numeric(0)
        for (i in 1:(length(breaks) - 1)) lbs[i] <- paste(breaks[i], 
            breaks[i + 1], sep = " to ")
        names(bins) <- lbs
    }
    else {
        NA
    }
    taq <- as.numeric(as.vector(xdf[, which(colnames(xdf) == 
        vars[1])]))
    tpq <- as.numeric(as.vector(xdf[, which(colnames(xdf) == 
        vars[2])]))
    dur <- vector("list", length = nrow(xdf))
    ifelse(isTRUE("id" %in% colnames(xdf)) == FALSE, NA, names(dur) <- unlist(xdf$id))
    for (i in seq_len(nrow(xdf))) {
        if (is.na(tpq[i]) == TRUE || is.na(taq[i]) == TRUE) {
            dur[[i]] <- 1
        }
        else {
            dur[[i]] <- tpq[i] - taq[i]
        }
    }
    rm(i)
    pertaq <- vector("list", length = length(bins))
    pertpq <- vector("list", length = length(bins))
    names(pertaq) <- names(pertpq) <- names(bins)
    for (k in seq_len(length(bins))) {
        ifelse(isTRUE(length(which(taq %in% (bins[[k]] - 1L))) > 
            0) == TRUE, pertaq[[k]] <- which(taq %in% (bins[[k]] - 
            1L)), NA)
        ifelse(isTRUE(length(which(tpq %in% (bins[[k]] + 1L))) > 
            0) == TRUE, pertpq[[k]] <- which(tpq %in% (bins[[k]] + 
            1L)), NA)
    }
    rm(k)
    if (isTRUE(aoristic == TRUE) == TRUE) {
        pertmq <- vector("list", length = length(bins))
        names(pertmq) <- names(bins)
        for (i in seq_len(length(taq))) {
            if (is.na(taq[i]) == TRUE) {
                tmpmq <- NULL
            }
            else {
                tmpmq <- which(breaks %in% seq(taq[i], taq[i] + 
                  (unlist(dur)[i] - 1L)))
            }
            for (k in tmpmq) {
                tmpm <- append(pertmq[[k]], i)
                pertmq[[k]] <- tmpm
            }
            rm(k)
        }
        rm(i)
        pertq <- lapply(mapply(c, pertaq, pertmq, pertpq, SIMPLIFY = FALSE), 
            unique)
        wpu <- weight/unlist(dur)
        wpu[which(wpu == Inf)] <- 0
        xaor <- data.frame(matrix(nrow = nrow(xdf), ncol = length(bins)))
        ifelse(isTRUE(flgb == TRUE) == TRUE, colnames(xaor) <- lbs, 
            colnames(xaor) <- names(bins))
        for (k in seq_len(length(pertq))) {
            slc <- pertq[[k]]
            if (isTRUE(aoristic == TRUE) == TRUE && isTRUE(flgb == 
                FALSE) == TRUE) {
                xaor[slc, k] <- length(bins[[k]]) * wpu[slc]
            }
            else {
                xaor[slc, k] <- obin * wpu[slc]
            }
        }
        rm(k)
        prxs <- colSums(xaor, na.rm = TRUE)
    }
    else {
        prxa <- vector("list", length = length(bins))
        names(prxa) <- names(bins)
        for (k in seq_len(length(bins))) {
            if (is.null(pertaq[[k]]) == FALSE) {
                for (i in seq_len(length(pertaq[[k]]))) {
                  if (isTRUE(dur[pertaq[[k]][i]] < breaks[k + 
                    1] - taq[pertaq[[k]][i]]) == TRUE) {
                    tmpr <- append(prxa[[k]], 1)
                  }
                  else {
                    tmpr <- append(prxa[[k]], (breaks[k + 1] - 
                      taq[pertaq[[k]][i]])/as.numeric(dur[[pertaq[[k]][i]]]))
                  }
                  prxa[[k]] <- tmpr
                }
                rm(i)
            }
            else {
                NA
            }
        }
        rm(k)
        prxp <- vector("list", length = length(bins))
        names(prxp) <- names(bins)
        for (k in seq_len(length(bins))) {
            if (is.null(pertpq[[k]]) == FALSE) {
                for (i in seq_len(length(pertpq[[k]]))) {
                  if (isTRUE(dur[pertpq[[k]][i]] < tpq[pertpq[[k]][i]] - 
                    breaks[k]) == TRUE) {
                    tmpr <- append(prxp[[k]], 1)
                  }
                  else {
                    tmpr <- append(prxp[[k]], (tpq[pertpq[[k]][i]] - 
                      breaks[k])/as.numeric(dur[pertpq[[k]][i]]))
                  }
                  prxp[[k]] <- tmpr
                }
                rm(i)
            }
            else {
                NA
            }
        }
        rm(k)
        prxs <- mapply(c, prxa, prxp, SIMPLIFY = FALSE)
    }
    if (isTRUE(plot == FALSE) == TRUE) {
        if (missing(DF) == FALSE && isTRUE(DF == TRUE) == TRUE) {
            return(list(data_frame = xdf, prxs = unlist(prxs, 
                use.names = TRUE)))
        }
        else {
            return(unlist(prxs, use.names = TRUE))
        }
    }
    else {
        graphics::barplot(unlist(prxs, use.names = TRUE), main = main)
    }
}
