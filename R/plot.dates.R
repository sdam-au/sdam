
## 
## PLOT FUNCTION plot.dates() to plot time intervals
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.8 (22-12-2020)
##
## PARAMETERS
## x      (data frame object of variables and observations)
## y      (optional identifiers)
## file   (path to file for a PDF format=
## taq    (terminus ante quem)
## tpq    (terminus post quem)
## out    (number of outliers to omit)
## 
## OPTIONAL PARAMETERS
## main   (main tile)
## xlab   (x label)
## ylab   (y label)
## xlim   (x limit)
## pch    (symbol for taq and tpq)
## cex    (size of pch)
## col    (colors of pch and time interval segment)
## lwd    (width)
## lty    (shape)
## alpha  (alpha color transparency)
## ...    (optional parameters)


plot.dates <-
function (x, y, file = NULL, taq, tpq, out, main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, cex, pch, col, lwd, lty, alpha, 
    ...) 
{
    ifelse(missing(taq) == TRUE, taq <- "not_before", NA)
    ifelse(missing(tpq) == TRUE, tpq <- "not_after", NA)
    ifelse(missing(lwd) == TRUE, lwd <- 1L, NA)
    ifelse(missing(lty) == TRUE, lty <- 1L, NA)
    ifelse(missing(cex) == TRUE, cex <- 1L, NA)
    ifelse(missing(pch) == TRUE, pch <- 20, pch <- pch[1])
    if (missing(col) == TRUE) {
        col <- c("#C0C0C0", "#808080", 8)
    }
    else {
        ifelse(isTRUE(length(col) < 3L) == TRUE, col <- rep(col, 
            3)[1:3], col <- col[1:3])
    }
    ifelse(missing(alpha) == TRUE, alpha <- 0.25, NA)
    ifelse(is.null(xlab) == TRUE, xlab <- "years", NA)
    if (missing(x) == FALSE) {
        ifelse(isTRUE(is.data.frame(x) == TRUE) == TRUE, xdates <- x, 
            xdates <- suppressWarnings(edhw(x = x, vars = c(taq, 
                tpq), as = "df", ...)))
    }
    else {
        stop("'x' is missing.")
    }
    nb <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
        taq)]))
    na <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
        tpq)]))
    if (missing(out) == FALSE) {
        outliert <- c(tail(sort(boxplot(nb, plot = FALSE)$out), 
            out[1]), tail(sort(boxplot(na, plot = FALSE)$out), 
            out[1]))
        if (isTRUE(length(out) > 1) == TRUE) {
            outlierh <- c(head(sort(boxplot(nb, plot = FALSE)$out), 
                out[2]), head(sort(boxplot(na, plot = FALSE)$out), 
                out[2]))
            outliers <- c(outliert, outlierh)
        }
        else {
            outliers <- outliert
        }
        xdates <- xdates[-c(which(nb %in% outliers), which(na %in% 
            outliers)), ]
        nb <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
            taq)]))
        na <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
            tpq)]))
        years <- c(min(nb, na.rm = TRUE), max(na, na.rm = TRUE))
    }
    else {
        years <- c(min(nb, na.rm = TRUE), max(na, na.rm = TRUE))
    }
    ifelse(is.null(xlim) == TRUE, xlim <- years, NA)
    ifelse(missing(y) == FALSE, ID <- y, ID <- as.numeric(sub("[[:alpha:]]+", 
        "", xdates$id)))
    warns = -1
    if (is.null(file) == TRUE) {
        plot(nb, ID, pch = pch, cex = cex, col = col[1], xlab = xlab, 
            ylab = ylab, xlim = xlim, main = main, ...)
        points(na, ID, pch = pch, cex = cex, col = col[2])
        segments(nb, ID, na, ID, lwd = lwd, lty = lty, col = grDevices::adjustcolor(col[3], 
            alpha = alpha))
    }
    else {
        pdf(file)
        plot(nb, ID, pch = pch, cex = cex, col = col[1], xlab = xlab, 
            ylab = ylab, xlim = xlim, main = main, ...)
        points(na, ID, pch = pch, cex = cex, col = col[2])
        segments(nb, ID, na, ID, lwd = lwd, lty = lty, col = grDevices::adjustcolor(col[3], 
            alpha = alpha))
        dev.off()
    }
    warns = 0
}
