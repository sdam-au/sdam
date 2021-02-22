
## 
## FUNCTION plot.map() to manipulate data API from the EDH dataset
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.2 (19-02-2021)
##
## PARAMETERS
##
## x      (char or vector, province or region acronym)
##
## OPTIONAL PARAMETERS
##
## cap    (optional and logical, display caption?)
## date   (optional and logical, display est date in caption?)
## name   (optional and logical, display title's name?)
## fsize  (optional, title's font size)
## fcol   (optional, title's font color)
##


plot.map <-
function (x, cap, date, name, fsize, fcol, ...) 
{
    if (!(exists("rpmp"))) {
        utils::data("rpmp", package = "sdam", envir = environment())
        rpmp <- get("rpmp", envir = environment())
    }
    else {
        invisible(NA)
    }
    if (isTRUE(x %in% names(rpmp) == FALSE)) {
        message("could not find \"x\"")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
    }
    else {
        NA
    }
    require("grid", "grImport2", quietly = TRUE)
    grid::grid.newpage()
    grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmp", 
        x, sep = "$"), "[[1]]", sep = "")))))
    if (missing(cap) == FALSE && isTRUE(cap == FALSE) == TRUE) {
        invisible(NA)
    }
    else {
        if (!(exists("rpmcd"))) {
            utils::data("rpmcd", package = "sdam", envir = environment())
            rpmcd <- get("rpmcd", envir = environment())
        }
        else {
            invisible(NA)
        }
        grid::pushViewport(grid::viewport(x = 1.165, y = -0.7, 
            w = 1.25, h = 1.25, just = c("center", "bottom")))
        grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmcd", 
            paste("c", x, sep = ""), sep = "$"), "[[1]]", sep = "")))))
        grid::popViewport()
        if (missing(date) == FALSE && isTRUE(date == TRUE) == 
            TRUE) {
            est <- eval(parse(text = paste(paste("rpmcd[[", which(names(rpmcd) %in% 
                paste("c", x, sep = "")), "]]", sep = ""), "provd", 
                sep = "$")))
            grid::grid.text(paste("est.", est), x = 0.68, y = 0.3, 
                gp = grid::gpar(fontsize = 8, col = "#808080"))
        }
        else {
            invisible(NA)
        }
    }
    if (missing(name) == FALSE && isTRUE(name == FALSE) == TRUE) {
        invisible(NA)
    }
    else {
        ifelse(missing(fsize) == TRUE, fsize <- 20, invisible(NA))
        ifelse(missing(fcol) == TRUE, fcol <- "grey", invisible(NA))
        grid::grid.text(eval(parse(text = paste(paste("rpmp[[", 
            which(names(rpmp) %in% x), "]]", sep = ""), "provn", 
            sep = "$"))), x = 0.5, y = 0.966, gp = grid::gpar(fontsize = fsize, 
            col = fcol))
    }
    if (isTRUE(length(x) > 1) == TRUE) {
        for (k in seq_len(length(x) - 1)) {
            grid::grid.newpage()
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmp", 
                x[k], sep = "$"), "[[1]]", sep = "")))))
            if (missing(cap) == FALSE && isTRUE(cap == FALSE) == 
                TRUE) {
                invisible(NA)
            }
            else {
                grid::pushViewport(grid::viewport(x = 1.165, 
                  y = -0.7, w = 1.25, h = 1.25, just = c("center", 
                    "bottom")))
                grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmcd", 
                  paste("c", x[k], sep = ""), sep = "$"), "[[1]]", 
                  sep = "")))))
                grid::popViewport()
                if (missing(date) == FALSE && isTRUE(date == 
                  TRUE) == TRUE) {
                  est <- eval(parse(text = paste(paste("rpmcd[[", 
                    which(names(rpmcd) %in% paste("c", x[k], 
                      sep = "")), "]]", sep = ""), "provd", sep = "$")))
                  grid::grid.text(paste("est.", est), x = 0.68, 
                    y = 0.3, gp = grid::gpar(fontsize = 8, col = "#808080"))
                }
                else {
                  invisible(NA)
                }
            }
            if (missing(name) == FALSE && isTRUE(name == FALSE) == 
                TRUE) {
                invisible(NA)
            }
            else {
                grid::grid.text(eval(parse(text = paste(paste("rpmp[[", 
                  which(names(rpmp) %in% x[k]), "]]", sep = ""), 
                  "provn", sep = "$"))), x = 0.5, y = 0.966, 
                  gp = grid::gpar(fontsize = fsize, col = fcol))
            }
        }
        rm(k)
    }
    else {
        invisible(NA)
    }
}
