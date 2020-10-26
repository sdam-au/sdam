
## 
## FUNCTION get.edh() to get data API from the Epigraphic Database Heidelber with R
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.2.6 (26-10-2020)
##
## Parameter description from https://edh-www.adw.uni-heidelberg.de/data/api
##
## search (whether to search in "inscriptions" or in "geography")

## SEARCH PARAMETERS FOR INSCRIPTIONS AND GEOGRAPHY:
## province (get list of valid values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/province, case insensitive)
## country (get list of valid values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/country, case insensitive)
## findspot_modern (add leading and/or trailing truncation by asterisk *, e.g. findspot_modern=köln*, case insensitive)
## findspot_ancient (add leading and/or trailing truncation by asterisk *, e.g. findspot_ancient=aquae*, case insensitive)
## bbox (bounding box in the format bbox=minLong , minLat , maxLong , maxLat , example: https://edh-www.adw.uni-heidelberg.de/data/api/inscriptions/search?bbox=11,47,12,48)
## offset (which row to start retrievening data)
## limit (limit the number of results)
##
## SEARCH PARAMETERS FOR INSCRIPTIONS:
## hd_nr (HD-No of inscription)
## year_not_before (integer, BC years are negative integers)
## year_not_after (integer, BC years are negative integers)
## tm_nr (integer value)
## transcription (automatic leading & trailing truncation, brackets are ignored)
## type (of inscription, get list of values at https://edh-www.adw.uni-heidelberg.de/data/api/terms/type, case insensitive)
##
## SEARCH PARAMETERS FOR GEOGRAPHY:
## findspot (level of village, street etc.; add leading and/or trailing truncation by asterisk *, e.g. findspot_modern=köln*, case insensitive)
## pleiades_id (Pleiades identifier of a place; integer value)
## geonames_id (Geonames identifier of a place; integer value)
## 
## ADDITIONAL PARAMETERS:
## addID (whether or not add numeric ID to the list)
## printQ (also print query?)


get.edh <-
function (search = c("inscriptions", "geography"), url = "https://edh-www.adw.uni-heidelberg.de/data/api", 
    hd_nr, province, country, findspot_modern, findspot_ancient, 
    year_not_before, year_not_after, tm_nr, transcription, type, 
    bbox, findspot, pleiades_id, geonames_id, offset, limit, 
    addID, printQ) 
{
    ifelse(missing(hd_nr) == FALSE, NA, hd_nr <- "")
    ifelse(missing(province) == FALSE, NA, province <- "")
    ifelse(missing(country) == FALSE, NA, country <- "")
    ifelse(missing(findspot_modern) == FALSE, NA, findspot_modern <- "")
    ifelse(missing(findspot_ancient) == FALSE, NA, findspot_ancient <- "")
    ifelse(missing(year_not_before) == FALSE, NA, year_not_before <- "")
    ifelse(missing(year_not_after) == FALSE, NA, year_not_after <- "")
    ifelse(missing(tm_nr) == FALSE, NA, tm_nr <- "")
    ifelse(missing(transcription) == FALSE, NA, transcription <- "")
    ifelse(missing(type) == FALSE, NA, type <- "")
    ifelse(missing(bbox) == FALSE, NA, bbox <- "")
    ifelse(missing(province) == FALSE, NA, province <- "")
    ifelse(missing(country) == FALSE, NA, country <- "")
    ifelse(missing(findspot_modern) == FALSE, NA, findspot_modern <- "")
    ifelse(missing(findspot_ancient) == FALSE, NA, findspot_ancient <- "")
    ifelse(missing(year_not_before) == FALSE, NA, year_not_before <- "")
    ifelse(missing(year_not_after) == FALSE, NA, year_not_after <- "")
    ifelse(missing(tm_nr) == FALSE, NA, tm_nr <- "")
    ifelse(missing(transcription) == FALSE, NA, transcription <- "")
    ifelse(missing(type) == FALSE, NA, type <- "")
    ifelse(missing(bbox) == FALSE, NA, bbox <- "")
    ifelse(missing(findspot) == FALSE, NA, findspot <- "")
    ifelse(missing(pleiades_id) == FALSE, NA, pleiades_id <- "")
    ifelse(missing(geonames_id) == FALSE, NA, geonames_id <- "")
    ifelse(missing(offset) == FALSE, NA, offset <- "")
    ifelse(missing(limit) == FALSE, NA, limit <- "")
    ifelse(missing(addID) == FALSE && isTRUE(addID == TRUE) == 
        FALSE, addID <- FALSE, addID <- TRUE)
    ifelse(missing(printQ) == FALSE && isTRUE(printQ == TRUE) == 
        TRUE, printQ <- TRUE, printQ <- FALSE)
    if (match.arg(search) == "inscriptions") {
        URL <- paste(url, match.arg(search), "search?", sep = "/")
        string <- paste(URL, "hd_nr=", hd_nr, "&", "province=", 
            province, "&", "country=", country, "&", "findspot_modern=", 
            findspot_modern, "&", "findspot_ancient=", findspot_ancient, 
            "&", "year_not_before=", year_not_before, "&", "year_not_after=", 
            year_not_after, "&", "tm_nr=", tm_nr, "&", "transcription=", 
            transcription, "&", "type=", type, "&", "bbox=", 
            bbox, "&", "offset=", offset, "&", "limit=", limit, 
            sep = "")
    }
    else if (match.arg(search) == "geography") {
        URL <- paste(url, match.arg(search), "search?", sep = "/")
        string <- paste0(URL, "province=", "%22", province, "%22", 
            "&", "country=", country, "&", "findspot_modern=", 
            findspot_modern, "&", "findspot_ancient=", findspot_ancient, 
            "&", "findspot=", findspot, "&", "bbox=", bbox, "&", 
            "pleiades_id=", pleiades_id, "&", "geonames_id=", 
            geonames_id, sep = "")
    }
    else {
        stop("Only \"inscriptions\" and \"geography\" parameters are currently supported.")
    }
    sst <- strsplit(string, "[&]")[[1]]
    if (isTRUE(substr(sst[1], nchar(sst[1]), nchar(sst[1])) == 
        "=") == FALSE && isTRUE(substr(sst[1], nchar(sst[1]) - 
        5, nchar(sst[1])) == "%22%22") == FALSE) {
        nstring <- sst[1]
    }
    else {
        nstring <- paste0(strsplit(sst[1], "[?]")[[1]][1], "?", 
            sep = "")
    }
    if (any(sst == "limit=") == TRUE) {
        total <- rjson::fromJSON(file = sub("=$", "=1", string))$total
        if (isTRUE(total > 4000) == TRUE && any(sst == "offset=") == 
            TRUE) {
            timeout <- paste("Total number of records is", total, 
                "and only 4000 records are returned.", "Use offset=4000 to complete the query.", 
                sep = " ")
            warning(timeout)
            total <- 4000
        }
        else {
            NA
        }
        ifelse(isTRUE(total == 1L) == TRUE, NA, sst[which(sst == 
            "limit=")] <- paste0("limit=", total, sep = ""))
        rm(total)
    }
    else {
        NA
    }
    for (i in seq(2, length(sst))) {
        if (isTRUE(substr(sst[i], nchar(sst[i]), nchar(sst[i])) == 
            "=") == FALSE && isTRUE(substr(sst[i], nchar(sst[i]) - 
            5, nchar(sst[i])) == "%22%22") == FALSE) {
            nstring <- paste0(nstring, sst[i], sep = "&")
        }
        else {
            NA
        }
    }
    rm(i)
    if (isTRUE(printQ == TRUE) == TRUE) {
        print(sub("&$", "", nstring))
    }
    raw.dat <- rjson::fromJSON(file = sub("&$", "", nstring))
    dat <- raw.dat$items
    if (isTRUE((raw.dat$total) == 0L) == TRUE || isTRUE(length(raw.dat) == 
        0L) == TRUE) {
        if (isTRUE(addID == TRUE) == TRUE & (match.arg(search) == 
            "inscriptions" && isTRUE(length(hd_nr) > 0L)) == 
            TRUE) {
            dat$ID <- hd_nr
            return(dat)
        }
        else {
            return(NULL)
        }
    }
    else {
        if (isTRUE(addID == TRUE) == TRUE) {
            switch(match.arg(search), inscriptions = {
                path <- ".*inschrift/HD"
            }, geography = {
                path <- ".*geographie/"
            })
        }
        else {
            NA
        }
        if (isTRUE(length(dat) == 1L) == TRUE) {
            dat[[1]] <- dat[[1]][order(names(dat[[1]]))]
            ifelse(isTRUE(addID == TRUE) == TRUE, return(c(ID = sub(path, 
                "", dat[[1]]$uri), dat[[1]])), return(dat[[1]]))
        }
        else {
            dato <- list()
            length(dato) <- length(dat)
            for (k in seq_len(length(dat))) {
                dato[[k]] <- dat[[k]][order(names(dat[[k]]))]
            }
            rm(k)
            if (isTRUE(addID == TRUE) == TRUE) {
                daton <- dato
                for (n in seq_len(length(dato))) {
                  daton[[n]] <- c(ID = sub(path, "", dato[[n]]$uri), 
                    dato[[n]])
                }
                return(daton)
            }
            else {
                return(dato)
            }
        }
    }
}
