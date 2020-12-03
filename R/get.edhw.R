
## 
## WRAPPER FUNCTION get.edhw() to retrieve records from EDH database API
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.2.1 (12-11-2020)
##
## PARAMETERS
## file  (optional file with a JSON format)
## hd_nr (HD-No of inscription)
## ...   (optional parameters)
##


get.edhw <-
function (file = NULL, hd_nr, ...) 
{
    if (is.null(file) == FALSE) {
        smpl <- rjson::fromJSON(file = file)
    }
    else if (is.null(file) == TRUE) {
        smpl <- list()
        for (i in hd_nr) {
            smpl[[length(smpl) + 1L]] <- try(get.edh(hd_nr = i, 
                ...))
        }
        rm(i)
    }
    return(smpl)
}
