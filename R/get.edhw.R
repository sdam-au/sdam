
## 
## FUNCTION get.edhw() wrapper to get data API from the Epigraphic Database Heidelber
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1 (27-04-2020)
##
## Parameters
## hd_nr  (vector with HD-No of inscriptions)
## ... additional parameters

get.edhw <-
function (hd_nr, ...) 
{
    smpl <- list()
    for (i in hd_nr) {
        smpl[[length(smpl) + 1L]] <- try(get.edh(hd_nr = i, ...))
    }
    return(smpl)
}
