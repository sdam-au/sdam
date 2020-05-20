get.edhw <-
function (hd_nr, ...) 
{
    smpl <- list()
    for (i in hd_nr) {
        smpl[[length(smpl) + 1L]] <- try(get.edh(hd_nr = i, ...))
    }
    return(smpl)
}
