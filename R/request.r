## 
## FUNCTION request() to get data API from a given server
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## First aimed to interact with DEiC's sciencedata.dk
## version 0.2.3 (03-08-2020)
##
## Parameters
## file (object under 'method')
## URL (protocol and domain of the url)
## method (the http "verb" for the object)
##        "GET" (list)
##        "POST" (place)
##        "PUT" (update)
##        "DELETE" (cancel)
## authenticate (logical, use basic authentication?)
## path (optional, add path to the url)
## 
## Additional parameters:
## cred (vector for username and password credentials)
## subdomain (optional, add subdomain to the url)
## force (optional, force remote file overwriting)
## ... (extra parameters if required)
##
## Aliases: sddk(), SDDK()


request <-
function (file, URL = "https://sciencedata.dk", method = c("GET", 
    "POST", "PUT", "DELETE"), authenticate = TRUE, cred = NULL, 
    path = "/files", subdomain = NULL, force = FALSE, ...) 
{
    URL0 <- URL
    ifelse(is.null(subdomain) == FALSE, URL <- gsub("//", paste0("//", 
        subdomain, sep = "."), URL), NA)
    ifelse(isTRUE(strsplit(path, "")[[1]][1] != "/") == TRUE, 
        path <- paste0("/", path, sep = ""), NA)
    ifelse(isTRUE(strsplit(path, "")[[1]][length(strsplit(path, 
        "")[[1]])] != "/") == TRUE, path <- paste0(path, "/", 
        sep = ""), NA)
    URL <- paste0(URL, path, "/", sep = "")
    if (isTRUE(authenticate) == TRUE && is.null(cred) == TRUE) {
        getLoginDetails <- function() {
            tt <- tcltk::tktoplevel()
            tcltk::tkwm.title(tt, "login credentials")
            Name <- tcltk::tclVar("username")
            Password <- tcltk::tclVar("password")
            entry.Name <- tcltk::tkentry(tt, width = "25", textvariable = Name)
            entry.Password <- tcltk::tkentry(tt, width = "25", 
                show = "-", textvariable = Password)
            tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter your login details."))
            tcltk::tkgrid(entry.Name)
            tcltk::tkgrid(entry.Password)
            OnOK <- function() {
                tcltk::tkdestroy(tt)
            }
            OK.but <- tcltk::tkbutton(tt, text = " OK ", command = OnOK)
            tcltk::tkbind(entry.Password, "<Return>", OnOK)
            tcltk::tkgrid(OK.but)
            tcltk::tkfocus(tt)
            tcltk::tkwait.window(tt)
            invisible(c(loginID = tcltk::tclvalue(Name), password = tcltk::tclvalue(Password)))
        }
        cred <- getLoginDetails()
    }
    else {
        cred <- cred
    }
    if (match.arg(method) == "GET") {
        ifelse(is.null(cred) == TRUE, resp <- httr::GET(paste0(URL, 
            file)), resp <- httr::GET(paste0(URL, file), httr::authenticate(as.vector(cred[1]), 
            as.vector(cred[2]))))
        return(noquote(httr::content(resp, "text")))
    }
    else if (match.arg(method) == "PUT") {
        FILE <- httr::upload_file(file)
        if (isTRUE(force == TRUE) == TRUE) {
            if (is.null(cred) == TRUE) {
                httr::PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                  "/")[[1]])]))
            }
            else {
                httr::PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                  "/")[[1]])]), httr::authenticate(as.vector(cred[1]), 
                  as.vector(cred[2])), body = FILE, httr::config(followlocation = 0L))
            }
        }
        else {
            ifelse(is.null(cred) == TRUE, status <- httr::HEAD(paste0(URL, 
                strsplit(file, "/")[[1]][length(strsplit(file, 
                  "/")[[1]])])), status <- httr::HEAD(paste0(URL, 
                strsplit(file, "/")[[1]][length(strsplit(file, 
                  "/")[[1]])]), httr::authenticate(as.vector(cred[1]), 
                as.vector(cred[2])), httr::config(followlocation = 0L)))
            flg <- 0
            if (isTRUE(substring(status$status_code, 1, 1) == 
                "2") == TRUE) {
                ans <- readline("File name already exists in remote directory. Do you want to overwrite it? (y/n) \n ")
                ifelse(substr(ans, 1, 1) == "y", flg <- 1, flg <- 2)
            }
            if (isTRUE(flg == 1) == TRUE) {
                if (is.null(cred) == TRUE) {
                  httr::PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                    "/")[[1]])]))
                }
                else {
                  httr::PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                    "/")[[1]])]), httr::authenticate(as.vector(cred[1]), 
                    as.vector(cred[2])), body = FILE, httr::config(followlocation = 0L))
                }
            }
            else if (isTRUE(flg == 2) == TRUE) {
                cat("File not overwrited.\n")
            }
            else {
                NA
            }
        }
    }
    else if (match.arg(method) == "POST") {
        if (isTRUE(URL0 == "https://sciencedata.dk") == TRUE) 
            stop("Method \"POST\" is not yet implemented in \"sciencedata.dk\"")
        FILE <- httr::upload_file(file)
        if (is.null(cred) == TRUE) {
            httr::POST(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                "/")[[1]])]))
        }
        else {
            httr::POST(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                "/")[[1]])]), httr::authenticate(as.vector(cred[1]), 
                as.vector(cred[2])), body = FILE, httr::config(followlocation = 0L))
        }
    }
    else if (match.arg(method) == "DELETE") {
        ifelse(is.null(cred) == TRUE, httr::DELETE(paste0(URL, 
            strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])])), 
            httr::DELETE(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                "/")[[1]])]), httr::authenticate(as.vector(cred[1]), 
                as.vector(cred[2])), httr::add_headers(Accept = "")))
    }
}
SDDK <- sddk <- request
request <- SDDK
