\name{request}
\alias{request}
\alias{ssdk}
\alias{SSDK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform an HTTP request
}
\description{
A function to perform an HTTP request
}
\usage{
request(file, URL = "https://sciencedata.dk", method = c("GET", "POST", "PUT", "DELETE"), 
        authenticate = TRUE, cred = NULL, path = "/files", subdomain = NULL, force = FALSE, 
        rm.file, ...)
}

\arguments{
  \item{file}{
The request file
}
  \item{URL}{
protocol and domain of the url
}
  \item{method}{
the http \emph{verb} for the object
}
  \item{authenticate}{
use basic authentication? (logical) 
}
  \item{cred}{
username and password credentials (vector) 
}
  \item{path}{
path to add to the url (optional)
}
  \item{subdomain}{
subdomain to add to the url (optional)
}
  \item{force}{
force remote file overwriting? (optional)
}
  \item{rm.file}{
remove file in local machine? (optional and logical)
}
  \item{\dots}{
extra parameters if required
}
}
\details{
\code{request} is basically a HTTP request, first aimed to interact with DEiC's (Danish e-Infrastructure Cooperation) \emph{\url{https://sciencedata.dk}}. 
However, it is possible to specify the URL path and subdomain if necessary. 

There are two types of folders in DEiC's \emph{\url{https://sciencedata.dk}} that are \emph{personal} and \emph{shared} folders and both requires authentication with credentials.

The \emph{path} to the shared folders where the files are located must be specified with the \code{path} argument. However, for personal folders is the \code{file} argument that includes the path information. Many times, DEiC's \emph{\url{https://sciencedata.dk}} places the data on a \emph{subdomain}, and for some methods like \code{PUT} it is required to specify the subdomain as well.

When a file already exists on the remote server, there is a prompt question for overwriting the file when the \code{PUT} method is invoked, and by activating argument \code{force} we can prevent confirmation and replace the file.

In case that accessing the server requires basic authentification, then package "\code{\link{tcltk}}" may be needed as well to input the credentials with a widget prompt, and there is the \code{cred} argument for performing a basic authentification without a prompt.
}
\value{
Depends on the method, an action on the server site. 
A \emph{Response} message is returned when the method is \code{PUT} with the url and items \code{Date}, \code{Status}, \code{Content-Type}.

Method \code{POST} is not currently supported at \emph{sciencedata.dk}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Aliases for this function are \code{sddk()} and \code{SDDK()}.
}

%\section{Warning }{
%
%} ~


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\url{https://sciencedata.dk}

\url{https://mplex.github.io/cedhar/Sciencedata_dk.html}
}

\examples{
## get a file from remote server
\dontrun{
request("filename.extension", method="GET")}

## put a file in remote server
\dontrun{
sddk("filename.extension", method="PUT")}

## put an existing file in remote server and force overwriting
\dontrun{
sddk("filename.extension", method="PUT", force=TRUE)}

## put an existing file in remote server and remove file from local machine
\dontrun{
sddk("filename.extension", method="PUT", rm.file=TRUE)}


## remove a file in remote server
\dontrun{
SDDK("filename.extension", method="DELETE")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
