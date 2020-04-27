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
        authenticate = TRUE, cred = NULL, path = "/files", subdomain = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{\dots}{
extra parameters if required
}
}
\details{
\code{request} is basically a HTTP request, first aimed to interact first with DEiC's \emph{sciencedata.dk}. 

}
\value{
Depends on the method, an action on the server site. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Method POST is not currently supported at \emph{sciencedata.dk}
}

%\section{Warning }{
%
%} ~


%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# TBD

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
