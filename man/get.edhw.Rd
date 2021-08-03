\name{get.edhw}
\alias{get.edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper to get data from the Epigraphic Database Heidelberg API
}
\description{
A wrapper function to obtain data from the Epigraphic Database Heidelberg repository.
}
\usage{
get.edhw(file = NULL, hd_nr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	JSON file with EDH data (optional)
}
  \item{hd_nr}{
	HD number of inscriptions
}
  \item{\dots}{
	Additional arguments
}
}
\details{
This is a wrapper function to obtain a sample data from the Epigraphic Database Heidelberg API repository 
by their HD numbers or, alternatively, a file with a valid format JSON can be specified in \code{file}. 

In any case, the JSON output will be converted into a list object with the \code{\link[rjson]{rjson}} 
package.
}
\value{
A list of lists object with the items described in \code{\link{get.edh}}.

}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
Large samples can take a lot of time.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{simil}}
}
\examples{
## get 10 records from EDH API data
## Not run:
get.edhw(hd_nr=1:10)
## End(**Not run**)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }
