\name{get.edhw}
\alias{get.edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper to get data from the Epigraphic Database Heidelberg
}
\description{
A wrapper function to obtain data from the Epigraphic Database Heidelberg repository.
}
\usage{
get.edhw(hd_nr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hd_nr}{
	HD number of inscriptions
}
  \item{\dots}{
	Additional arguments
}
}
\details{
This is a wrapper function to obtain a sample data from the Epigraphic Database Heidelberg repository.

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
# TBD

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
