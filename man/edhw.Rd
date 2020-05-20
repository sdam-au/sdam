\name{edhw}
\alias{edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Epigraphic Database Heidelberg wrapper function
}
\description{
A function to obtain variable data from the \code{EDH} data set and perform transformations.
}
\usage{
edhw(vars, as = c("list", "df"), limit, id, na.rm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
Chosen variables from the \code{EDH} data set (vector)
}
  \item{as}{
Format to return the output. Currently either as a \code{list} or a data frame \code{df} object.
}
  \item{limit}{
Limit the returned output. Ignored if \code{id} is specified (optional, integer or vector)
}
  \item{id}{
Select only the \code{hd_nr} id(s) (optional, integer or character)
}
  \item{na.rm}{
Remove entries with NA data? (logical and optional)
}
  \item{\dots}{
Optional arguments if needed.
}
}
\details{
This is a convenient function to "extract" information from the \code{EDH} dataset attached to this 
package through a vector of chosen \emph{variables}. By default, a list object is returned but it is 
possible to convert the list into a data frame, and if desired remove missing data in the output format. 

Arguments \code{id} and \code{limit} serve to reduce the returned output either to some Epigraphic 
Database number or numbers, which are specified by \code{hd_nr}, or else by limiting the amount of 
the returned output. This latter is like the \code{limit} argument of function \code{get.edh()}, where 
the offset can be specified as well. 
}
\value{
A list or a data frame 
}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
Argument \code{"id"} is for refering to precisely \code{hd_nr}s, while \code{"limit"} is a faster way to 
get to these entries in the \code{EDH} dataset.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{get.edhw}}
}
\examples{
# TBD

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
