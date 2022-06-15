\name{simil}
\alias{simil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity between (column) vectors
}
\description{
A function to compute similarity between vectors, which can arise from columns in a data frame or list entries.
}
\usage{
simil(x, vars, uniq, diag.incl, dichot, rm.isol, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	A list or a data frame object
}
  \item{vars}{
	Column(s) in \code{x} representing variable attributes (vector)
}
  \item{uniq}{
	unique entries? (optional and logical)
}
  \item{diag.incl}{
	include entries in matrix diagonal? (optional and logical) 
}
  \item{dichot}{
	dichotomize output? (optional and logical) 
}
  \item{rm.isol}{
	remove isolates in output? (optional and logical) 
}
  \item{k}{
	cut-off for dichotomization. If not specified, \code{max} of output.
}
}
\details{
At this point, the ID column in the input represents the labels of the nodes. 
In case that an ID column does not exists, then the first column is taken provided that there are not duplicated entry names. 
}
\value{
A valued matrix with similarities among units by simple matching.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Other similarity measures will be added in the near future.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link[multiplex:dichot]{dichot}}, \code{\link[multiplex:rm.isol]{rm.isol}}
}
\examples{
# TBD 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ metrics }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }