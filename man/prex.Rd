\name{prex}
\alias{prex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute probability of existence
}
\description{
A function to compute the probability of existence of events.
}
\usage{
prex(x, vars, bins = NULL, cp = c("bin5", "bin8"), aoristic = TRUE, weight = 1, DF, 
     plot = FALSE, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  list or data frame object of variables and observations.
}
  \item{vars}{
   boundaries of existence in \code{x} (vector for TAQ and TPQ)
}
  \item{bins}{
  length of the break (integer)
}
  \item{cp}{
  chronological phase.\code{"bin5"} or \code{"bin8"} for five- or eight-periods
}
  \item{aoristic}{
  return aoristic sum? (logical)
}
  \item{weight}{
  weight to observations
}
  \item{DF}{
  return also data frame with observations? Ignored for plot (logical and optional)
}
  \item{plot}{
  plot the results? (logical and optional)
}
  \item{main}{
  plot's main title (optional)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
Currently, the probability of existence is the \emph{aoristic sum} computed accross 
events for a single portion of time. 

When \code{aoristic} is set to \code{FALSE}, then a simple matching of only \emph{terminus ante quem} TAQ 
and \emph{terminus post quem} TPQ is computed. 
}
\value{
A data frame with values according to either \code{bins} or \code{cp}.
}
\references{
For aoristic sum refer to Crema, E. (2012) \emph{INSERT TITLE}
}
\author{
Antonio Rivero Ostoic
}
\note{
Further measures of probability of existence will be incorporated.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edhw}}, \code{\link{plot.dates}}
}
\examples{
#TBD.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ metrics }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
