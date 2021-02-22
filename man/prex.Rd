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
prex(x, taq, tpq, vars, bins = NULL, cp, aoristic = TRUE, weight = 1, 
     DF, out, plot = FALSE, main = NULL, ...)
}

\arguments{
  \item{x}{
  list or data frame object of variables and observations.
}
  \item{taq}{
  \emph{terminus ante quem} (TAQ)
}
  \item{tpq}{
  \emph{terminus post quem} (TPQ)
}
  \item{vars}{
   boundaries of existence in \code{x} (vector for TAQ and TPQ)
}
  \item{bins}{
  length of the break (integer)
}
  \item{cp}{
  chronological phase
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
  \item{out}{
  number of outliers to omit (integer or vector where first entry id for latest date)
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
Currently, the probability of existence is the \emph{aoristic sum} computed accross events for portions of time 
delimited by a TAQ in \code{taq} and TPQ in \code{tpq}, or else by the boundaries of existence in \code{vars}. 

In case that \code{bins} is \code{NULL}, then the time breaks take the chronological periods in \code{cp}, which 
by default is \code{"bin5"} or five-periods for the \code{EDH} dataset. Another built-in option is \code{"bin8"} 
for eight chronological periods, but \code{cp} is open for other periodization models as long as they are recorded 
as a list object. 

When \code{aoristic} is set to \code{FALSE}, then a simple matching of only TAQ and TPQ is computed from \code{x}. 
}
\value{
A data frame with values according to either \code{bins} or \code{cp}.
}
\references{
For aoristic sum: Crema, E. (2012) ``Modelling temporal uncertainty in archaeological analysis'' \emph{J Archaeol Method Theory}.

For default chronological periods: see \emph{Bevan et al}, 2013 (\code{doi: 10.1111/j.1475-4754.2012.00674.x})
}
\author{
Antonio Rivero Ostoic
}
%\note{
%Further measures of probability of existence will be incorporated.
%}

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
