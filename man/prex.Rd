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
prex(x, type = c("aoristic", "mp", "other"), taq, tpq, vars, 
     bins = NULL, cp, weight = 1, DF, out, plot = FALSE, main = NULL, 
     ylim, keep, ...)
}

\arguments{
  \item{x}{
  list or data frame object of variables and observations.
}
  \item{type}{
      Type of date format to plot: 
        
        \code{aoristic} aoristic sum
        
        \code{mp} mid points and range
        
        \code{other} 
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
  \item{ylim}{
  limit in \eqn{y}-axis (optional, only with plot)
}
  \item{keep}{
  for \code{mp}, keep variables in output? (optional)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
Currently, the probability of existence is the \emph{aoristic sum} computed across events for portions of time 
delimited by a TAQ in \code{taq} and TPQ in \code{tpq}, or else by the boundaries of existence in \code{vars}. 

In case that \code{bins} is \code{NULL}, then the time breaks take the chronological periods in \code{cp}, which 
by default is \code{"bin5"} or five-periods for the \code{EDH} dataset. Another built-in option is \code{"bin8"} 
for eight chronological periods, but \code{cp} is open for other periodization models as long as they are recorded 
as a list object. 

When \code{aoristic} is set to \code{FALSE}, then a simple matching of only TAQ and TPQ is computed from \code{x}. 
}
\value{
A data frame with values according to either \code{bins} or \code{cp}. 
If \code{plot}, a bar plot with bars of outcomes by \code{bins} or \code{cp}.
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
\keyword{ datagen }%
\keyword{ metrics }% use one of  RShowDoc("KEYWORDS")
