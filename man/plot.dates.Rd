\name{plot.dates}
\alias{plot.dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot interval dates
}
\description{
A function to plot interval dates.
}
\usage{
plot.dates(x, y, file = NULL, taq, tpq, id, out, main = NULL,
           xlab = NULL, ylab = NULL, xlim = NULL, axes = TRUE,
           cex, pch, col, lwd, lty, alpha, ...)
}
%\method{plot.dates}{plot}(x, y, file = NULL, taq, tpq, out, main = NULL, xlab = NULL, ylab = NULL, 
%                     xlim = NULL, cex, pch, col, lwd, lty, alpha, \dots)
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  dataset as a data frame object of variables and observations. 
}
  \item{y}{
  optional identifiers (vector).
}
  \item{file}{
  path to file for a PDF format (optional)
}
  \item{taq}{
  \emph{terminus ante quem}
}
  \item{tpq}{
  \emph{terminus post quem}
}
  \item{id}{
  IDs in dataset \code{x}
}
  \item{out}{
  number of outliers to omit (integer or vector where first entry id for latest date)
}
  \item{main}{
  plot's main tile
}
  \item{xlab}{
  plot's \code{x} label
}
  \item{ylab}{
  plot's \code{y} label
}
  \item{xlim}{
  plot's \code{x} limits
}
  \item{axes}{
  plot's axes (logical)
}
  \item{cex}{
  size of \code{pch}
}
  \item{pch}{
  symbol for \code{taq} and \code{tpq}
}
  \item{col}{
  color of \code{pch}
}
  \item{lwd}{
  width of time interval segments
}
  \item{lty}{
  shape of time interval segments
}
  \item{alpha}{
  alpha transparency for time interval segments
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
This plot function is for time interval segments given in the dataset \code{x}, which is 
given as a data frame or as a ``tibble'' class object.
}
\value{
A graphical plot.
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
\note{
If \code{x} is \code{NULL}, then \code{EDH} dataset is taken.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{edhw}}, \code{\link{prex}}.
}
\examples{
#TBD.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
