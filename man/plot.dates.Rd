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
plot.dates(x, y, type = c("ts", "mp", "rg"), taq, tpq, id, out, 
    col, cex, lwd, lty, pch, main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, axes = TRUE, alpha, file = NULL, ...)
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
  \item{type}{
      Type of date format to plot: 
        
        \code{ts} timespans
        
        \code{mp} mid points and range
        
        \code{rg} only range
}
  \item{taq}{
  \emph{terminus ante quem}
}
  \item{tpq}{
  \emph{terminus post quem}
}
  \item{id}{
  IDs as variable or rownames in dataset \code{x}
}
  \item{out}{
  number of outliers to omit (integer or vector where first entry id for latest date)
}
  \item{col}{
  color of \code{pch}
}
  \item{cex}{
  size of \code{pch}
}
  \item{lwd}{
  width of time interval segments
}
  \item{lty}{
  shape of time interval segments
}
  \item{pch}{
  symbol for \code{taq} and \code{tpq}
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
  \item{alpha}{
  alpha transparency for time interval segments
}
  \item{file}{
  path to file for a PDF format (optional)
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
