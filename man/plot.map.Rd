\name{plot.map}
\alias{plot.map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot cartographical map
}
\description{
A function to plot a cartographical map, initially of a Roman province or an Italian region.
}
\usage{
plot.map(x = NULL, type = c("plain", "rp", "si", "tetra", "med"), settl, roads, shipr, 
         main, cap, date, name, fsize, fcol, fsize2, fcol2, xd, yd, new, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
(character) acronym of ancient Roman province or Italian region (see \code{\link{get.edh}})
}
  \item{type}{
      Type of cartographical map: 
        
        \code{plain}
        
        \code{rp} Roman provinces
        
        \code{si} Senatorial-Imperial
        
        \code{tetra} Tetrarchy
        
        \code{med} Mediterranean sea
}
  \item{settl}{
  (logical for cartographical map) Display settlements?
}
  \item{roads}{
  (logical for cartographical map) Display roads?
}
  \item{shipr}{
  (logical for cartographical map) Display shipping routes?
}
  \item{main}{
  (optional for cartographical map) Plot's main title
}
  \item{cap}{
(logical for provinces) display caption?
}
  \item{date}{
(logical for provinces) display date?
}
  \item{name}{
(logical for provinces) display province title name?
}
  \item{fsize}{
(optional) font size in main title
}
  \item{fcol}{
(optional) font color in main title
}
  \item{fsize2}{
(optional) font size in date
}
  \item{fcol2}{
(optional) font color in date
}
  \item{xd}{
(optional) \emph{x} positioning for date
}
  \item{yd}{
(optional) \emph{y} positioning for date
}
  \item{new}{
(optional) whether or not plotted map has superimposed graphics
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
This plot function is for creating cartographical maps of ancient provinces and Italian regions of the Roman Empire around year 117 AD. 
The input data \code{x} can be a character vector, but this is intended for a recording output. 
By default, arguments \code{name} and \code{cap} are set to \code{TRUE} while \code{date} is set to \code{FALSE}. 

The \code{type} argument allows plotting cartographical maps related to the Roman Empire and Mediterranean sea. 
}
\value{
A cartographical map plot with a name, and a caption with an approximate province establishment date.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Positions for caption and date are fixed for a PDF output. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rpmp}}, \code{\link{rpmcd}}, \code{\link{get.edh}}, \code{\link{rp}}, \code{\link{EDH}}
}
\examples{
#TBD.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
