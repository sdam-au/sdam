\name{plot.map}
\alias{plot.map}
\title{
Plot cartographical maps
}
\description{
A function to plot cartographical maps of the Roman world and Mediterranean region.
}
\usage{
\method{plot}{map}(x = NULL, type = c("plain", "rp", "si", "tetra", "med"), settl, roads, shipr, 
         main, cap, date, name, fsize, fcol, fsize2, fcol2, xd, yd, new, \dots)
%plot.map(x = NULL, type = c("plain", "rp", "si", "tetra", "med"), settl, roads, shipr, 
%         main, cap, date, name, fsize, fcol, fsize2, fcol2, xd, yd, new, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 acronym of ancient Roman province or Italian region (see "\code{\link{rp}}")
}
  \item{type}{
      Type of cartographical map: 
        
        \code{plain} most of Europe and land around the Mediterranean
        
        \code{rp} ancient Roman provinces
        
        \code{si} Senatorial-Imperial provinces
        
        \code{tetra} First Tetrarchy
        
        \code{med} Mediterranean region
}
  \item{settl}{
  display settlements? (optional and logical, for cartographical map)
}
  \item{roads}{
  display terrestrial routes? (optional and logical, for cartographical map)
}
  \item{shipr}{
  display shipping routes? (optional and logical, for cartographical map)
}
  \item{main}{
  plot's main title (optional)
}
  \item{cap}{
 display caption? (optional and logical, for provinces)
}
  \item{date}{
 display date? (optional and logical, for provinces)
}
  \item{name}{
 display map title name? (optional and logical, for provinces)
}
  \item{fsize}{
 font size in main title (optional)
}
  \item{fcol}{
 font color in main title (optional)
}
  \item{fsize2}{
 font size in date (optional)
}
  \item{fcol2}{
 font color in date (optional)
}
  \item{xd}{
 \emph{x} positioning for the date (optional)
}
  \item{yd}{
 \emph{y} positioning for the date (optional)
}
  \item{new}{
 whether the plotted map has superimposed graphics (optional)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
This plot function is for creating cartographical maps of ancient provinces and Italian regions of the Roman Empire around the year AD 117. 
The input data \code{x} can be a character vector, but this is intended for a recording output. 
By default, the argument's \code{name} and \code{cap} are set to \code{TRUE} while the \code{date} is set to \code{FALSE}; 
however, the argument \code{main} prevails over \code{name}.


The \code{type} argument allows plotting cartographical maps related to the Roman Empire and the Mediterranean basin 
as specified in \code{\link{rpmp}} and \code{\link{rpmcd}} datasets. 
In the cartographical maps, settlements are displayed as circles while squares are for military forts, while terrestrial and maritime routes are given as solid paths with different colours. 
Shapes of places and routes are specified in the \code{\link{retn}} dataset.
}
\value{
A plot of a cartographical map for the Roman world with a title name, and a caption with an approximate province establishment date.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Positions for caption and date are for a PDF output and the rendering may vary for browser displays. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rpmp}}, \code{\link{rpmcd}}, \code{\link{retn}}, \code{\link{get.edh}}
}
\examples{
# Roman province of Aegyptus
plot.map(x="Aeg")

# Roman Empire transport network
plot.map(roads=TRUE, shipr=TRUE)
}
\keyword{ graphs }
\keyword{ utilities }
