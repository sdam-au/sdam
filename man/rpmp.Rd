\name{rpmp}
\docType{data}
\alias{rpmp}
\title{
Maps of ancient Roman provinces and Italian regions
}
\description{
This is a list with specifications to plot cartographical maps of ancient Roman provinces and Italian regions. 
}
\usage{
data("rpmp")
}
\format{
A list of lists object of 59 Roman provinces and Italian regions in year 117AD, and where \code{names(rpmp)} gives the 
province acronyms according to \code{EDH} dataset. 
Each province in \code{rpmp} has a two-length list with the province name and the shape data for a cartographical map in 
different slots. 
}
\source{
https://commons.wikimedia.org/wiki/File:RomanEmpire_117.svg

https://commons.wikimedia.org/wiki/File:Roman_provinces_trajan.svg

https://commons.wikimedia.org/wiki/File:Regioni_dell'Italia_Augustea.svg
}
%\references{
%}
\seealso{
\code{\link{plot.map}}, \code{\link{rpmcd}}, \code{\link{rp}}, \code{\link{EDH}}
}
\keyword{datasets}
