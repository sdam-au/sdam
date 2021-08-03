\name{retn}
\docType{data}
\alias{retn}
\title{
Roman Empire transport network 
}
\description{
This is a list of lists with specifications to plot different cartographical maps of the Roman Empire and 
its transport network including settlements, roads, and shipping routes. 
}
\usage{
data("retn")
}
\format{
A list of lists object with the shape data in different slots for 4 cartographical maps of the Roman Empire 
with names \code{rcoast} for a plain map, \code{rpcoast} for a map with provinces, \code{rpsi} for a map 
with senatorial and imperial provinces, and \code{rptetra} for a tetrarchy map. 
Three components in \code{retn} have coordinates for settlements \code{nds}, roads \code{rds}, and shipping 
routes \code{srs}.
}
\source{
DARMC, Center for Geographic Analysis, Harvard University

Rodrigue, Comtois, Slack. (2013) \emph{The geography of transport systems}. Routledge

https://commons.wikimedia.org/wiki/File:RomanEmpire_117.svg

https://commons.wikimedia.org/wiki/File:Roman_provinces_trajan.svg

https://commons.wikimedia.org/wiki/File:Regioni_dell'Italia_Augustea.svg
}
%\references{
%}
\seealso{
\code{\link{plot.map}}, \code{\link{rp}}, \code{\link{rpmp}}, \code{\link{rpmcd}}
}
\keyword{datasets}
