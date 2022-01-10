\name{rpd}
\docType{data}
\alias{rpd}
\title{
Roman provinces dates from EDH
}
\description{
This is a list with Roman province dates from the Epigraphic Database Heidelberg, EDH. 
}
\usage{
data("rpd")
}
\format{
A list object of 66 Roman provinces with dates. 
Each list component has a vector for the province containing of the dates: 
the average, min TAQ, max TPQ, and the average length time span.
Components in the list have also an attribute class with the \code{"HD_nr"} entries of the province.
}
\source{
https://edh-www.adw.uni-heidelberg.de/data/api/
}
%\references{
%}
\seealso{
\code{\link{rmids}}, \code{\link{rp}}, \code{\link{EDH}}, \code{\link{edhwpd}}, \code{\link{get.edh}}
}
\keyword{datasets}
