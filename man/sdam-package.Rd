\name{sdam-package}
\alias{sdam-package}
\alias{sdam}
\docType{package}
\title{
\packageTitle{sdam}
}
\description{
\packageDescription{sdam}
}
\details{

\tabular{ll}{
Package: \tab sdam \cr
Type: \tab Package \cr
Version: \tab 0.3.3 \cr
Date: \tab 3 December 2020 \cr
License: \tab CC BY-SA 4.0 \cr
}


%The DESCRIPTION file:
%\packageDESCRIPTION{sdam}

%\packageIndices{sdam}
Currently, it is possible with the \code{sdam} package to access data from the Epigraphic Database 
Heidelberg API with \code{get.edh()}, and the wrapper functions \code{get.edhw()} \code{edhw()} as well. 
Most of the data is available in the dataset attached to the package, called \code{EDH}, and which 
can be manipulated by using the \code{edhw()} convenient function. 

Besides, the \code{request()} function allows performing different types of 
HTTP requests from a cloud repository like DEiC'S \url{https://sciencedata.dk} or another 
customized URL address. 

Similarity by simple matching among column vectors is achieved by the \code{simil()} function (still under development)
in order to make analyses of relations between assemblages and artifacts. 

There is also the possibility to compute probabilities of existence with \code{prex()} with either 
the aoristic sum or count matching for observations. Function \code{plot.dates()} allows visualizing 
interval time events. 
}
\author{
Author: \packageAuthor{sdam} 

Maintainer: \packageMaintainer{sdam}
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link[multigraph]{multigraph}}
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
