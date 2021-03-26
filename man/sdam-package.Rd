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
Version: \tab 0.4.9 \cr
Date: \tab 26 March 2021 \cr
License: \tab CC BY-SA 4.0 \cr
}


%The DESCRIPTION file:
%\packageDESCRIPTION{sdam}

%\packageIndices{sdam}
Currently, it is possible with the \code{"sdam"} package to access data from the Epigraphic Database 
Heidelberg API with \code{get.edh()}, and the wrapper function \code{get.edhw()}. 
Most of the data is available in the \code{EDH} dataset attached to the package, which 
can be manipulated by using \code{edhw()}. 

Besides, the \code{request()} function allows performing different types of HTTP requests to 
DEiC'S \url{https://sciencedata.dk} or another cloud repository with a customized URL address. 

There is also the possibility to compute probabilities of existence with \code{prex()} with either 
the aoristic sum or count matching for observations for different periodization options. 
Function \code{plot.dates()} allows visualizing interval time events that can be manipulated by the 
internal function \code{dts} as illustrated in a vignette. 

Another plotting function in \code{"sdam"} is found in \code{plot.map()} that allows visualizing maps 
of ancient Roman provinces that are found in \code{EDH}. This function depends on two datasets that 
are \code{rpmp} .

Similarity by simple matching among column vectors is achieved by the \code{simil()} function (still under development)
in order to make analyses of relations between assemblages and artifacts. 
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
