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
Version: \tab 0.2.7 \cr
Date: \tab 21 October 2020 \cr
License: \tab CC BY-SA 4.0 \cr
}


%The DESCRIPTION file:
%\packageDESCRIPTION{sdam}

%\packageIndices{sdam}
Currently, it is possible with the \code{sdam} package to access data from the Epigraphic Database 
Heidelberg open repository through its API with \code{get.edh()}, and the wrapper function \code{get.edhw()} as well. 
Most of the data is available in the dataset attached to the package, called \code{EDH}, and which 
can be manipulated by using the \code{edhw()} convenient function, for example by extracting variables from it. 

Besides EDH, function \code{request()} allows performing different types of 
HTTP requests from a cloud repository like DEiC'S \url{https://sciencedata.dk} or with another 
customized URL address. 

Similarity by simple matching among column vectors is achieved by the \code{simil()} function 
in order to make analyses of relations between assemblages and artifacts. 

Note that this latter function still under development. 
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
