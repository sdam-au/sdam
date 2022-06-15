\name{cln}
\alias{cln}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean and re-encode a vector, list or dataframe
}
\description{
A function to re-encode Greek (and other) characters and to remove symbols at the end of a record.
}
\usage{
cln(x, level, what, na.rm, case, repl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    a vector, list or dataframe
}
  \item{level}{
    optional clean level, either \code{0} for no-clean, default \code{1} or \code{2} (see details)
}
  \item{what}{
    additional characters to clean (optional)
}
  \item{na.rm}{
   Remove entries with NA data? (optional and logical)
}
  \item{case}{
   case for text \code{1} for 1st uppercase, code{2} for lowercase, code{3} for uppercase (optional)
}
  \item{repl}{
   data frame with text to replace (optional)
}
}
\details{
This function is meant to re-encode Greek (and other) characters in the \code{EDH} set given 
either as list format, vector, or a dataframe produced with function \code{edhw()} for example. 
By default, symbols \code{"?" "*" "+"} placed at the end of each record are removed after the re-encoding. 
However, when level is \code{0} only re-encoding is performed, and level \code{2} is either to force 
an extra iteration in the re-encoding, removing extra spaces, or clean \code{what} is invoked. 
With \code{repl}, is possible to replace a list of text in two columns, for `text to replace' and for `text that replace'
}
\value{
Depending on the input, a vector, list or dataframe.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edhw}}, \code{\link{get.edh}}
}
\examples{
# clean Greek characters
cln("Caesar?*+")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
