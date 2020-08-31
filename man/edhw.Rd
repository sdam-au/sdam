\name{edhw}
\alias{edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Epigraphic Database Heidelberg wrapper function
}
\description{
A function to obtain variable data from the \code{EDH} data set and perform transformations.
}
\usage{
edhw(vars, x = NULL, as = c("list", "df"), limit, id, na.rm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
Choose variables from \code{x}. If \code{x=NULL}, the entire \code{EDH} dataset is taken. (optional, vector)
}
  \item{x}{
 A list object name with fragments of the \code{EDH} dataset, or another EDH dataset created either with \code{rjson} or \code{jsonlite} (optional)
}
  \item{as}{
Format to return the output. Currently either as a \code{list} or a data frame \code{df} object.
}
  \item{limit}{
Limit the returned output. Ignored if \code{id} is specified (optional, integer or vector)
}
  \item{id}{
Select only the \code{hd_nr} id(s) (optional, integer or character)
}
  \item{na.rm}{
Remove entries with NA data? (logical and optional)
}
  \item{\dots}{
Optional arguments if needed.
}
}
\details{
This is a convenient function to "extract" \emph{variables} from the \code{EDH} dataset attached 
to this package through \code{vars}. When \code{vars} is missing, then all entries are taken.

In case \code{x} is present, it is typically given as a fragment of the \code{EDH} dataset that can be 
obtained for example with function \code{get.edh()} or \code{get.edhw()}. Otherwise, it must be at least 
a list object with a comparable structure to \code{EDH}. 

By default, a list object is returned but it is possible to convert the list into a data frame, and with 
this option the ordering of the variables is given alphabetically. 
If desired, it is also possible to remove missing data in the output format by activating \code{na.rm}. 

Arguments \code{id} and \code{limit} serve to reduce the returned output either to some Epigraphic 
Database number or numbers, which are specified by \code{hd_nr}, or else by limiting the amount of 
the returned output. This latter is like the \code{limit} argument of function \code{get.edh()}, where 
the offset can be specified as well. 
Argument \code{id} is for refering to precisely \code{hd_nr}s, while \code{"limit"} is a faster way to 
get to these entries in the \code{EDH} dataset. 
}
\value{
A list or a data frame, depending on the input. 
}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
In case that the output chosen is a dataframe, then component \code{"people"} is ignored. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{get.edhw}}
}
\examples{
\dontrun{
## load data set
data(EDH)

## make a list for three variables in 'EDH' (default)
edhw(vars=c("ID", "not_after", "not_before") )}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
