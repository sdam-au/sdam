\name{edhw}
\alias{edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper function for the \code{EDH} dataset
}
\description{
A function to obtain variable data and perform transformations on the Epigraphic Database Heidelberg \code{EDH} dataset.
}
\usage{
edhw(vars, x = NULL, as = c("list", "df"), addID, limit, id, na.rm, wide, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
   Choose variables of interest from \code{x}. If \code{x=NULL}, the entire \code{EDH} dataset is taken. (optional, vector)
}
  \item{x}{
   A list object name with fragments of the \code{EDH} dataset (optional)
}
  \item{as}{
   Format to return the output. Currently either as a \code{list} or a data frame \code{df} object.
}
  \item{addID}{
   Add identification to the output? (optional and logical)
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
  \item{wide}{
   Use a wide format in data frame? Only for \code{"people"}. (logical and optional)
}
  \item{\dots}{
   Optional arguments if needed.
}
}
\details{
This is a convenient function to "extract" \emph{variables} from the \code{EDH} dataset attached 
to this package. However, the input in \code{x} can be fragments of the \code{EDH} dataset or from 
the the Epigraphic Database Heidelberg API obtained by functions \code{get.edh()} or \code{get.edhw()}
with the \code{rjson} format. 
When \code{x} is explicited, it must be at least a list object with a comparable structure to the \code{EDH} dataset. 


Through \code{vars} argument and return the output either \code{as} a list with \code{list} 
or a data frame with \code{df}, and when argument \code{vars} is missing, then all entries in \code{x} are taken. 

By default, a list object is returned, with or without an ID identification provided by the \code{addID} argument. 
When the input list is converted into a data frame, the ordering of the variables is given alphabetically. 
If desired, it is also possible to remove missing data from the output by activating \code{na.rm} and 
work with complete cases. 

Arguments \code{id} and \code{limit} serve to reduce the returned output either to some Epigraphic 
Database number or numbers, which are specified by \code{hd_nr}, or else by limiting the amount of 
the returned output. \code{limit} here is like the \code{limit} argument of function \code{get.edh()}, but in 
this case the offset can be specified as a sequence. 
While \code{"limit"} is a faster way to get to entries in the \code{EDH} dataset, argument \code{id} is for 
refering to precisely one or more \code{hd_nr}s in the Epigraphic Database Heidelberg API.

Component "\code{people}" is a separated list in the \code{EDH} dataset, and it should be considered as 
a separate case from the rest of the variables. 
In the case that the output is a data frame, the default output is a \emph{long} table; that is records can 
appear in different rows and each variable is assigned into a single column. By setting \code{wide} to \code{TRUE}, 
it is possible to place the different people from a single entry column by column in the data frame and each 
record has a single row. 
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
In case that the output chosen is a dataframe, then component \code{"people"} should be chosen alone. 
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
