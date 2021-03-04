\name{edhw}
\alias{edhw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper function for manipulation of \code{EDH} dataset
}
\description{
A function to obtain variable data and perform transformations on the Epigraphic Database Heidelberg \code{EDH} dataset.
}
\usage{
edhw(x = NULL, vars, as = c("df", "list"), type = c("long", "wide", "narrow"), split, 
     select, addID, limit, id, na.rm, clean, province, gender, ...)
}

\arguments{
  \item{x}{
   A list object name with fragments of the \code{EDH} dataset (optional)
}
  \item{vars}{
   Variables of interest from \code{x}; if \code{x=NULL}, the entire \code{EDH} dataset is taken. (optional, vector)
}
  \item{as}{
   Format to return the output. Currently either as a \code{"list"} or a data frame \code{"df"} object.
}
  \item{type}{
   Type format of data frame. Currently either \code{"long"} or \code{"wide"} (\code{"narrow"} not yet implemented)
}
  \item{split}{
   Divide the data into groups by id? (optional and logical)
}
  \item{select}{
   Choose \code{"people"} variables (optional, vector)
}
  \item{addID}{
   Add identification to the output? (optional and logical)
}
  \item{limit}{
   Limit the returned output. Ignored if \code{id} is specified (optional, integer or vector)
}
  \item{id}{
   Select only \code{hd_nr} records (optional, integer or character)
}
  \item{na.rm}{
   Remove entries with NA data? (optional and logical)
}
  \item{clean}{
   clean list object \code{x}? (optional and logical)
}
  \item{province}{
   Roman province (character, optional)
}
  \item{gender}{
   people gender in \code{EDH} (character, optional)
}
  \item{\dots}{
   Optional arguments if needed.
}
}
\details{
This is a convenient function to "extract" \emph{variables} from the \code{EDH} dataset attached 
to this package. However, the input in \code{x} can be fragments of the \code{EDH} dataset or from 
the Epigraphic Database Heidelberg API obtained by functions \code{get.edh()} or \code{get.edhw()}
with the \code{rjson} format. 
When \code{x} is explicit, it must be at least a list object with a comparable structure to the \code{EDH} dataset. 


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
referring to precisely one or more \code{hd_nr}s in the Epigraphic Database Heidelberg API.

Component \code{"people"} is a separated list in the \code{EDH} dataset, and it should be considered as 
a separate case from the rest of the variables. 
In the case that the output is a data frame, the default output is a `long' \code{type} table; that is records can 
appear in different rows and each variable is assigned into a single column, and with this option is possible to 
\code{select} \code{"people"} variables.

By setting \code{"wide"} in \code{type}, it is possible to place the different people from a single entry 
column by column in the data frame and each record has a single row. Finally, argument \code{split} allows 
dividing the data in the data frame into groups by `id', which corresponds to the HD number of inscription 
in the \code{EDH} dataset. 

Ad hoc arguments are the \code{EDH} entries \code{province} and \code{gender} 
for entering a Roman province and people's gender in \code{x}.
}
\value{
A list or a data frame with a long or wide format, depending on the arguments inputs. 
}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
When choosing \code{"people"} variables with \code{select} with a data frame output, then this attribute should be chosen 
in \code{vars}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{get.edhw}}, \code{\link{prex}}, \code{\link{plot.dates}}, \code{\link{EDH}}
}
\examples{
\dontrun{
## load data set
data(EDH)

## make a list for three variables in 'EDH' (default)
edhw(vars=c("type_of_inscription", "not_after", "not_before") )

## select 'gender' from 'people' in 'EDH' (default)
edhw(vars=c("people", "not_after", "not_before"), select="gender" )}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
