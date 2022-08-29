\name{edhw}
\alias{edhw}
\title{
Wrapper function for manipulation of the EDH dataset
}
\description{
A function to obtain variable data and perform transformations on the \code{\link{EDH}} dataset.
}
\usage{
edhw(x = "EDH", vars, as = c("df", "list"), type = c("long", "wide", "narrow"), 
     split, select, addID, limit, id, na.rm, ldf, province, gender, rp, ...)
}

\arguments{
  \item{x}{
   a list object name with fragments of the \code{\link{EDH}} dataset (optional)
}
  \item{vars}{
   vector of variables of interest from \code{x}; if \code{x=NULL}, the entire \code{\link{EDH}} dataset is taken (optional)
}
  \item{as}{
   format to return the output; either as a \code{"list"} or a data frame \code{"df"} object.
}
  \item{type}{
   type format of data frame; either \code{"long"} or \code{"wide"} (\code{"narrow"} not yet implemented)
}
  \item{split}{
   divide the data into groups by id? (optional and logical)
}
  \item{select}{
   vector with \code{"people"} variables (optional)
}
  \item{addID}{
   add identification to the output? (optional and logical)
}
  \item{limit}{
   integer or vector to limit the returned output. Ignored if \code{id} is specified (optional)
}
  \item{id}{
   select only \code{hd_nr} records (optional, integer or character)
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
  \item{ldf}{
   is \code{x} list of data frames? (optional and logical)
}
  \item{province}{
   name or abbreviation of Roman province in \code{\link{EDH}} as in \code{\link{rp}} dataset
}
  \item{gender}{
   gender of people in \code{\link{EDH}}: \code{male} or \code{female}
}
  \item{rp}{
   customized list of Roman provinces as in \code{\link{rp}} dataset
}
  \item{\dots}{
   optional arguments if needed.
}
}
\details{
This is an interface to extract attribute variables in \code{vars} from the \code{\link{EDH}} dataset attached 
to this package. However, the input in \code{x} can be fragments of the \code{\link{EDH}} dataset or from 
the Epigraphic Database Heidelberg API obtained by functions \code{\link{get.edh}} or \code{\link{get.edhw}}
with the "\code{\link[rjson]{rjson}}" format, or transformed data organized, for example, by provinces. 
When \code{x} is explicit, it must be at least a list object with a comparable structure to the \code{\link{EDH}} dataset. 

Through \code{vars} argument and return the output either \code{as} a list with \code{list} 
or a data frame with \code{df}. In case that argument \code{vars} is missing, then all entries in \code{x} are taken. 

By default, a list object is returned, with or without an `ID' identification provided by the \code{addID} argument. 
When the input list is converted into a data frame, the ordering of the variables is given alphabetically. 
If desired, it is also possible to remove missing data from the output by activating \code{na.rm} and 
work with complete cases. 

Arguments \code{id} and \code{limit} serve to reduce the returned output either to some Epigraphic Database number or to numbers, 
which are specified by \code{hd_nr}, or else by limiting the amount of the returned output. 
\code{limit} here is like the \code{limit} argument of function \code{\link{get.edh}}, but in 
this case the offset can be specified as a sequence. 
While \code{limit} is a faster way to get to entries in the \code{\link{EDH}} dataset, argument \code{id} is for 
referring to precisely one or more \code{hd_nr}s in the Epigraphic Database Heidelberg API.

Component \code{"people"} is a separated list in the \code{\link{EDH}} dataset, and it should be considered as 
a separate case from the rest of the variables. 
In the case that the output is a data frame, the default output is a `long' \code{type} table; that is records can 
appear in different rows and each variable is assigned into a single column, and with this option is possible to 
\code{select} \code{"people"} variables like gender and origin.

When choosing people variables with \code{select} and a data frame output, then \code{"people"} attribute must be in \code{vars}.

By setting \code{"wide"} in \code{type}, it is possible to place the different people from a single entry 
column by column in the data frame and each record has a single row. Finally, argument \code{split} allows for 
dividing the data in the data frame into groups by `id', which corresponds to the HD number of inscription 
in the \code{\link{EDH}} dataset. 

Ad hoc arguments are the \code{\link{EDH}} entries \code{province} and \code{gender} for entering a Roman province 
and people's gender in \code{x} as a data frame; otherwise, these arguments are ignored. 
When \code{province} is used, it is possible to refer to a customized list of provinces with argument "\code{rp}"; 
otherwise, dataset \code{\link{rp}} is the default where names and abbreviations are accepted. 

Argument \code{ldf} is a flag when the input in \code{x} is a created list of data frames that is 
organized by variables rather than by records as in the \code{\link{EDH}} dataset. 
}
\value{
A list or a data frame with a long or wide format, depending on the input arguments. 

Argument \code{province} with no \code{vars} returns a list of lists.
}
\references{
https://edh.ub.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
\code{Warning} messages are given for the \code{EDH} dataset as the input, and when choosing 
the \code{province} argument alone. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{EDH}}, \code{\link{get.edh}}, \code{\link{get.edhw}}, \code{\link{rp}}, \code{\link{edhwpd}}, \code{\link{prex}}, 
\code{\link{plot.dates}}, \code{\link[rjson]{rjson}}
}
\examples{
\dontrun{
## load dataset
data(EDH)

## make a list for three variables in 'EDH' for first 4 entries
edhw(vars=c("type_of_inscription", "not_after", "not_before"), limit=4 )

## as before, but also select 'gender' from 'people'
edhw(vars=c("people", "not_after", "not_before"), select="gender", limit=4 )}
}
\keyword{ data }
\keyword{ manip }
\keyword{ utilities }