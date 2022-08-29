\name{edhwpd}
\alias{edhwpd}
\title{
Organize EDH dataset province and dates by similarity
}
\description{
Wrapper function to organize \code{\link{EDH}} dataset province and dates by simple match similarity.
}
\usage{
edhwpd(x = "EDH", vars, province, dates, clean, ...)
}
\arguments{
  \item{x}{
   EDH dataset, or fragments of, or database via API (optional, list)
}
  \item{vars}{
   vector with variables or attributes chosen from \code{x}
}
  \item{province}{
   Roman province abbreviation as in \code{rp}
}
  \item{dates}{
   vector with TAQ and TPQ (optional)
}
  \item{clean}{
   whether to remove special characters in text (optional and logical)
}
  \item{\dots}{
   additional arguments if needed
}
}
\details{
This wrapper function aims to organize data per Roman province and date by simple match similarities 
among inscriptions in the \code{\link{EDH}} dataset. 
As with function \code{\link{edhw}}, it is an interface to extract attribute variables 
in \code{vars} from the \code{\link{EDH}} or similar dataset if \code{x} is not specified. 

The Roman Empire \code{province} is the abbreviation used in the value given by 
function \code{\link{get.edh}} and which is in \code{\link{rp}} dataset. 

Argument \code{dates} is optional to specify the variables for time intervals 
(TAQ and TPQ) that in \code{\link{EDH}} are \code{not_after} and \code{not_before}, 
but other datasets may have different names for the endpoints of the timespan. 
Another dependence with this function is from package \code{"multiplex"} to find clusters of items having similar 
characteristics as co-occurrence relations and for removing isolated items from the system of relations.

Argument \code{clean} applies function \code{\link{cln}} to the province data frame with the chosen 
variables to remove special characters such as \code{?*+} and, if needed, re-encode the text. 

The output is a list of data frames with similar arguments by descending matches. The records 
with one or less similarity matches (or having \code{NA} attribute values) are placed in the 
last data frame of the list.
}
\value{
A \code{\link{EDH}} class object with the province and the number of records with a list of data frames organised 
by components where the first one has records having most common attribute variables, whereas the 
last component is a dataframe with records having least common attribute variables.
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
\code{\link{rmids}}, \code{\link{rp}}, \code{\link{edhw}}, \code{\link{get.edh}}, \code{\link{cln}}
}
\examples{
\dontrun{
## load dataset
data(EDH)

## extract province & dates with a single variable attribute from EDH
edhwpd(vars="type_of_inscription", province="Rom", dates=c("not_after", "not_before"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
\keyword{ utilities }