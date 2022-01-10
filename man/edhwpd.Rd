\name{edhwpd}
\alias{edhwpd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organize EDH dataset province and dates by similarity
}
\description{
Wrapper function to organize EDH dataset province and dates by simple match similarity.
}
\usage{
edhwpd(x = NULL, vars, province, dates, clean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
typically fragments of EDH dataset or database API (optional, list)
}
  \item{vars}{
variables or attributes to be chosen from \code{x} (vector)
}
  \item{province}{
choosen EDH province abbreviation
}
  \item{dates}{
vector with TAQ and TPQ (optional)
}
  \item{clean}{
whether to remove special characters in text (optional and logical)
}
}
\details{
As with function \code{edhw}, this is an interface to extract attribute variables 
in \code{vars} from the \code{EDH} or similar dataset if \code{x} is not specified. 
The aim is to organize data per province and dates by simple match similarity among
records. 

The Roman Empire \code{province} is the abbreviation used in the value given by 
function \code{get.edh} and which is in \code{"rp"} dataset. 

Argument \code{dates} is optional to specify the variables for time intervals 
(TAQ and TPQ) that in \code{EDH} are \code{"not_after"} and \code{"not_before"}.

Argument \code{clean} applies function \code{cln} to the province data frame with the chosen 
variables to remove special characters such as \code{?*+} and, if needed, re-encode the text. 

The output is a list of data frames with similar arguments by descending matches. The records 
with one or less similarity matches (or having \code{NA} attribute values) are placed in the 
last data frame of the list.
}
\value{
List of data frames of a \code{EDH} class object that includes the province with the number of
records.
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
\code{\link{edhw}}, \code{\link{get.edh}}, \code{\link{cln}}
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
\keyword{ data }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }