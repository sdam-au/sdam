\name{get.edh}
\alias{get.edh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get data from the Epigraphic Database Heidelberg
}
\description{
A function to obtain data from the Epigraphic Database Heidelberg repository.
}
\usage{
get.edh(search = c("inscriptions", "geography"), 
        url = "https://edh-www.adw.uni-heidelberg.de/data/api", 
        hd_nr, province, country, findspot_modern, findspot_ancient, 
        year_not_before, year_not_after, tm_nr, transcription, type, 
        bbox, findspot, pleiades_id, geonames_id, addID, printQ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{search}{
	Whether the search is on \code{inscriptions} \emph{or} on \code{geography}.
}
  \item{url}{
	Open data repository API
}
  \item{hd_nr}{
	HD number of inscription
}
  \item{province}{
	Ancient Roman province name
}
  \item{country}{
	Actual country name
}
  \item{findspot_modern}{
	Actual location name findspot
}
  \item{findspot_ancient}{
	Ancient location name findspot
}
  \item{year_not_before}{
	Year, not before (integer, BC years are negative)
}
  \item{year_not_after}{
	Year, not after (integer, BC years are negative)
}
  \item{tm_nr}{
	Trismegistos' database number (?)
}
  \item{transcription}{
	Automatic leading and trailing truncation (brackets are ignored)
}
  \item{type}{
	Type of inscription (case insensitive)
}
  \item{bbox}{
	Bounding box with character format \code{bbox = "minLong, minLat, maxLong, maxLat"}
}
  \item{findspot}{
	Level of village, street etc. (add leading and/or trailing)
}
  \item{pleiades_id}{
	Pleiades identifier of a place (integer)
}
  \item{geonames_id}{
	Geonames identifier of a place (integer)
}
  \item{addID}{
	Add identification to the output? (optional and logical)
}
  \item{printQ}{
	Also print query? (optional and logical)
}
}
\details{
Since with the \code{inscriptions} option the \code{id} "component" of the 
output list is not with a numeric format, then the function adds an \code{ID} 
at the beginning of the list with the identifier with a numerical format. 

Use function \code{\link{get.edhw}} in case you want to grab several items.

The other two search options from the [EDH] database [API], which are \code{"photos"} 
and \code{"bibliography"} may be implemented in the future. 

Entries in \code{country} are abbreviated country names where the inscription was located. 
A list with the of valid values for countries from the EDH API are 

\tabular{llllll}{
\code{"ad"}  \tab  Andorra                 \tab  \code{"gr"}  \tab  Greece                  \tab  \code{"pl"}  \tab  Poland                \cr 
\code{"al"}  \tab  Albania                 \tab  \code{"hr"}  \tab  Croatia                 \tab  \code{"pt"}  \tab  Portugal              \cr 
\code{"am"}  \tab  Armenia                 \tab  \code{"hu"}  \tab  Hungary                 \tab  \code{"rks"} \tab  Kosovo                \cr 
\code{"at"}  \tab  Austria                 \tab  \code{"il"}  \tab  Israel                  \tab  \code{"ro"}  \tab  Romania               \cr 
\code{"az"}  \tab  Azerbaijan              \tab  \code{"iq"}  \tab  Iraq                    \tab  \code{"rs"}  \tab  Serbia                \cr 
\code{"ba"}  \tab  Bosnia and Herzegovina  \tab  \code{"it"}  \tab  Italy                   \tab  \code{"ru"}  \tab  Russia                \cr 
\code{"be"}  \tab  Belgium                 \tab  \code{"jo"}  \tab  Jordan                  \tab  \code{"sa"}  \tab  Saudi Arabia          \cr 
\code{"bg"}  \tab  Bulgaria                \tab  \code{"kg"}  \tab  Kyrgyzstan              \tab  \code{"sd"}  \tab  Sudan                 \cr 
\code{"ch"}  \tab  Switzerland             \tab  \code{"kz"}  \tab  Kazakhstan              \tab  \code{"se"}  \tab  Sweden                \cr 
\code{"cy"}  \tab  Cyprus                  \tab  \code{"lb"}  \tab  Lebanon                 \tab  \code{"si"}  \tab  Slovenia              \cr 
\code{"cz"}  \tab  Czech Republic          \tab  \code{"li"}  \tab  Liechtenstein           \tab  \code{"sk"}  \tab  Slovakia              \cr 
\code{"de"}  \tab  Germany                 \tab  \code{"lu"}  \tab  Luxembourg              \tab  \code{"sm"}  \tab  San Marino            \cr 
\code{"dk"}  \tab  Denmark                 \tab  \code{"ly"}  \tab  Libyan Arab Jamahiriya  \tab  \code{"sy"}  \tab  Syrian Arab Republic  \cr 
\code{"dz"}  \tab  Algeria                 \tab  \code{"ma"}  \tab  Morocco                 \tab  \code{"tj"}  \tab  Tajikistan            \cr 
\code{"eg"}  \tab  Egypt                   \tab  \code{"mc"}  \tab  Monaco                  \tab  \code{"tn"}  \tab  Tunisia               \cr 
\code{"es"}  \tab  Spain                   \tab  \code{"md"}  \tab  Moldova                 \tab  \code{"tr"}  \tab  Turkey                \cr 
\code{"fr"}  \tab  France                  \tab  \code{"me"}  \tab  Montenegro              \tab  \code{"ua"}  \tab  Ukraine               \cr 
\code{"gb"}  \tab  United Kingdom          \tab  \code{"mk"}  \tab  Macedonia               \tab  \code{"uz"}  \tab  Uzbekistan            \cr 
\code{"ge"}  \tab  Georgia                 \tab  \code{"mt"}  \tab  Malta                   \tab  \code{"va"}  \tab  Vatican City State    \cr 
\code{"gi"}  \tab  Gibraltar               \tab  \code{"nl"}  \tab  Netherlands             \tab  \code{"ye"}  \tab  Yemen                 \cr 
}

}
\value{
A list object with the following items:

\item{"ID"}{ }
\item{"commentary"}{ }
\item{"country"}{ }
\item{"depth"}{ }
\item{"diplomatic_text"}{ }
\item{"edh_geography_uri"}{ }
\item{"findspot_ancient"}{ }
\item{"findspot_modern"}{ }
\item{"height"}{ }
\item{"id"}{ }
\item{"language"}{ }
\item{"last_update"}{ }
\item{"letter_size"}{ }
\item{"literature"}{ }
\item{"material"}{ }
\item{"modern_region"}{ }
\item{"not_after"}{ }
\item{"not_before"}{ }
\item{"people"}{ }
\item{"province_label"}{ }
\item{"responsible_individual"}{ }
\item{"transcription"}{ }
\item{"trismegistos_uri"}{ }
\item{"type_of_inscription"}{ }
\item{"type_of_monument"}{ }
\item{"uri"}{ }
\item{"width"}{ }
\item{"work_status"}{ }
}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api

https://edh-www.adw.uni-heidelberg.de/data/api/terms/country
}
\author{
Antonio Rivero Ostoic
}
\note{
\code{hd_nr} is not the same as \code{ID}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edhw}}, \code{\link{simil}}
}
\examples{
# TBD

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
