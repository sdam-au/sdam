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
        bbox, findspot, pleiades_id, geonames_id, offset, limit, 
        addID, printQ)
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
  \item{offset}{
	Clause to specify which row to start from retrieving data (optional and integer)
}
  \item{limit}{
	Clause to limit the number of results (optional and integer)
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

Notice that \code{hd_nr} is not the same as \code{ID} nor \code{id}. 


Use function \code{\link{get.edhw}} in case you want to grab several items.

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

And for the ancient Roman provices the valued values are

\tabular{llllll}{
\code{"Ach"}  \tab  Achaia                           \tab  \code{"Cor"}  \tab  Corsica                       \tab  \code{"Mes"}  \tab  Mesopotamia                   \cr 
\code{"Aeg"}  \tab  Aegyptus                         \tab  \code{"Cre"}  \tab  Creta                         \tab  \code{"MoI"}  \tab  Moesia inferior               \cr 
\code{"Aem"}  \tab  Aemilia (Regio VIII)             \tab  \code{"Cyp"}  \tab  Cyprus                        \tab  \code{"MoS"}  \tab  Moesia superior               \cr 
\code{"Afr"}  \tab  Africa Proconsularis             \tab  \code{"Cyr"}  \tab  Cyrene                        \tab  \code{"Nar"}  \tab  Narbonensis                   \cr 
\code{"AlC"}  \tab  Alpes Cottiae                    \tab  \code{"Dac"}  \tab  Dacia                         \tab  \code{"Nor"}  \tab  Noricum                       \cr 
\code{"AlG"}  \tab  Alpes Graiae                     \tab  \code{"Dal"}  \tab  Dalmatia                      \tab  \code{"Num"}  \tab  Numidia                       \cr 
\code{"AlM"}  \tab  Alpes Maritimae                  \tab  \code{"Epi"}  \tab  Epirus                        \tab  \code{"PaI"}  \tab  Pannonia inferior             \cr 
\code{"AlP"}  \tab  Alpes Poeninae                   \tab  \code{"Etr"}  \tab  Etruria (Regio VII)           \tab  \code{"PaS"}  \tab  Pannonia superior             \cr 
\code{"ApC"}  \tab  Apulia et Calabria (Regio II)    \tab  \code{"Gal"}  \tab  Galatia                       \tab  \code{"Pic"}  \tab  Picenum (Regio V)             \cr 
\code{"Aqu"}  \tab  Aquitania                        \tab  \code{"GeI"}  \tab  Germania inferior             \tab  \code{"Rae"}  \tab  Raetia                        \cr 
\code{"Ara"}  \tab  Arabia                           \tab  \code{"GeS"}  \tab  Germania superior             \tab  \code{"ReB"}  \tab  Regnum Bospori                \cr 
\code{"Arm"}  \tab  Armenia                          \tab  \code{"HiC"}  \tab  Hispania citerior             \tab  \code{"Rom"}  \tab  Roma                          \cr 
\code{"Asi"}  \tab  Asia                             \tab  \code{"Inc"}  \tab  Provincia incerta             \tab  \code{"Sam"}  \tab  Samnium (Regio IV)            \cr 
\code{"Ass"}  \tab  Assyria                          \tab  \code{"Iud"}  \tab  Iudaea                        \tab  \code{"Sar"}  \tab  Sardinia                      \cr 
\code{"Bae"}  \tab  Baetica                          \tab  \code{"LaC"}  \tab  Latium et Campania (Regio I)  \tab  \code{"Sic"}  \tab  Sicilia, Melita               \cr 
\code{"Bar"}  \tab  Barbaricum                       \tab  \code{"Lig"}  \tab  Liguria (Regio IX)            \tab  \code{"Syr"}  \tab  Syria                         \cr 
\code{"Bel"}  \tab  Belgica                          \tab  \code{"Lug"}  \tab  Lugdunensis                   \tab  \code{"Thr"}  \tab  Thracia                       \cr 
\code{"BiP"}  \tab  Bithynia et Pontus               \tab  \code{"Lus"}  \tab  Lusitania                     \tab  \code{"Tra"}  \tab  Transpadana (Regio XI)        \cr 
\code{"BrL"}  \tab  Bruttium et Lucania (Regio III)  \tab  \code{"LyP"}  \tab  Lycia et Pamphylia            \tab  \code{"Tri"}  \tab  Tripolitania                  \cr 
\code{"Bri"}  \tab  Britannia                        \tab  \code{"MaC"}  \tab  Mauretania Caesariensis       \tab  \code{"Umb"}  \tab  Umbria (Regio VI)             \cr 
\code{"Cap"}  \tab  Cappadocia                       \tab  \code{"MaT"}  \tab  Mauretania Tingitana          \tab  \code{"Val"}  \tab  Valeria                       \cr 
\code{"Cil"}  \tab  Cilicia                          \tab  \code{"Mak"}  \tab  Macedonia                     \tab  \code{"VeH"}  \tab  Venetia et Histria (Regio X)  \cr 
}

}
\value{
A list object with at least one the following items:

\item{"ID"}{(Optional), only if \code{addID} is set to \code{TRUE}.}
\item{"commentary"}{ }
\item{"fotos"}{ }
\item{"country"}{ }
\item{"depth"}{ }
\item{"diplomatic_text"}{ }
\item{"edh_geography_uri"}{ }
\item{"findspot"}{ }
\item{"findspot_ancient"}{ }
\item{"findspot_modern"}{ }
\item{"geography"}{ }
\item{"height"}{ }
\item{"id"}{ }
\item{"language"}{ }
\item{"last_update"}{ }
\item{"letter_size"}{ }
\item{"literature"}{ }
\item{"material"}{ }
\item{"military"}{ }
\item{"modern_region"}{ }
\item{"not_after"}{ }
\item{"not_before"}{ }
\item{}{ }
\item{"people"}{This item is another list with at least one the following items: \cr  
\code{"person_id"} \cr  \code{"nomen"} \cr  \code{"cognomen"} \cr  \code{"praenomen"} \cr  \code{"name"} \cr  \code{"gender"} \cr  \code{"status"} \cr  \code{"tribus"} \cr  
\code{"origo"} \cr  \code{"occupation"} \cr  \code{"age: years"} \cr  \code{"age: months"} }
\item{}{ }
\item{"present_location"}{ }
\item{"religion"}{ }
\item{"province_label"}{ }
\item{"responsible_individual"}{ }
\item{"social_economic_legal_history"}{ }
\item{"transcription"}{ }
\item{"trismegistos_uri"}{ }
\item{"type_of_inscription"}{ }
\item{"type_of_monument"}{ }
\item{"uri"}{ }
\item{"width"}{ }
\item{"work_status"}{ }
\item{"year_of_find"}{ }
\item{}{ }

And also the query is printed if specified. 
}
\references{
https://edh-www.adw.uni-heidelberg.de/data/api

https://edh-www.adw.uni-heidelberg.de/data/api/terms/country

https://edh-www.adw.uni-heidelberg.de/data/api/terms/province
}
\author{
Antonio Rivero Ostoic
}
\note{
The other two search options from the [EDH] database [API], which are \code{"photos"} 
and \code{"bibliography"} may be implemented in the future. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edhw}}, \code{\link{simil}}
}
\examples{
## get inscriptions from EDH API data
## Not run:
get.edh(findspot_modern="madrid")
## End(**Not run**)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
