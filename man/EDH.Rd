\name{EDH}
\docType{data}
\alias{EDH}
\title{
Epigraphic Database Heidelberg Data Set
}
\description{
This is a data set retrieved from the Epigraphic Database Heidelberg. 
}
\usage{
data("EDH")
}
\format{
A list object of 82464 items (until 20-11-2019) with at least one of the following 46 
(or more) names in the EDH list: 

\code{"ID"}, \code{"commentary"}, \code{"fotos"}, \code{"country"}, 
\code{"depth"}, \code{"diplomatic_text"}, \code{"edh_geography_uri"}, \code{"findspot"}, 
\code{"findspot_ancient"}, \code{"findspot_modern"}, \code{"geography"}, \code{"height"}, \code{"id"}, 
\code{"language"}, \code{"last_update"}, \code{"letter_size"}, \code{"literature"}, 
\code{"material"}, \code{"military"}, \code{"modern_region"}, \code{"not_after"}, 
\code{"not_before"}, \code{"person"} (which is a list with: \code{"person_id"}, \code{"nomen"}, \code{"cognomen"}, 
\code{"praenomen"}, \code{"name"}, \code{"gender"}, \code{"status"}, \code{"tribus"}, 
\code{"origo"}, \code{"occupation"}, \code{"age: years"}, \code{"age: months"}), 
\code{"present_location"}, \code{"province_label"}, \code{"religion"}, \code{"responsible_individual"}, 
\code{"social_economic_legal_history"}, \code{"transcription"}, \code{"trismegistos_uri"}, 
\code{"type_of_inscription"}, \code{"type_of_monument"}, \code{"uri"}, \code{"width"}, 
\code{"work_status"}, and \code{"year_of_find"}. 
}
\source{
https://edh-www.adw.uni-heidelberg.de/data/api
}
%\references{
%}
\seealso{
\code{\link{get.edh}}, \code{\link{get.edhw}},\code{\link{edhw}}
}
\keyword{datasets}
