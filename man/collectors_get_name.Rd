% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collectors_get_name.R
\name{collectors_get_name}
\alias{collectors_get_name}
\title{Get the last name of the main collector}
\usage{
collectors_get_name(
  x = NA,
  surname_selection_type = "largest_string",
  max_words_name = 6,
  maximum_characters_in_name = 4
)
}
\arguments{
\item{x}{recordedBy field}

\item{surname_selection_type}{Allows you to select two types of results
for the main collector's last name: large_string - word with the
largest number of characters or last_name - literally the last name
of the main collector, with more than two characters.}

\item{maximum_characters_in_name}{Maximum characters in name}
}
\value{
last name of the main collector
}
\description{
Get the last name of the main collector in recordedBy field
}
\details{
Returns the last name
}
\examples{
\donttest{
help(collectors_get_name)

collectors_get_name('Melo, P.H.A & Monro, A.')

collectors_get_name('Monro, A. & Melo, P.H.A')
}

}
\seealso{
\code{\link[parseGBIF]{collectors_prepare_dictionary}}, \code{\link[parseGBIF]{collectors_update_dictionary}}
}
\author{
Pablo Hendrigo Alves de Melo,
Nadia Bystriakova &
Alexandre Monro
}
