\name{fget_A_RothC}
\alias{fget_A_RothC}
\title{
  RothC Transfer Matrix Building Function
}
\author{
Marc Scherstjanoi \email{marc.scherstjanoi@thuenen.de}, Rene Dechow
}
\description{
Builds a RothC transfer matrix. Parameters taken from \insertCite{Coleman1996;textual}{sorcering}.
\loadmathjax  
}
\usage{
fget_A_RothC(    clay = 23.4
)
}
\arguments{
  \item{clay}{ double. Soil clay content in \%.}

}

\value{
 \code{fget_A_RothC()} returns a \mjeqn{5\times 5}{5X5} matrix that contains RothC specific carbon transfer parameters based on clay content.
} 
 
\seealso{
  \code{\link{sorcering}}.
}

\references{

\insertAllCited{}

}

\examples{
  fget_A_RothC(clay=30) 
}
