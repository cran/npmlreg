\name{masspoint.classifier}
\alias{masspoint.classifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Classify observations to mass points}
\description{
  Takes an object of class \code{glmmNPML} or \code{glmmGQ} and classifies
  all observations to the mass point with highest posterior probability.
}
\usage{
masspoint.classifier(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{glmmNPML} or \code{glmmGQ}. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{

a numerical vector containing the class numbers (the order of the classes corresponds to the 
order of the mass points given in the output of \code{alldist} or \code{allvc}).  
 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Jochen Einbeck and John Hinde (2006) }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{alldist}}, \code{\link{allvc}} }
\examples{
 data(galaxies, package="MASS")
 gal<-as.data.frame(galaxies)
 masspoint.classifier(alldist(galaxies/1000~1, random=~1, data=gal, k=5))
}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
