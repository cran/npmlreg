\name{post}
\alias{post}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Posterior probabilities/intercepts, and mass point classifications}
\description{
  Takes an object of class \code{glmmNPML} or \code{glmmGQ} and displays the 
  posterior probabilites \eqn{w_{ik}}{w_ik} as well as  the posterior intercepts 
  (Sofroniou et. al, 2006). Further it classfies the observations to mass points 
  according to their  posterior probability. The level on which the information 
  in all three cases is displayed can be chosen by the user via the \code{level} 
  argument (\code{"upper"} or \code{"lower"}). The actual information in both cases is 
   identical, the latter is just an  expanded version of the former. In case of 
   simple overdispersion models, the  \code{level} argument is not relevant.
}
\usage{
post(object, level="upper")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{glmmNPML} or \code{glmmGQ}. }
  \item{level}{ \code{"upper"} or \code{"lower"}.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{

A list of the following four items:

\item{prob}{posterior probabilities (identical to \code{object$post.prob} 
in case of \code{"lower"} and for one-level models).}
\item{int}{posterior intercepts (identical to \code{object$post.int} 
in case of \code{"lower"} and for one-level models).}
\item{classif}{a numerical vector containing the class numbers (the order of the classes corresponds to the 
order of the mass points given in the output of \code{alldist} or \code{allvc}).} 
\item{level}{either \code{"lower"}, \code{"upper"}, or \code{"none"} (for one-level models).}
 }
\references{
Sofroniou, N., Einbeck, J., and Hinde, J. (2006). Analyzing Irish suicide rates with mixture models. Proceedings of the 21st International Workshop on Statistical Modelling in Galway, Ireland, 2006. 
}

\author{Jochen Einbeck and John Hinde (2006) }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{alldist}}, \code{\link{allvc}} }
\examples{
 data(galaxies, package="MASS")
 gal <- as.data.frame(galaxies)
 post(alldist(galaxies/1000~1, random=~1, data=gal, k=5))$classif
    # classifies the 82 galaxies to one of the five mass points
 }
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
