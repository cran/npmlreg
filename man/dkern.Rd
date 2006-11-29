\name{dkern}
\alias{dkern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aitchison-Aitken kernel}
\description{
  Discrete kernel for categorical data  with \code{k} unordered categories.
}
\usage{
dkern(x, y, k, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{categorical data vector}
  \item{y}{postive integer defining a fixed category}
  \item{k}{positive integer giving the number of categories}
  \item{lambda}{smoothing parameter}
}
\details{
This kernel was introduced in Aitchison & Aitken (1976); see also Titterington (1980). 
 
 The setting \code{lambda =1/k} corresponds to the extreme case 'maximal smoothing',
   while \code{lambda = 1} means `no smoothing'. Statistically sensible settings are 
   only \code{1/k}\eqn{\le}{<=} \code{lambda} \eqn{\le}{<=}\code{1}. 
}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
\references{ 
  Aitchison, J. and Aitken, C.G.G. (1976). Multivariate binary discrimination by kernel 
  method. Biometrika 63, 413-420. 

  Titterington, D. M. (1980). A comparative study of kernel-based density estimates for
   categorical data.  Technometrics, 22, 259-268.

 }
\author{ Jochen Einbeck (2006)}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

k<-6; 
dkern(1:k,4,k,0.99)   
# Kernel centered at the 4th component with a very small amount of smoothing.


## The function is currently defined as
function(x,y,k,lambda){
ifelse(y==x, lambda, (1-lambda)/(k-1))
  }
}

\keyword{models }% at least one, from doc/KEYWORDS
\keyword{regression }% __ONLY ONE__ keyword per line
