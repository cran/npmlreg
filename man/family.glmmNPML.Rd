\name{family.glmmNPML}
\alias{family.glmmNPML}
\alias{family.glmmGQ}
\alias{model.matrix.glmmNPML}
\alias{model.matrix.glmmGQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generic functions for objects of class glmmNPML or glmmGQ}
\description{
  Methods for the generic \code{family} and \code{model.matrix} functions 
}
\usage{
family.glmmNPML(object, ...)
family.glmmGQ(object, ...)
model.matrix.glmmNPML(object, ...)
model.matrix.glmmGQ(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{glmmNPML} or \code{glmmGQ}. }
  \item{\dots}{ further arguments, ensuring compability with generic functions. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{Jochen Einbeck and John Hinde (2006)}
\note{
The generic \R functions \code{update()}, \code{coefficients()},  \code{coef()}, 
\code{fitted()}, \code{fitted.values()}, and \code{df.residual()}
can also be applied straightforwardly on all objects of 
class \code{glmmNPML} or \code{glmmGQ}.  They are not listed above as they use
the generic default functions  and are not separately implemented.

Explicit implementations exist for \code{predict},  \code{summary}, 
\code{print}, and \code{plot}, and these functions are explained in the corresponding
help files.   
}
\seealso{ \code{\link{summary.glmmNPML}}, \code{\link{predict.glmmNPML}}, 
\code{\link{family}}, \code{\link{model.matrix}}, \code{\link{update}}, 
\code{\link{coefficients}}, \code{\link{alldist}}.  
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function(object, ...) {
%     object$family
% }
%}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
