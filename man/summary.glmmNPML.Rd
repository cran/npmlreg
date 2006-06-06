\name{summary.glmmNPML}
\alias{print.glmmNPML}
\alias{print.glmmGQ}
\alias{summary.glmmNPML}
\alias{summary.glmmGQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarizing finite mixture regression fits}
\description{
  These functions are the \code{summary} and \code{print} methods for objects of  type
  \code{glmmNPML} and \code{glmmGQ}.
}
\usage{
summary.glmmNPML(object, digits = max(3, getOption("digits") - 3), ...)
summary.glmmGQ(object, digits = max(3, getOption("digits") - 3), ...)

print.glmmNPML(x, digits=max(3,getOption('digits')-3), ...)
print.glmmGQ(x, digits=max(3,getOption('digits')-3),  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a fitted object of class \code{glmmNPML} or \code{glmmGQ}.   }
  \item{x}{a fitted object of class \code{glmmNPML} or \code{glmmGQ}. }
  \item{digits}{number of digits; applied on various displayed quantities.}
  \item{\dots}{further arguments, which will mostly be ignored.}
}
\details{

The \code{summary...}- and \code{print...} -functions invoke the generic 
\code{UseMethod(...)} function. Application of \code{summary.glm()}  
on an object created by \code{alldist} is also possible and yields a summary of 
the GLM fitted in the last iteration of the EM algorithm. 
Note again that the deviance given in that summary is not the deviance of 
the NPML estimate! The generic \R functions \code{update(), model.matrix(), 
coefficients(), coef()}, and \code{family()} 
can be applied straightforwardly on all objects of class \code{glmmNPML} 
or \code{glmmGQ}. The functions \code{df.residual(), fitted.values(), fitted()} 
and \code{residuals()} are also supported, they have indeed to be used with 
care as they give information on the extended GLM in the final EM cycle, 
rather than on the NPML/GQ estimate. To obtain predicted 
('fitted') values of the random effect model, use \code{predict()}. 
}
\value{
  Print or Summary.
  
  Objects returned by \code{summary.glmmNPML}  or  \code{summary.glmmGQ} are identical
  to objects of class \code{glmmNPML} or \code{glmmGQ}, but have an additional component
  \code{$dispersion} providing the estimated dispersion parameter.
  
}
%\references{ ~put references to the literature/web site here ~ }
\author{ originally from Ross Darnell (2002), modified and prepared for 
publication by Jochen Einbeck and John Hinde (2006)}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{alldist}}, \code{\link{allvc}}, \code{\link{summary}}, 
\code{\link{print}}, \code{\link{family.glmmNPML}} }
%\examples{

%}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
