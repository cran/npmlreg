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
\method{summary}{glmmNPML}(object, digits = max(3, getOption("digits") - 3), ...)
\method{summary}{glmmGQ}(object, digits = max(3, getOption("digits") - 3), ...)
\method{print}{glmmNPML}(x, digits=max(3,getOption('digits')-3), ...)
\method{print}{glmmGQ}(x, digits=max(3,getOption('digits')-3),  ...)
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
\code{UseMethod(...)} function and detect the right model class
 automatically.  In other words, it is enough to write
 \code{summary(...)} or \code{print(...)}.

}

\value{
  Prints regression output or summary on screen.
  
  Objects returned by \code{summary.glmmNPML}  or  \code{summary.glmmGQ} are 
  essentially identical to objects of class \code{glmmNPML} or \code{glmmGQ}.
  However,  their \code{$coef} component contains the parameter standard errors 
  and t values (taken from the GLM fitted in the last EM iteration),  and they 
  have two additional components \code{$dispersion} and \code{$lastglmsumm} 
  providing the estimated dispersion parameter and a summary of the \code{glm} 
  fitted in the last EM iteration.
}

\note{
  Please note that the provided parameter standard errors tend to be
  underestimated as the uncertainty due to the EM algorithm is not
  incorporated into them.  According to Aitkin et al (2005), Section 7.6, page
  428, more accurate standard errors can be obtained by dividing the
  (absolute value of the) parameter estimate through the square root of
  the change in disparity when omitting/not omitting the variable from the model.
  }

\references{ 
Aitkin, M., Francis, B. and Hinde, J. (2005). Statistical Modelling in GLIM 4. Second Edition, Oxford Statistical Science Series, Oxford, UK. 
}

\author{originally from Ross Darnell (2002), modified and prepared for 
publication by Jochen Einbeck and John Hinde (2007).}
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
