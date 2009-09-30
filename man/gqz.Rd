\name{gqz}
\alias{gqz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gauss-Hermite integration points}
\description{
   Calculate Gaussian Quadrature points for the Normal distribution 
   using the abscissas and weights for Hermite integration.
 }
\usage{
gqz(numnodes=20, minweight=0.000001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numnodes}{theoretical number of quadrature points.}
  \item{minweight}{locations with weights that are less than this value will be omitted.}
}
\details{
  The  conversion of the locations and weights is given in Lindsey (1992,
  page 169:3) and Skrondal & Rabe-Hesketh (2004, page 165:1).
  The argument numnodes is the theoretical number of quadrature points,
  locations with weights that are less than the argument \code{minweight} will
  be omitted. The default value of \code{minweight=0.000001} returns 14 masspoints 
  for the default \code{numnodes=20} as in Aitkin, Francis & Hinde (2005).
}
\value{
 A list with two vectors: 
  \item{location}{locations of mass points}
  \item{weight}{masses}
}
\references{ 

Aitkin, M., Francis, B. and Hinde, J. (2005). Statistical Modelling in GLIM 4. 
Second Edition, Oxford Statistical Science Series, Oxford, UK. 

Lindsey, J. K. (1992). The Analysis of Stochastic Processes using GLIM.
Berlin: Springer-Verlag.

Skrondal, A. and Rabe-Hesketh, S. (2004). Generalized
latent variable modelling. Boca Raton: Chapman and Hall/CRC.

}
\author{
    Nick Sofroniou (2005) 
}

%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{
  \code{\link{alldist}}, \code{\link{allvc}}
}
\examples{
gqz(20, minweight=1e-14)
  # gives k=20 GH integration points. These are used in alldist  
  # and allvc as fixed mass point locations when working with 
  # option random.distribution='gq', and serve as EM starting points 
  # otherwise. 
}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
