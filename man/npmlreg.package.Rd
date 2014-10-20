\name{npmlreg-package}
\alias{npmlreg-package}
\alias{npmlreg}
\docType{package}
\title{
Nonparametric maximum likelihood estimation for random effect models
}
\description{
Nonparametric maximum likelihood estimation or Gaussian quadrature 
    for overdispersed generalized linear models and variance component models. 
    The main functions  are \code{\link{alldist}} and \code{\link{allvc}}.
}
\details{
\tabular{ll}{
Package: \tab npmlreg\cr
Type: \tab Package\cr
License: \tab GPL version 2 or newer\cr
}

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

For details on  the GNU General Public License
see \url{http://www.gnu.org/copyleft/gpl.html} or   
write to the Free Software Foundation, Inc., 51 Franklin Street, 
Fifth Floor, Boston, MA  02110-1301, USA. 
}

\author{
Jochen Einbeck, Ross Darnell and John Hinde.

Maintainer: Jochen Einbeck <jochen.einbeck@durham.ac.uk>
}

\section{Acknowledgments}{
This \R package is based on several GLIM4 macros originally written by 
Murray Aitkin and Brian Francis.  The authors are also grateful to 
Nick Sofroniou for retrieving the suicide data and providing the function \code{gqz}.

The work on this \R package was supported by Science
Foundation Ireland Basic Research Grant 04/BR/M0051.
}


\references{

Aitkin, M., Francis, B. and Hinde, J. (2009). Statistical Modelling in R. 
Oxford Statistical Science Series, Oxford, UK. 

Einbeck, J., and Hinde, J.: Nonparametric maximum likelihood estimation
 for random effect models in \R. Vignette to \R package  \pkg{npmlreg}. 
Type \code{vignette("npmlreg-v")} to open it. 


}
\keyword{models}
\keyword{regression}

\seealso{
\code{\link{glm}}
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
