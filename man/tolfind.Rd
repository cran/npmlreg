\name{tolfind}
\alias{tolfind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Grid search over tol for NPML estimation of (generalized) random effect models}
\description{
  Performs a grid search to select the parameter  \code{tol}, 
  which is a tuning parameter for starting point selection of the EM algorithm 
  for NPML estimation (see e.g. Aitkin, Hinde & Francis, 2005, p. 418)
}
\usage{
tolfind(formula, 
        random = ~1, 
        family = gaussian(), 
        data, 
        k = 4, 
        random.distribution="np",
        offset, 
        weights, 
        na.action, 
        EMmaxit = 500, 
        EMdev.change = 0.001, 
        lambda = 0, 
        damp = TRUE, 
        damp.power = 1, 
        spike.protect = 1, 
        sdev,
        shape, 
        plot.opt = 1, 
        steps = 15, 
        find.in.range = c(0.05, 0.8), 
        verbose = FALSE, 
        noformat = FALSE,
        ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula defining the response and the fixed effects (e.g. \code{y ~ x}). }
  \item{random}{a formula defining the random model. Set \code{random=~1}
       to model overdispersion. }
  \item{family}{conditional distribution of responses. "gaussian", "poisson", "binomial", or "Gamma" can be set.}
  \item{data}{the data frame (mandatory, even if it is attached to the workspace!).  }
  \item{k}{the number of mass points/integration points (supported are 
      up to 30 mass points). }
    \item{random.distribution}{the mixing distribution, Gaussian Quadrature 
      (\code{gq}) or NPML (\code{np}) can be set.   }    
  \item{offset}{an optional offset to be included in the model.}
  \item{weights}{optional prior weights for the data.}
  \item{na.action}{a function indicating what should happen when \code{NA}'s occur,
    with possible arguments \code{na.omit} and \code{na.fail}. The default is 
    set by the \code{na.action} setting in \code{options()} .   }
   \item{EMmaxit}{maximum number of EM iterations.  }
  \item{EMdev.change}{stops EM algorithm when deviance change falls below this value.  }
  \item{lambda}{see the help file for \code{alldist}. }
  \item{damp}{ switches EM damping on or off.}
  \item{damp.power}{steers degree of damping. }
  \item{spike.protect}{see the help file for \code{alldist}. For unequal or 
    smooth component dispersion parameters, the setting \code{spike.protect=1}
    is strongly recommended.  }
  \item{sdev}{optional fixed standard deviation for normal mixture.}
  \item{shape}{optional fixed shape parameter for Gamma mixture.}
   \item{plot.opt}{For \code{plot.opt=1} the EM trajectories are plotted, 
    for \code{plot.opt=2} the development of the disparity \eqn{-2\log L}{-2logL} over 
    iteration number is plotted, for \code{plot.opt=3} both plots are shown, 
    and for \code{plot.opt=0} none of them. }
  \item{steps}{number of grid points for the search of \code{tol}.}
  \item{find.in.range}{range for the search of \code{tol}.  }
  \item{verbose}{If set to \code{FALSE}, no printed output is given during 
    execution of \code{alldist} or \code{allvc}.  }
  \item{noformat}{If \code{TRUE}, then any formatting of the plots is omitted.}  
  \item{\dots}{further arguments which will be ignored. }
}
\details{
The EM algorithm for NPML estimation (Aitkin, 1996) uses the Gauss-Hermite masses 
and mass points as starting  points. The position of the starting points can be 
concentrated or extended by setting \code{tol} smaller or larger than 1, 
respectively. The tuning parameter \code{tol} is, as in GLIM4, responsible for this scaling. 
A careful selection of \code{tol} may be necessary for some data sets. 
The reason  is that NPML has a tendency to get stuck in local maxima, 
as the log-likelihhod function is not concave for fixed \code{k}  (Boehning, 1999).

For Gaussian and Gamma mixtures this R implementation uses by default a damping 
procedure  in the first cycles of the EM algorithm (Einbeck & Hinde, 2006), 
which stabilizes the algorithm and  makes it less sensitive to the optimal choice 
of \code{tol}.  Application of  \code{tolfind} to check that the optimal  solution has 
not been overlooked may  nevertheless be advisable. 

\code{tolfind} works for \code{alldist} and \code{allvc}. The \code{tolfind} function 
is mainly designed for NPML (\code{random.distribution="np"}). It
can also be applied to Gaussian Quadrature (\code{random.distribution="gq"}), 
though \code{tol} is of little importance for this and primarily influences 
the speed of convergence.
 

}
\value{
  A list of 5 items: 
  
 \item{MinDisparity}{the minimal disparity achieved (for which EM converged).}
 \item{Mintol}{the \code{tol} value at which this disparity is achieved.}
 \item{AllDisparities}{a vector containing all disparities calculated on the grid.}
 \item{Alltol}{all corresponding \code{tol} values making up the grid.}
 \item{AllEMconverged}{a vector of Booleans indicating 
 if EM converged for the particular \code{tol} values.}
  }
\references{ 

   Aitkin, M. (1996). A general maximum likelihood analysis of overdispersion in generalized linear models. Statistics and Computing 6 , 251-262. 

Aitkin, M., Francis, B. and Hinde, J. (2005). Statistical Modelling in GLIM 4. Second Edition, Oxford Statistical Science Series, Oxford, UK. 

Böhning, D. (1999). Computer-Assisted Analysis of Mixtures and Applications. Meta-Analysis, Disease Mapping and others. Chapman & Hall / CRC, Boca Raton, FL, USA.

Einbeck, J. & Hinde, J. (2006). A note on NPML estimation for exponential family regression models with unspecified dispersion parameter. Austrian Journal of Statistics 35, 233-243. 
}

\author{Jochen Einbeck & John Hinde (2006). 

}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{alldist}}, \code{\link{allvc}} }
\examples{
  data(galaxies, package="MASS")
  gal<-as.data.frame(galaxies)
  tolfind(galaxies/1000~1, random=~1, k=5, data=gal, lambda=1, damp=TRUE, 
      find.in.range=c(0,1), steps=10) 
  # Minimal Disparity: 380.1444 at tol= 0.5 
 }
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
