\name{alldist}
\alias{alldist}
\alias{allvc}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ NPML estimation or Gaussian quadrature for overdispersed GLM's and variance component models}
\description{ Fits a random effect model using Gaussian quadrature (Hinde, 1982) or nonparametric maximum likelihood (Aitkin, 1996a).  
The function \code{alldist} is designed to account for overdispersion, while \code{allvc} fits variance component models. 
}
\usage{
alldist(formula, 
        random = ~1, 
        family = gaussian(), 
        data,
        k = 4, 
        random.distribution = "np", 
        tol = 0.5, 
        offset, 
        weights, 
        pluginz, 
        na.action, 
        EMmaxit = 500, 
        EMdev.change = 0.001, 
        lambda = 0, 
        damp = TRUE, 
        damp.power = 1, 
        spike.protect = 0, 
        sdev,
        shape, 
        plot.opt = 3, 
        verbose = TRUE,
        ...)
        
allvc(formula, 
        random = ~1, 
        family = gaussian(), 
        data, 
        k = 4, 
        random.distribution = "np", 
        tol = 0.5, 
        offset, 
        weights, 
        pluginz, 
        na.action, 
        EMmaxit = 500, 
        EMdev.change = 0.001, 
        lambda=0,
        damp = TRUE, 
        damp.power = 1, 
        spike.protect=0,
        sdev,
        shape, 
        plot.opt = 3, 
        verbose = TRUE,
        ...)        
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula defining the response and the fixed effects (e.g. \code{y ~ x}).   }
  \item{random}{a formula defining the random model. In the case of \code{alldist},  set \code{random = ~1} to model overdispersion, and for instance \code{random = ~x} to introcude a random coeffcient \code{x}.  
    In the case of \code{allvc},  set \code{random=~1|PSU} to model overdispersion on the upper level, where PSU is a \bold{factor} for the primary sampling units, e.g. groups, clusters, classes, or individuals in longitudinal data, and define random coefficents accordingly.}
  \item{family}{conditional distribution of responses. "gaussian", "poisson", "binomial", or "Gamma" can be set. If "gaussian" or "Gamma", then equal component dispersion parameters are assumed, except if the optional parameter lambda is modified.   }
  \item{data}{the data frame (mandatory, even if it is attached to the workspace!). }
  \item{k}{the number of mass points/integration points 
    (supported are up to 600 mass points).}
  \item{random.distribution}{the mixing distribution, Gaussian Quadrature (\code{gq}) or NPML (\code{np}) can be set.   }
  \item{tol}{the tol scalar (usually, \eqn{0<}\code{tol} \eqn{\le 1}{<= 1})   }
  \item{offset}{an optional offset to be included in the model.}
  \item{weights}{optional prior weights for the data.}
  \item{pluginz}{optional numerical vector of length \code{k} specifying the starting mass points of the EM algorithm.  }
  \item{na.action}{a function indicating what should happen when \code{NA}'s occur,
    with possible arguments \code{na.omit} and \code{na.fail}. The default is set
     by the \code{na.action} setting in \code{options()}. }
  \item{EMmaxit}{maximum number of EM iterations.   }
  \item{EMdev.change}{stops EM algorithm when deviance change falls below this value.  }
  \item{lambda}{only applicable for Gaussian and Gamma mixtures. If set, standard 
  deviations/ shape parameters are calculated smoothly across components via 
  a Aitchison-Aitken kernel (\code{\link{dkern}}) with parameter \code{lambda}. 
  The setting \code{lambda= 0} is automatically mapped to \code{lambda =1/k} and 
  corresponds to the case 'maximal smoothing' (i.e. equal component dispersion parameters), 
  while \code{lambda=1} means 'no smoothing' (unequal disp. param.)}
  \item{damp}{switches EM damping on or off.   }
  \item{damp.power}{steers degree of damping applied on dispersion parameter according 
  to formula \code{1-(1-tol)^(damp.power*iter+1)}, see Einbeck & Hinde (2005).  }
  \item{spike.protect}{protects algorithm to converge into likelihood spikes for 
    Gaussian and Gamma mixtures with unequal or smooth component standard deviations,  
    by stopping the EM algorithm if one of the component standard deviations 
    (shape parameters, resp.), divided by the fitted mass points, falls below
     (exceeds, resp.) a certain threshold, which is \code{0.000001*spike.protect} 
     (\code{10^6*spike.protect}, resp.) Setting \code{spike.protect=0} means 
     disabling the spike protection. If set, then \code{spike.protect=1}
     is recommended. Note that the displayed disparity may not be correct 
     when convergence is not achieved. This can be checked with \code{EMconverged}. }
  \item{sdev}{optional; specifies standard deviation for normally distributed response.
     If unspecified, it will be estimated from the data. }
  \item{shape}{optional; specifies shape parameter for gamma-distributed response.
     Setting \code{shape=1} gives an exponential distribution. If unspecified,
      it will be estimated from the data. }
  \item{plot.opt}{if equal to zero, then no graphical output is given. 
      For \code{plot.opt=1} the development of the disparity \eqn{-2\log L}{-2logL} over 
      iteration number is plotted, for \code{plot.opt=2} the EM trajectories are plotted,
         and for \code{plot.opt=3} both plots are shown.   }
  \item{verbose}{if set to \code{FALSE}, no printed output is given during 
    function execution. Useful for \code{tolfind}.   }   
  \item{\dots}{ generic options for the \code{glm} function. Not all options may
   be supported under any circumstances.}
}
\details{
  The nonparametric maximum likelihood (NPML) approach was introduced in Aitkin (1996) 
  as a tool to fit overdispersed generalized linear models. The idea is to approximate 
  the unknown and unspecified distribution of the random effect by a discrete mixture 
  of exponential family densities, leading to a simple expression of the marginal 
  likelihood which can then be maximized using a standard EM algorithm.
  
  Aitkin (1999) extended this method to generalized linear models with shared 
  random effects arising through variance component or repeated measures 
  structure. Applications are two-stage sample designs, when firstly the 
  primary sampling units (the upper-level units, e.g. classes) and then the 
  secondary sampling units (lower-level units, e.g. students) are selected, or 
  longitudinal data. Models of this type have also been referred to as multi-level 
  models (Goldstein, 2003). \code{allvc} is restricted to 2-level models. 

  The number of components \code{k} of the finite mixture has to be specified beforehand.
  When option \code{'gq'} is set, then Gauss-Hermite masses and mass points are used, 
  assuming implicitly a normally distributed random effect. 
  When option \code{'np'} is chosen, the EM algorithm uses the Gauss-Hermite masses 
  and mass points as starting points. The position of the starting points can 
  be concentrated or extended by setting \code{tol} smaller or larger than one,
  respectively. 

  Fitting random coefficient models (Aitkin, Francis & Hinde, 2005, pp. 474, p. 491) is 
  possible by specifying the random term explicitly. Note that the setting 
  \code{random= ~ x} gives a model with a random slope and a random intercept,
  that only one random coefficient can be specified, and that the option
   \code{random.distribution} is restricted to \code{np} in this case. 

  The weights have to be understood as frequency weights, i.e. setting all weights in 
  \code{alldist} equal to 2 will duplicate each data point and hence double the 
  disparity and deviance.  

  For \code{k} \eqn{\ge 70}{>= 70}, mass points with negligible mass
  (i.e. < 1e-55) are omitted.  The maximum number of 'real' mass points
  is then 240.
  
}
\value{

The function alldist produces an object of class \code{glmmNPML} 
(if \code{random.distributon} is set to \code{np})  or \code{glmmGQ} 
(\code{gq}).  Both objects contain the following 29 components: 

  \item{coefficients}{a named vector of coefficients (including the mass points). 
    In case of Gaussian quadrature, the coefficient given at \code{z}  
    corresponds to the standard deviation of the mixing distribution. }
  \item{residuals}{the difference between the true response and the emprical Bayes predictions.}
  \item{fitted.values}{the empirical Bayes predictions (Aitkin, 1996b) on the scale of the responses.}
  \item{family}{the `family' object used.}
  \item{linear.predictors}{the extended linear predictors \eqn{\hat{\eta}_{ik}}{eta_ik}. }
  \item{disparity}{the disparity (\code{-2logL}) of the fitted mixture regression model.}
  \item{deviance}{the deviance of the fitted mixture regression model.}
  \item{null.deviance}{The deviance for the null model (just containing an intercept), comparable with
          `deviance'.}
  \item{df.residual}{the residual degrees of freedom of the fitted model (including the random part).}
  \item{df.null}{the residual degrees of freedom for the null model.}
  \item{y}{the (extended) response vector.}
  \item{call}{the matched call.}
  \item{formula}{the formula supplied.}
  \item{random}{the random term of the model formula.}
  \item{data}{the data argument.} 
  \item{model}{the (extended) design matrix.}
  \item{weights}{the case weights initially supplied.} 
  \item{offset}{the offset initially supplied.}
  \item{mass.points}{the fitted mass points.}
  \item{masses}{the mixture probabilities corresponding to the mass points.}
  \item{sdev}{a list of the two elements \code{sdev$sdev} and \code{sdev$sdevk}. 
    The former is the estimated standard deviation of the
    Gaussian mixture components (estimated over all mixture components), and the latter 
    gives the unequal or smooth component-specific standard deviations. 
    All values are equal if \code{lambda=0}.}
  \item{shape}{a list of the two elements \code{shape$shape} and \code{shape$shapek}, 
    to be interpreted in analogy to sdev.}
  \item{rsdev}{estimated random effect standard deviation.}
  \item{post.prob}{a matrix of posteriori probabilities.}
  \item{post.int}{a vector of `posteriori intercepts' (as in Sofroniou et al. (2006)).}
  \item{ebp}{the empirical Bayes Predictions on the scale of the linear predictor. 
  For compatibility with older versions.}
  \item{EMiter}{gives the number of iterations of the EM algorithm. }
  \item{EMconverged}{logical value indicating if the EM algorithm converged. }
  \item{lastglm}{the fitted \code{glm} object from the last EM iteration.}
  \item{Misc}{contains additional information relevant for the summary and plot functions,
   in particular the disparity trend and the EM trajectories. }  
  
If a binomial model is specified by giving a two-column response, 
the weights returned by \code{weights} are the total numbers of cases 
(factored by the supplied case weights) and the component \code{y} 
of the result is the proportion of successes. 

As a by-product, \code{alldist} produces a plot showing the disparity
in dependence of the iteration number. Further, a plot with the EM trajectories
is given. The x-axis corresponds  to the iteration number, and the y-axis
to the value of the mass points at a particular iteration. 
This plot is not produced for GQ. 
}



\references{ 

Aitkin, M. and Francis, B. (1995). Fitting overdispersed generalized linear models by nonparametric maximum likelihood. GLIM Newsletter 25, 37-45. 

Aitkin, M. (1996a). A general maximum likelihood analysis of overdispersion in generalized linear models. Statistics and Computing 6, 251-262. 

Aitkin, M. (1996b). Empirical Bayes shrinkage using posterior random effect means from nonparametric maximum likelihood estimation in general random effect models. Statistical Modelling: Proceedings of the 11th IWSM 1996, 87-94. 

Aitkin, M. (1999). A general maximum likelihood analysis of variance components in generalized linear models. Biometrics 55, 117-128. 

Aitkin, M., Francis, B. and Hinde, J. (2005). Statistical Modelling in GLIM 4. Second Edition, Oxford Statistical Science Series, Oxford, UK. 

Einbeck, J. & Hinde, J. (2006). A note on NPML estimation for exponential family regression models with unspecified dispersion parameter. Austrian Journal of Statistics 35, 233-243. 

Goldstein, H. (2003). Multilevel Statistical Models (3rd edition). Arnold, London, UK.

Hinde, J. (1982). Compound Poisson regression models. Lecture Notes in Statistics 14, 109-121. 

Sofroniou, N., Einbeck, J., and Hinde, J. (2006). Analyzing Irish suicide rates with mixture models. Proceedings of the 21st International Workshop on Statistical Modelling in Galway, Ireland, 2006. 

}

\author{Originally translated from the GLIM 4 functions \code{alldist} and 
\code{allvc} (Aitkin & Francis, 1995) to \R by Ross Darnell (2002). Modified, 
extended, and prepared for publication by Jochen Einbeck & John Hinde (2006). 

 }
\note{ 
In contrast to the GLIM 4 version, this \R implementation 
  uses for Gaussian and Gamma mixtures by default a damping procedure in the
  first cycles of the EM algorithm (Einbeck & Hinde, 2006), which stabilizes
  the algorithm and makes it less sensitive to the optimal 
  choice of \code{tol}. If \code{tol} is very small
  (i.e. less than 0.1), it can be useful  to set \code{damp.power} to values 
  larger than 1 in order to accelerate convergence. 
  Do not use \code{damp.power=0}, as this would mean permanent damping during EM. 
  Using the option \code{pluginz}, one  can to some extent circumvent the 
  necessity to  specify \code{tol} by giving the starting points explicitly. 
  However, when using \code{pluginz} for normal or gamma-distributed response, 
  damping will be strictly necessary to ensure that the imposed starting points
  don't get blurred immediately due to initial fluctuations, implying that 
  \code{tol} still plays a role in this case. 
}

\seealso{ \code{\link{glm}}, \code{\link{summary.glmmNPML}}, 
\code{\link{predict.glmmNPML}} \code{\link{family.glmmNPML}}, 
\code{\link{plot.glmmNPML}}.
}
\examples{

# The first three examples (galaxy data, toxoplasmosis data , fabric faults) 
# are based on GLIM examples in Aitkin et al. (2005), and the forth example using
# the Hospital-Stay-Data (Rosner, 2000) is taken from Einbeck & Hinde (2006).
# The fifth data example using the Oxford boys is again inspired by Aitkin et al. (2005).
# The sixth example on Irish suicide rates is taken from Sofroniou et al. (2006).
  

# The galaxy data   
  data(galaxies, package="MASS")
  gal<-as.data.frame(galaxies)
  galaxy.np6 <- alldist(galaxies/1000~1, random=~1, random.distribution="np", 
      data=gal, k=6)
  galaxy.np8u <- alldist(galaxies/1000~1, random=~1, random.distribution="np", 
      data=gal, k=8, lambda=0.99)
  round(galaxy.np8u$sdev$sdevk, digits=3)
  #[1] 0.906 0.435 0.218 0.676 1.205 0.216 0.412 0.295

# The toxoplasmosis data 
  data(rainfall, package="forward")
  rainfall$x<-rainfall$Rain/1000  
  rainfall$x2<- rainfall$x^2; rainfall$x3<- rainfall$x^3
  toxo.np3<- alldist(cbind(Cases,Total-Cases) ~ x+x2+x3, random=~1, 
      random.distribution="np", family=binomial(link=logit), data=rainfall, k=3)
  toxo.np3x<- alldist(cbind(Cases,Total-Cases) ~ x, random=~x, 
      random.distribution="np", family=binomial(link=logit), data=rainfall, k=3)
  #is the same as 
  toxo.np3x<- alldist(Cases/Total ~ x, random = ~x, weights=Total, 
      family=binomial(link=logit), data=rainfall, k=3)
  #or
  toxo.np3x<-update(toxo.np3, .~.-x2-x3, random = ~x)

# The fabric faults data
  data(fabric, package="gamlss")
  coefficients(alldist(y ~ x, random=~1, family=poisson(link=log), 
      random.distribution="gq", data= fabric, k=3, verbose=FALSE))
  #(Intercept)           x           z 
  # -3.3088663   0.8488060   0.3574909
  
# The Pennsylvanian hospital stay data
  data(hosp)
  fitnp3<-  alldist(duration~age+temp1, data=hosp, k=3, family=Gamma(link=log),
      tol=0.5) 
  fitnp3$shape$shape
  #[1] 50.75232
  fitnp3<-  alldist(duration~age+temp1, data=hosp, k=3, family=Gamma(link=log),
      tol=0.5, lambda=0.9) 
  fitnp3$shape$shapek
  #[1]  49.03108  42.79532 126.64046 
  
# The Oxford boys data
  data(Oxboys, package="nlme")
  Oxboys$boy <- gl(26,9)
  allvc(height~age, random=~1|boy, data=Oxboys, random.distribution='gq', k=20)
  allvc(height~age, random=~1|boy, data=Oxboys,random.distribution='np',k=8) 
  #with random coefficients:
  allvc(height~age,random=~age|boy, data=Oxboys, random.distribution='np', k=8)
    
# Irish suicide data
  data(irlsuicide)
  # Crude rate model:
  crude<- allvc(death~sex* age, random=~1|ID, offset=log(pop), 
      k=3, data=irlsuicide, family=poisson) 
  crude$disparity 
  # [1] 654.021
  # Relative risk model:
  relrisk<- allvc(death~1, random=~1|ID, offset=log(expected), 
      k=3, data=irlsuicide, family=poisson) 
  relrisk$disparity    
  # [1] 656.4955
  
}
\keyword{models}% at least one, from doc/KEYWORDS
\keyword{regression}% __ONLY ONE__ keyword per line
