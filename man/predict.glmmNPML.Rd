\name{predict.glmmNPML}
\alias{predict.glmmNPML}
\alias{predict.glmmGQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction from objects of class glmmNPML or glmmGQ }
\description{
 The functions \code{alldist} and \code{allvc} produce objects of type \code{glmmGQ},
  if Gaussian quadrature (Hinde, 1982, \code{random.distribution="gq"} ) 
  was applied for computation, and objects of class \code{glmmNPML}, if 
  parameter estimation was carried out by nonparametric maximum likelihood 
  (Aitkin, 1996a, \code{random.distribution="np"} ). The functions presented here
   give predictions from those objects.  
}
\usage{
predict.glmmNPML(object, newdata, type = "link", ...)
predict.glmmGQ(object, newdata, type = "link", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a fitted object of class \code{glmmNPML} or \code{glmmGQ}. }
  \item{newdata}{a data frame with covariates from which prediction is desired. 
  If omitted, empirical Bayes predictions for the original data will be given.  }
  \item{type}{if set to \code{link}, the prediction is given on the linear predictor scale.
   If set to \code{response}, prediction is given on the scale of the responses. }
  \item{\dots}{further arguments which will mostly not have any effect (and are 
  included only to ensure compatibility 
  with the generic \code{predict()}- function.)  }
}
\details{
 The predicted values are obtained by 

\itemize{
\item Empirical Bayes (Aitkin, 1996b), if \code{newdata} has not been specified.
 That is, the prediction on the linear predictor scale is given by 
 \eqn{\sum{\eta_{ik}w_{ik}} }{sum(eta_ik * w_ik)}, 
whereby \eqn{\eta_{ik}}{eta_ik} are the fitted linear predictors, 
\eqn{w_{ik}}{w_ik} are the weights in the final iteration of the EM algorithm
 (corresponding to the posterior probability for observation \eqn{i} 
 to come from component \eqn{k} ), and the sum is taken over the number
  of components \eqn{k} for fixed \eqn{i}. 
\item the marginal model, if object is of class \code{glmmNPML} and 
\code{newdata} has been specified. That is, computation is identical as above, but
 with \eqn{w_{ik}}{w_ik} replaced by the masses \eqn{\pi_k}{pi_k} of the fitted model. 
\item the analytical expression for the marginal mean of the responses, 
if object is of class \code{glmmGQ} and \code{newdata} has been specified. 
See Aitkin et al. (2005), p. 459, for the formula. This method is only
 supported for the logarithmic link function, as otherwise no analytical
  expression for the marginal mean of the responses exists.  
}
It is sufficient to write \code{predict} instead of \code{predict.glmmNPML} or 
\code{predict.glmmGQ}, since the generic predict function provided in \R{} automatically selects the right 
 model class.

}
\value{
 A vector of predicted values.
 }
 
 
\section{Note}{The results of the generic \code{fitted()} method 
 correspond to \code{predict(object, type="response")}.  Note that, as we are 
 working with random effects, fitted values are never really `fitted' but rather 
 `predicted'.    
 }
 
\references{
Aitkin, M. (1996a). A general maximum likelihood analysis of overdispersion in generalized linear models. Statistics and Computing 6, 251-262. 

Aitkin, M. (1996b). Empirical Bayes shrinkage using posterior random effect means from nonparametric maximum likelihood estimation in general random effect models. Statistical Modelling: Proceedings of the 11th IWSM 1996, 87-94. 

Aitkin, M., Francis, B. and Hinde, J. (2005). Statistical Modelling in GLIM 4. Second Edition, Oxford Statistical Science Series, Oxford, UK. 

Hinde, J. (1982). Compound Poisson regression models. Lecture Notes in Statistics 14, 109-121. 

 }
\author{Jochen Einbeck and John Hinde (2006).}

\seealso{ 
\code{\link{alldist}}, \code{\link{allvc}}, \code{\link{predict}}
}
\examples{
 # Toxoplasmosis data:
    data(rainfall, package="forward")
    rainfall$x<-rainfall$Rain/1000
    toxo.0.3x<- alldist(cbind(Cases,Total-Cases)~1, random=~x,
          data=rainfall, k=3, family=binomial(link=logit))
    toxo.1.3x<- alldist(cbind(Cases,Total-Cases)~x, random=~x, 
          data=rainfall, k=3, family=binomial(link=logit))
    predict(toxo.0.3x, type="response", newdata=data.frame(x=2))
    # [1] 0.4608
    predict(toxo.1.3x, type="response", newdata=data.frame(x=2))
    # [1] 0.4608
    # gives the same result, as both models are equivalent and only differ
    # by a  parameter transformation.

# Fabric faults data:
    data(fabric, package="gamlss")
    names(fabric) 
    # [1] "leng" "y"    "x"    
    faults.g2<- alldist(y ~ x, family=poisson(link=log), random=~1, 
        data= fabric,k=2, random.distribution="gq") 
    predict(faults.g2, type="response",newdata=fabric[1:6,])
    # [1]  8.715805 10.354556 13.341242  5.856821 11.407828 13.938013
    # is not the same as
    predict(faults.g2, type="response")[1:6]
    # [1]  6.557786  7.046213 17.020242  7.288989 13.992591  9.533823
    # since in the first case prediction is done using the analytical 
    # mean of the marginal distribution, and in the second case  using the
    # individual posterior probabilities in an  empirical Bayes approach. 


}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{ regression }% __ONLY ONE__ keyword per line
