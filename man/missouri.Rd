\name{missouri}
\alias{missouri}
\docType{data}
\title{Missouri lung cancer data}
\description{
  Lung cancer mortality in the 84 largest Missouri cities, for males aged 45-54, 1972-1981. 
  Data presented in Tsutakawa (1985).
}
\usage{data(missouri)}
\format{
  A data frame with 84 observations on the following 2 variables.
  \describe{
    \item{\code{Size}}{population of the city.}
    \item{\code{Deaths}}{number of lung cancer deaths.}
  }
}
\details{
The data set was analyzed using a Poisson model with normal random effect in 
Tsutakawa (1985), and using a binomial logit model with unspecified random effect 
distribution  in Aitkin (1996b). Aitkin fitted this model with GLIM4. 
}
\source{
  Tsutakawa, R. (1985).
}
\references{
Aitkin, M. (1996b). Empirical Bayes shrinkage using posterior random effect means from nonparametric maximum likelihood estimation in general random effect models. Statistical Modelling: Proceedings of the 11th IWSM 1996, 87-94. 

Tsutakawa, R. (1985). Estimation of Cancer Mortalilty Rates: 
  A Bayesian Analysis of Small Frequencies.  Biometrics 41, 69-79. 
 }
\examples{
data(missouri)
alldist(Deaths~1, offset=log(Size), random=~1, k=2,
   family=poisson(link='log'), data=missouri)
}
\keyword{datasets}
