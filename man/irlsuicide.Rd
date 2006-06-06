\name{irlsuicide}
\alias{irlsuicide}
\docType{data}
\title{Irish Suicide Data}
\description{
    Suicide Rates in the Republic of Ireland 1989-1998.
}
\usage{data(irlsuicide)}
\format{
  A data frame with 104 observations on the following 8 variables.
  \describe{
    \item{\code{Region}}{a factor with levels \code{Cork }, \code{Dublin }, \code{EHB - Dub.}, \code{Galway}, \code{Lim.}, \code{Mid HB}, \code{MWHB-Lim.}, \code{NEHB}, \code{NWHB}, \code{SEHB-Wat.}, \code{SHB-Cork}, \code{Waterf.}, \code{WHB-Gal.}.}
    \item{\code{ID}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} correspomnding to Regions.}
    \item{\code{pop}}{a numeric vector giving the population sizes (estimated for 1994).}
    \item{\code{death}}{a numeric vector giving the total number of deaths.}
    \item{\code{sex}}{a factor for gender with levels \code{0} (female) and \code{1} (male).}
    \item{\code{age}}{a factor for age with levels \code{1} (0-29),  \code{2} (30-39), \code{3} (40-59), \code{4} (60+ years).}
    \item{\code{smr}}{a numeric vector with standardized mortality  ratios (SMRs) }
    \item{\code{expected}}{a numeric vector with `expected' number of cases obtained from a reference population (Ahlborn, 1993).}
  }
}
\details{
The data set is  examined in Sofroniou et al. (2006), using a variance component model 
with regions as upper level.   
  
}
\source{
  Institute of Public Health in Ireland (2005). All
     Ireland Mortality Database. Retrieved August 8, 2005,
     from \url{http://mapserver1.cdc-ni.com/iph/index.htm}.
}
\references{
      Ahlborn, A.,  (1993). Biostatistics for Epidemiologists. Boca Raton: Lewis Publishers.
      
      Sofroniou, N., Einbeck, J., and Hinde, J. (2006). Analyzing Irish Suicide Rates with Mixture Models.
       Proceedings of the 21st Workshop on Statistical Modelling in Galway, Ireland, 2006. 
}
\examples{
data(irlsuicide)
library(lattice)
trellis.device(color=FALSE)
plot2age<-rep(gl(4,2),13)
xyplot(irlsuicide$death/irlsuicide$pop~plot2age|irlsuicide$Region, 
    pch=(1+(irlsuicide$sex==1)),xlab="age",ylab="Crude rates")


}
\keyword{datasets}
