\name{hosp}
\alias{hosp}
\docType{data}
\title{The Pennsylvanian Hospital Stay Data }
\description{
 The data, 25 observations, are a subset from a larger data set collected 
 on persons discharged from a selected Pennsylvania hospital as part of a 
 retrospective chart review of antibiotic use in hospitals (Towensend et al., 
 1979, Rosner, 2000). 

 
}
\usage{data(hosp)}
\format{
  A data frame with 25 observations on the following 9 variables. All variables 
  are given as numerical vectors.
  \describe{
    \item{\code{id}}{patient ID.}
    \item{\code{duration}}{the total number of days patients 
      spent in hospital.}
    \item{\code{age}}{age of patient in whole years.}
    \item{\code{sex}}{gender: 1=M, 2=F.}
    \item{\code{temp1}}{first temperature following admission.}
    \item{\code{wbc1}}{first WBC count (\eqn{\times 10^3}{x10^3}) following admission. [WBC= white blood cells]. }
    \item{\code{antib}}{received antibiotic: 1=yes, 2=no. }
    \item{\code{bact}}{received bacterial culture: 1=yes, 2=no. }
    \item{\code{serv}}{service: 1 =med., 2=surg.}
  }
}
%\details{
%See Townsend et al. (1979), Rosner (2000). 
%Don't confuse with the Barcelona 'Hospital stay data' aep in package {gamlss}.   
%}
\source{
 B. Rosner, Harvard University. 
}   

\section{Warnings}{
Don't confuse with the Barcelona 'Hospital stay data' \code{aep}  in package \pkg{gamlss}. 
}

\references{
Rosner, B. (2000). Fundamentals of Biostatistics. Thomson Learning, Duxbury, CA, USA. 

Townsend, T.R., Shapiro, M., Rosner, B., & Kass, E. H. (1979). Use of antimicrobial drugs in general hospitals. I. Description of population and definition of methods. Journal of Infectious Diseases 139 , 688-697. 
}
\examples{
data(hosp)
glm1<- glm(duration~age+temp1+wbc1, data=hosp, family=Gamma(link=log))
}
\keyword{datasets}
