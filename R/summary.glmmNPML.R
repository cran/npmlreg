"summary.glmmNPML" <-
function(object,digits=max(3,getOption('digits')-3), ...){
  np <- length(object$coefficients)
  m <- seq(1,np)[substr(attr(object$coefficients,'names'),1,4)=='MASS']
  mass.points <- object$coefficients[m]
  cat('\nCall: ',deparse(object$call),'\n\n')
  cat('Coefficients')
  cat(":\n")
  dispers<-  sum(object$weights * object$residuals^2, na.rm=TRUE)/object$df.residual
  print(summary.glm(object, dispersion=dispers)$coeff[,1:3])    #21-04-06
  #These 2 lines above are necessary, as in some extreme cases the calculation of the dispersion inside summary.glm
  #fails, since $weights may be zero and $residuals at the same time NA. #up to v. 0.31: print(summary.glm(object)$coeff[,1:3])
  p <- object$masses
  names(p) <- paste('MASS',seq(1,ncol(object$post.prob)),sep='')
  dispersion <- 1
  
  if (object$family$family=='gaussian'){
      dispersion <- (object$sdev$sdev)^2
      if (object$Misc$lambda<=1/length(object$masses)){
          cat('\nMLE of sigma:\t  ',format(signif(object$sdev$sdev,digits)),'\n')
      } else {
          cat('\nComponent standard deviations:\n'); s<- object$sdev$sdevk; names(s)<- names(p);  print.default(format(s,digits),print.gap=2,quote=FALSE)
      }
  }
  if (object$family$family=='Gamma'){
      dispersion <- 1/object$shape$shape
      if (object$Misc$lambda<=1/length(object$masses)){cat('\nMLE of shape parameter:\t  ',format(signif(object$shape$shape,digits)),'\n')
      } else {
          cat('\nComponent shape parameters:\n'); s<- object$shape$shapek; names(s)<- names(p);  print.default(format(s,digits),print.gap=2,quote=FALSE)
      }
  }
  
  cat('\nMixture proportions:\n')
  print.default(format(p,digits),print.gap=2,quote=FALSE)
 
  cat('-2 log L:\t   ',format(round(object$Disparity,digits=1)))
  if (!is.null(object$post.prob)) cat('     Convergence at iteration ',round(object$EMiter,0))
  cat('\n')
  invisible(c(object,list(dispersion=dispersion)))
}

