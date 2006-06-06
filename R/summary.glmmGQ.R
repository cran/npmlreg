"summary.glmmGQ" <-
function(object, digits=max(3,getOption('digits')-3), ...){
  cat('\nCall: ',deparse(object$call),'\n\n')
  cat('Coefficients')
  cat(":\n")
  dispers<-  sum(object$weights * object$residuals^2, na.rm=TRUE)/object$df.residual
  print(summary.glm(object, dispersion=dispers)$coeff[,1:3]) #21-4-06
  #up to version 0.31: print(summary.glm(object)$coeff[,1:3])
  dispersion <- 1
  if (object$family$family=='gaussian'){
      dispersion <- (object$sdev$sdev)^2
      if (object$Misc$lambda<=1/length(object$masses)){
          cat('\nMLE of sigma:\t  ',format(signif(object$sdev$sdev,digits)),'\n')
      } else {
      cat('\nComponent standard deviations:\n'); s<- object$sdev$sdevk;  print.default(format(s,digits),print.gap=2,quote=FALSE)
      }
  }
  if (object$family$family=='Gamma'){
      dispersion <- 1/object$shape$shape
      if (object$Misc$lambda<=1/length(object$masses)){
          cat('\nMLE of shape parameter:\t  ',format(signif(object$shape$shape,digits)),'\n')
          } else {
              cat('\nComponent shape parameters:\n'); s<- object$shape$shapek; names(s)<- names(p);  print.default(format(s,digits),print.gap=2,quote=FALSE)
          }
  }
  cat('\n-2 log L:\t   ',format(round(object$Disparity,digits=1)),
        '     Convergence at iteration ',round(object$EMiter,0),"\n")
  invisible(c(object,list(dispersion=dispersion)))
}

