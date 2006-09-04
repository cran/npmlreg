"summary.glmmNPML" <-
function(object,digits=max(3,getOption('digits')-3), ...){
  np <- length(object$coefficients)
  if (np > 0){
      m <- seq(1,np)[substr(attr(object$coefficients,'names'),1,4)=='MASS']
      mass.points <- object$coefficients[m]
      cat('\nCall: ',deparse(object$call),'\n\n')
      cat('Coefficients')
      cat(":\n")
      df.r <- object$lastglm$df.residual
       
      glm.dispersion <- if (any(object$family$family == c("poisson", 
            "binomial"))) 
            1
            else if (df.r > 0) {
                sum(object$lastglm$weights * object$lastglm$residuals^2, na.rm=TRUE)/df.r
            }
            else Inf
      #dispers     <-  sum(object$lastglm$weights * object$lastglm$residuals^2, na.rm=TRUE)/object$lastglm$df.residual
      lastglmsumm <-  summary.glm(object$lastglm, dispersion=glm.dispersion)
      fitcoef<-matrix(lastglmsumm$coeff[,1:3], ncol=3,dimnames= list(dimnames(lastglmsumm$coef)[[1]], c(dimnames(lastglmsumm$coeff)[[2]][1:2], "t value") ))  #03-08-06
      print(fitcoef)
  } else {
      cat('\nCall: ',deparse(object$call),'\n\n')
      cat("No coefficients. \n")
  }
 
  p <- object$masses
  names(p) <- paste('MASS',seq(1,ncol(object$post.prob)),sep='')
  dispersion <- 1    #now calculate dispersion in the sense of 'dispersion parameter':
  
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
 
  cat('-2 log L:\t   ',format(round(object$disparity,digits=1)))
  if (!is.null(object$post.prob)) cat('     Convergence at iteration ',round(object$EMiter,0))
  cat('\n')
  if (np >0){
      invisible(c("coefficients"=list(fitcoef), object[-1],list(dispersion=dispersion), list(lastglmsumm=lastglmsumm)))
  } else {  
      invisible(c("coefficients"=list(fitcoef), object[-1],list(dispersion=dispersion)) )
  }
}

