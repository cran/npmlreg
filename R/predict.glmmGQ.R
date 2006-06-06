"predict.glmmGQ" <-
function(object, newdata,  type="link", ...){
  N<-length(object$case.weights)
  if (missing(newdata)){#Emp. Bayes Pred. (Aitkin, 96)
      k<-length(object$masses)
      ebp<-apply(object$post.prob*matrix(object$linear.predictor,N,k,byrow=FALSE),1,sum)

      if (type=="link") {
          return(round(ebp,digits=4))
      } else {
          rebp<- switch(object$family$link, "log"= exp(ebp),
                                            "logit"=exp(ebp)/(1+exp(ebp)),
                                            "identity"=ebp,
                                            "inverse"=1/ebp,
                                            "probit"= pnorm(ebp,0,1))
          return(round(rebp,digits=4))
      }
  } else {      # Analytical mean of compounded model, see Aitkin, Francis, Hinde 2005, p. 459.
      if (object$family$link!="log"){
          stop("Prediction for objetcs of class glmmGQ only supported for log link")
      }
      Terms<-  delete.response(terms(object$formula))
      X<-model.matrix(Terms, model.frame(Terms,newdata))#[,-1,drop=FALSE]
      pred<-    X%*%matrix(object$coef[1:dim(X)[2]])+1/2* (object$coef[length(object$coef)])^2
      if (type=="link"){
          rpred<-as.vector(pred)
      } else {
          rpred<- exp(as.vector(pred))
      }
      names(rpred)<-dimnames(X)[[1]]
      return(round(rpred,digits=4))
  }
}
