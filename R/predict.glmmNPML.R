"predict.glmmNPML" <-
predict.glmmNPML<-function(object, newdata,  type="link", ...){

  if(missing(newdata)){ #Emp. Bayes Pred. (Aitkin, 96)
      N<-length(object$case.weights)
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

  } else  {
      m<-length(object$mass.points)
      k<-length(object$masses)
      Terms<-  delete.response(terms(object$formula))
      if (k==1){  # GLM
              X<-model.matrix(Terms, model.frame(Terms,newdata))
              dimnames(X)[[1]]<-dimnames(model.frame(Terms,newdata))[[1]]
              pred<-  X%*%matrix(object$coef[1:dim(X)[2]])
      }  else if (k==m){ #Overdispersion model
              X<-model.matrix(Terms, model.frame(Terms,newdata))[,-1,drop=FALSE]
              dimnames(X)[[1]]<-dimnames(model.frame(Terms,newdata))[[1]]
              if (dim(X)[2]!=0){
                    pred<- X%*%matrix(object$coef[1:dim(X)[2]])+ sum(object$masses*object$mass.points)
              } else {
                    pred<- rep(0, dim(X)[1])+ sum(object$masses*object$mass.points)
              }
      } else { #Random coefficient models
              X<-model.matrix(Terms, model.frame(Terms,newdata))[,-1,drop=FALSE]
              dimnames(X)[[1]]<-dimnames(model.frame(Terms,newdata))[[1]]
              object$mass.points<- ifelse(is.na(object$mass.points),0,object$mass.points)
              #r<-  names(newdata) %in% gsub('~','',object$random)[2]
              r<-  names(newdata) %in% object$Misc$mform   #28/02/06
              if(is.factor(newdata[,r])){newdata[,r]<-as.numeric(newdata[,r])-1}  #28/02/06
              if (dim(X)[2]!=0){
                  pred<- X%*%matrix(object$coef[1:dim(X)[2]])+ sum(object$masses*object$mass.points[1:k])+ newdata[,r]*sum(object$masses[1:k]*object$mass.points[(k+1):m])
                  } else {
                  pred<- rep(0, dim(X)[1])+ sum(object$masses*object$mass.points[1:k])+ newdata[,r]*sum(object$masses[1:k]*object$mass.points[(k+1):m])
              }
      }
      if (type=="link"){
             rpred<-as.vector(pred)
      } else {
             rpred<-as.vector( switch(object$family$link, "log"= exp(pred),
                                            "logit"=exp(pred)/(1+exp(pred)),
                                            "identity"=pred,
                                            "inverse"=1/pred,
                                            "probit"= pnorm(pred,0,1)))
             
      }
      names(rpred)<-dimnames(X)[[1]]
      return(round(rpred,digits=4))
  }

}

