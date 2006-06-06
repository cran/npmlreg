"masspoint.classifier" <-
function(object){ 
    classif<-apply(object$post.prob, 1, which.max)
    names(classif)<-dimnames(object$data)[[1]]
    return(classif)
}  
 