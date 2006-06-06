"tolfind" <-
function(formula,
                    random=~1,
                    family = gaussian(),
                    data ,
                    k = 4,
                    random.distribution="np",
                    offset,
                    weights,
                    na.action,
                    EMmaxit=500,
                    EMdev.change=0.001,
                    lambda=0,
                    damp=TRUE,
                    damp.power=1,
                    spike.protect=1,
                    sdev,
                    shape,
                    vc=FALSE,
                    plot.opt=1,
                    steps=15,
                    find.in.range=c(0.05,0.8),
                    verbose=FALSE,
                    noformat=FALSE,
                    ...)

{
  # JE/JH, 2005.

  call <- match.call()
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("`family' not recognized")
  }

  if (missing(data))
    data <- environment(formula)

  
  all.Disparities<- all.converged<-rep(0,steps)
  min.Disp <-min.Disp.conv <-10^8
  step.min<-step.min.conv<- 1

  if (k==1){stop("Please choose k > 1.")}
  if(missing(offset)) data$offset <- rep(0, length(data[,1])) else data$offset <-offset
  if(missing(weights))data$pweights<- rep(1, length(data[,1])) else data$pweights <-weights
  data<-data[!is.na(data$pweights) & ! is.na(data$offset),]
   if (!noformat){if (steps >8) par(mfrow=c(4,4), cex=0.5) else par(mfrow=c(3,3), cex=0.5,cex.axis=1.1 )}

  for (t in 1: steps){
     tol<-find.in.range[1]+ (find.in.range[2]-find.in.range[1])*t/steps
     if(vc==FALSE){ 
         npfit<- try(alldist(formula=formula,random=formula(random),family=family, data=data,k=k,random.distribution=random.distribution,  offset=data$offset,  weights=data$pweights, na.action=na.action,  tol=tol, EMmaxit=EMmaxit, EMdev.change=EMdev.change, lambda=lambda, damp=damp, damp.power=damp.power, spike.protect=spike.protect, sdev=sdev, shape=shape, plot.opt=plot.opt, verbose=verbose))         
         if(class(npfit)=="try-error"){
                cat("tolfind failed using tol=", tol, ". Possible reason: Wrong setting of option vc. Otherwise:  specify another range of tol values and try again. "); return()
         }  
         all.Disparities[t]<-  npfit$Disparity
         all.converged[t]<-  npfit$EMconverged
     } else { 
         vcfit<- try(allvc(formula=formula,random=formula(random),family=family, data=data,k=k,random.distribution=random.distribution,  offset=data$offset, weights=data$pweights, na.action=na.action, tol=tol, EMmaxit=EMmaxit,EMdev.change=EMdev.change, lambda=lambda, damp=damp, damp.power=damp.power, spike.protect=spike.protect, shape=shape, sdev=sdev, plot.opt=plot.opt, verbose=verbose))
         if(class(vcfit)=="try-error"){
                cat("tolfind failed using tol=", tol,  ". Possible reason: Wrong setting of option vc.  Otherwise: specify another range of tol values and try again. "); return()
         } 
         all.Disparities[t]<-  vcfit$Disparity
         all.converged[t]<-vcfit$EMconverged
     }
     
     if (all.Disparities[t]< min.Disp){min.Disp<- all.Disparities[t]; step.min<-t }
     if (all.Disparities[t]< min.Disp.conv && all.converged[t]){min.Disp.conv<- all.Disparities[t]; step.min.conv<-t }
  }
  
  tol.min<- find.in.range[1]+ (find.in.range[2]-find.in.range[1])*step.min/steps
  tol.min.conv<- find.in.range[1]+ (find.in.range[2]-find.in.range[1])*step.min.conv/steps

  npcolors<-2+ all.converged

 plot(find.in.range[1]+(find.in.range[2]-find.in.range[1])*(1:steps)/steps,all.Disparities,type="o",xlab="tol",ylab="Disparity",col=npcolors)
  segments(tol.min, min.Disp, tol.min, 1.1*min.Disp,col=4)
  cat("Minimal Disparity:", min.Disp, "at tol=", tol.min, "\n")

  if(max(all.converged)==1){
      cat("Minimal Disparity with EM converged:", min.Disp.conv, "at tol=", tol.min.conv, "\n")
  } else {
      cat(" No convergence achieved for any choice of tol.", "\n")
  }

  list("MinDisparity"=min.Disp.conv, "Mintol"=tol.min.conv, "AllDisparities"= all.Disparities , "Alltol"=  find.in.range[1]+ (find.in.range[2]-find.in.range[1])* (1:steps)/steps, "AllEMconverged"=all.converged==TRUE)
}

