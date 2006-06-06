###################################################
### chunk number 1: 
###################################################
data(galaxies, package="MASS")
galaxies[78]<-26960
gal<-as.data.frame(galaxies)


###################################################
### chunk number 2: 
###################################################
gal$v1000<- gal$galaxies/1000
gal$v1000


###################################################
### chunk number 3: 
###################################################
library(npmlreg)


###################################################
### chunk number 4: 
###################################################
glm(v1000~1,data=gal)


###################################################
### chunk number 5: 
###################################################
(galaxy.np1 <- alldist(v1000~1,random=~1,random.distribution='np',k=1,data=gal))


###################################################
### chunk number 6: 
###################################################
galaxy.np1$Dev


###################################################
### chunk number 7: 
###################################################
(galaxy.np2 <- alldist(v1000~1,random=~1,random.distribution='np',k=2,data=gal))
(galaxy.np3 <- alldist(v1000~1,random=~1,random.distribution='np',k=3,data=gal))
(galaxy.np4 <- alldist(v1000~1,random=~1,random.distribution='np',k=4,data=gal))


###################################################
### chunk number 8: 
###################################################
plot(galaxy.np4, plot.opt=3)


###################################################
### chunk number 9: 
###################################################
(galaxy.np5 <- alldist(v1000~1,random=~1,k=5,data=gal, verbose=FALSE))$Disp
(galaxy.np6 <- alldist(v1000~1,random=~1,k=6,tol=0.2,data=gal,verbose=FALSE))$Disp
(galaxy.np7 <- alldist(v1000~1,random=~1,k=7,tol=0.12,data=gal,verbose=FALSE))$Disp
(galaxy.np8 <- alldist(v1000~1,random=~1,k=8,tol=0.2,data=gal,verbose=FALSE))$Disp
(galaxy.np9 <- alldist(v1000~1,random=~1,k=9,tol=0.06,data=gal,verbose=FALSE))$Disp


###################################################
### chunk number 10: 
###################################################
summary(galaxy.np4u <- alldist(v1000~1, random=~1, k=4, tol=0.5, data=gal, lambda=1, verbose=FALSE))


###################################################
### chunk number 11:  eval=FALSE
###################################################
## plot(galaxy.np4u, plot.opt=15, height=5)


###################################################
### chunk number 12: 
###################################################
plot(galaxy.np4u, plot.opt=15)


###################################################
### chunk number 13: 
###################################################
(galaxy.np8us <- alldist(v1000~1, random=~1, k=8, tol=0.5, data=gal, lambda=1, verbose=FALSE, spike.protect=TRUE))
galaxy.np8us$sdev$sdevk


###################################################
### chunk number 14: 
###################################################
(galaxy.np8ud <- alldist(v1000~1, random=~1, k=8, tol=0.5, data=gal, lambda=0.99))
galaxy.np8ud$sdev$sdevk


###################################################
### chunk number 15: 
###################################################
par(mfrow=c(1,1), cex=0.65)
tolfind(v1000~1, random=~1, k=8, data=gal, lambda=1, find.in.range=c(0.0,0.6), steps=12, plot.opt=0, verbose=FALSE, noformat=TRUE)[c(3,4)]


###################################################
### chunk number 16: 
###################################################
data(fabric, package="gamlss")
(faults0 <- glm(y ~ 1, family=poisson(link=log),data=fabric))
(faults1 <- glm(y ~ x, family=poisson(link=log),data=fabric)) 


###################################################
### chunk number 17: 
###################################################
(faults.g1<- alldist(y ~ x, family=poisson(link=log), random=~1, data= fabric,k=1, random.distribution="gq")) 
(faults.g2<- alldist(y ~ x, family=poisson(link=log), random=~1, data= fabric,k=2, random.distribution="gq")) 
(faults.g3<- alldist(y ~ x, family=poisson(link=log), random=~1, data= fabric,k=3, random.distribution="gq",verbose=F)) 


###################################################
### chunk number 18: 
###################################################
faults.g1$Dev


###################################################
### chunk number 19: 
###################################################
(faults.np2<- alldist(y ~ x, family=poisson(link=log), random=~1, data= fabric,k=2, random.distribution="np")) 
(faults.np3<- alldist(y ~ x, family=poisson(link=log), random=~1, data= fabric,k=3, random.distribution="np",verbose=F)) 


###################################################
### chunk number 20: 
###################################################
predict(faults.g2, type="response",newdata=fabric[1:6,])


###################################################
### chunk number 21: 
###################################################
predict(faults.g2, type="response")[1:6]


###################################################
### chunk number 22: 
###################################################
data(rainfall, package="forward")
rainfall$x<-rainfall$Rain/1000  
rainfall$x2<- rainfall$x^2; rainfall$x3<- rainfall$x^3


###################################################
### chunk number 23: 
###################################################
 (toxo.np<- alldist(cbind(Cases,Total-Cases)~1, random=~1, data=rainfall, k=3, family=binomial(link=logit)))


###################################################
### chunk number 24: 
###################################################
 toxo.np$Disparity


###################################################
### chunk number 25: 
###################################################
 (toxo.npx<- alldist(cbind(Cases,Total-Cases)~x, random=~1, data=rainfall, k=3, family=binomial(link=logit)))


###################################################
### chunk number 26: 
###################################################
 (toxo.npxx<- alldist(cbind(Cases,Total-Cases)~x, random=~x, data=rainfall, k=3, family=binomial(link=logit)))


###################################################
### chunk number 27: 
###################################################
   round(t(toxo.np$post.prob),digits=2)
   plot(toxo.np, plot.opt=8)


###################################################
### chunk number 28: 
###################################################
round(toxo.ebp<-toxo.np$ebp,digits=3)
round(exp(toxo.ebp)/(1+exp(toxo.ebp)),digits=4)


###################################################
### chunk number 29: 
###################################################
predict(toxo.np, type="response")


###################################################
### chunk number 30: 
###################################################
predict(toxo.npx,type="response",newdata=data.frame(x=2))


###################################################
### chunk number 31: 
###################################################
data(hosp)
(fitnp3<-  alldist(duration~age+temp1, data=hosp,k=3, family=Gamma(link=log),tol=0.2)) 


###################################################
### chunk number 32: 
###################################################
 fitnp3$shape


###################################################
### chunk number 33: 
###################################################
 (fitnp3e<-  alldist(duration~age+temp1, data=hosp,k=3, family=Gamma(link=log),tol=0.2,shape=1))


###################################################
### chunk number 34: 
###################################################
 data(Oxboys, package = "nlme")
 Oxboys$boy <- gl(26,9) 
 plot(Oxboys$age[Oxboys$boy==1],Oxboys$height[Oxboys$boy==1],ylim=c(125,175),type='b',pch=1,xlab='age',ylab='height')
 for (i in 2:nlevels(Oxboys$Subject)){lines(Oxboys$age[Oxboys$boy==i],Oxboys$height[Oxboys$boy==i], pch=1,type='b',col=i)}


###################################################
### chunk number 35: 
###################################################
 (Oxboys.g20 <- allvc(height~age,random=~1|boy,data=Oxboys,random.distribution='gq',k=20))


###################################################
### chunk number 36: 
###################################################
 (Oxboys.np7 <- allvc(height~age,random=~1|boy,data=Oxboys,random.distribution='np',k=7))
 (Oxboys.np8 <- allvc(height~age,random=~1|boy,data=Oxboys,random.distribution='np',k=8)) 


###################################################
### chunk number 37: 
###################################################
 plot(Oxboys.np8, plot.opt=2)


###################################################
### chunk number 38: 
###################################################
 (Oxboys.np8s <- allvc(height~age,random=~age|boy,data=Oxboys,random.distribution='np',k=8))


###################################################
### chunk number 39: 
###################################################
  Oxboys.np8$Disp-Oxboys.np8s$Disp


###################################################
### chunk number 40: 
###################################################
 plot(Oxboys.np8, plot.opt=2)


###################################################
### chunk number 41: 
###################################################
 data(irlsuicide)


###################################################
### chunk number 42: 
###################################################
citation(package="npmlreg")


###################################################
### chunk number 43: 
###################################################
ls("package:npmlreg")


