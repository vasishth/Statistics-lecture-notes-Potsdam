### R code from vignette source 'lecture3.Rnw'

###################################################
### code chunk number 1: lecture3.Rnw:98-127
###################################################
## function for plotting type 1 error.
plot.type1.error<-function(x,
                           x.min,
                           x.max,
                           qnts,
                           mean,
                           sd,
                           gray.level,main,show.legend=TRUE){

        plot(x,dnorm(x,mean,sd), 
                     type = "l",xlab="",ylab="",main=main)
        abline(h = 0)

## left side    
    x1 = seq(x.min, qnorm(qnts[1]), qnts[1]/5)
    y1 = dnorm(x1, mean, sd)

    polygon(c(x1, rev(x1)), 
            c(rep(0, length(x1)), rev(y1)), 
            col = gray.level)

## right side            
    x1 = seq(qnorm(qnts[2]), x.max, qnts[1]/5)
    y1 = dnorm(x1, mean, sd)
    polygon(c(x1, rev(x1)), 
            c(rep(0, length(x1)), rev(y1)), 
            col = gray.level)
if(show.legend==TRUE){legend(2,0.3, legend="Type I error",fill=gray.level,cex=1)}
	}


###################################################
### code chunk number 2: lecture3.Rnw:135-173
###################################################
plot.type1type2.error<-function(x,
                           x.min,
                           x.max,
                           qnts,
                           mean.null,
                           mean.true,
                           sd,
                           gray1,
                           gray2,main,show.legend=TRUE){
    	## the reality:
    	plot(x, dnorm(x,mean.true,sd), type = "l",ylab="",xlab="",main=main)
    	## null hypothesis distribution:
      lines(x,dnorm(x,mean.null,sd),col="black") 
      abline(h = 0)
    
    ## plot Type II error region: 
 
    	x1 = seq(qnorm(qnts[1]), x.max, qnts[1]/5)
        y1 = dnorm(x1, mean.true, sd) 
 
      polygon(c(x1, rev(x1)), 
              c(rep(0, length(x1)), 
              rev(y1)), col = gray2)

    ## plot Type I error region assuming alpha 0.05:

    x1 = seq(x.min, qnorm(qnts[1]), qnts[1]/5)
    y1 = dnorm(x1, mean.null, sd)
    polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)), col = gray1)

    x1 = seq(qnorm(qnts[2]), x.max, qnts[1]/5)
    y1 = dnorm(x1, mean.null, sd) ## changed
    polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)), col = gray1)

if(show.legend==TRUE){
    legend(2,0.3, legend=c("Type I error","Type II error"),
    fill=c(gray1,gray2),cex=1)}
}   


###################################################
### code chunk number 3: lecture3.Rnw:179-215
###################################################

shadenormal2<- 
function (plot.only.type1=TRUE,
          alpha=0.05,
          gray1=gray(0.3), ## type I shading
          gray2=gray(0.7), ## type II shading
          x.min=-6,
          x.max=abs(x.min),
          x = seq(x.min, x.max, 0.01),
          mean.null=0,
          mean.true=-2,
          sd=1,main="",show.legend=TRUE) 
{

    qnt.lower<-alpha/2
    qnt.upper<-1-qnt.lower
    qnts<-c(qnt.lower,qnt.upper)
 
    if(plot.only.type1==TRUE){

     plot.type1.error(x,x.min,x.max,qnts,mean.null,sd,
     gray1,main,show.legend)     

    } else { ## plot type I and type II error regions
      
   plot.type1type2.error(x,
                         x.min,
                         x.max,
                         qnts,
                         mean.null,
                         mean.true,
                         sd,
                         gray1,
                         gray2,main,show.legend)     
     }
}


###################################################
### code chunk number 4: nullhyprejectionregion
###################################################
shadenormal2(plot.only.type1=TRUE)


###################################################
### code chunk number 5: lecture3.Rnw:224-225
###################################################
shadenormal2(plot.only.type1=TRUE)


###################################################
### code chunk number 6: nullvstrue
###################################################
shadenormal2(plot.only.type1=TRUE)
xvals <- seq(-6,6,.1)
lines(xvals,dnorm(xvals,mean=-2,sd=1),lwd=2)


###################################################
### code chunk number 7: lecture3.Rnw:252-253
###################################################
shadenormal2(plot.only.type1=TRUE)
xvals <- seq(-6,6,.1)
lines(xvals,dnorm(xvals,mean=-2,sd=1),lwd=2)


###################################################
### code chunk number 8: nullvstrue2
###################################################
shadenormal2(plot.only.type1=FALSE)


###################################################
### code chunk number 9: lecture3.Rnw:274-275
###################################################
shadenormal2(plot.only.type1=FALSE)


###################################################
### code chunk number 10: nullvstrue3
###################################################
shadenormal2(plot.only.type1=FALSE,mean.true=-4)


###################################################
### code chunk number 11: lecture3.Rnw:295-296
###################################################
shadenormal2(plot.only.type1=FALSE,mean.true=-4)


###################################################
### code chunk number 12: nullvstrue4
###################################################
shadenormal2(plot.only.type1=FALSE,alpha=0.01,main="alpha=.01")


###################################################
### code chunk number 13: lecture3.Rnw:313-314
###################################################
shadenormal2(plot.only.type1=FALSE,alpha=0.01,main="alpha=.01")


###################################################
### code chunk number 14: nullvstrue5
###################################################
## simulating larger sample size by decreasing SD to 0.75 from 1:
shadenormal2(plot.only.type1=FALSE,sd=0.75,main="Larger sample size")


###################################################
### code chunk number 15: lecture3.Rnw:331-332
###################################################
## simulating larger sample size by decreasing SD to 0.75 from 1:
shadenormal2(plot.only.type1=FALSE,sd=0.75,main="Larger sample size")


###################################################
### code chunk number 16: lecture3.Rnw:425-430
###################################################
power.t.test(n = NULL, delta = 100, sd = 100, sig.level = 0.05,
             power = 0.80,
             type = c("paired"),
             alternative = c("two.sided"),
             strict = FALSE)


###################################################
### code chunk number 17: lecture3.Rnw:439-441
###################################################
qnorm(0.025,mean=93,sd=5/sqrt(20))
qnorm(0.975,mean=93,sd=5/sqrt(20))


###################################################
### code chunk number 18: lecture3.Rnw:447-471
###################################################
sd<-5
n<-20
power.fn<-function(mu){
## lower and upper bounds of rejection regions
## defined by null hypothesis mean:
lower<-qnorm(0.025,mean=93,sd=5/sqrt(20))
upper<-qnorm(0.975,mean=93,sd=5/sqrt(20))

## lower rejection region:
z.l<-(lower-mu)/(sd/sqrt(n))	
## upper rejection region for given true mu:	
z.u<-(upper-mu)/(sd/sqrt(n))
## return rejection probability:
    return(pnorm(z.u,lower.tail=F)+
          pnorm(z.l,lower.tail=T))}

## a range of true values:	
alt<-seq(86,100,by=0.1)
pow<-power.fn(alt)
plot(alt,pow,type="l",
xlab="Specific parameters 
     for alternative hypothesis",
     ylab="Power",main="Power function 
     for H0: mu=93")


###################################################
### code chunk number 19: lecture3.Rnw:580-581
###################################################
pnorm(-1.96-sqrt(10))


###################################################
### code chunk number 20: lecture3.Rnw:598-599
###################################################
pnorm(1.96-sqrt(18))


###################################################
### code chunk number 21: lecture3.Rnw:610-626
###################################################
##Standard:
pvals<-NULL
tstat_standard<-NULL
n<-10
nsim<-1000
## assume a standard dev of 1:
stddev<-1
mn<-0
for(i in 1:nsim){
  samp<-rnorm(n,mean=mn,sd=stddev)
  pvals[i]<-t.test(samp)$p.value
  tstat_standard[i]<-t.test(samp)$statistic
}

## Type I error rate: about 5% as theory says:
table(pvals<0.05)[2]/nsim


###################################################
### code chunk number 22: lecture3.Rnw:631-659
###################################################
pvals<-NULL
tstat<-NULL
## how many subjects can I run?
upper_bound<-n*6

for(i in 1:nsim){
## at the outset we have no significant result:
  significant<-FALSE
## null hyp is going to be true,
## so any rejection is a mistake.
## take sample:
  x<-rnorm(n,mean=mn,sd=stddev)
while(!significant & length(x)<upper_bound){
  ## if not significant:
if(t.test(x)$p.value>0.05){
  ## get more data:
  x<-append(x,rnorm(n,mean=mn,sd=stddev))
  ## otherwise stop:
} else {significant<-TRUE}
}
## will be either significant or not:
pvals[i]<-t.test(x)$p.value
tstat[i]<-t.test(x)$statistic
}

## Type I error rate:
## much higher than 5%:
table(pvals<0.05)[2]/nsim


###################################################
### code chunk number 23: lecture3.Rnw:664-667
###################################################
hist(tstat_standard,main="The t-distributions for the standard case (white) \n
     vs the stopping rule (gray)",freq=F)
hist(tstat,add=T,col="#0000ff22",freq=F)


