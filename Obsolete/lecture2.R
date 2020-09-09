### R code from vignette source 'lecture2.Rnw'

###################################################
### code chunk number 1: lecture2.Rnw:8-10
###################################################
x<-rnorm(100)
head(x)


###################################################
### code chunk number 2: lecture2.Rnw:21-23
###################################################
## plot density histogram:
hist(x,freq=F)


###################################################
### code chunk number 3: lecture2.Rnw:44-47
###################################################
## mean and sigma set at 0 and 1 by default:
normal.density.function <- function(x,mu=0,sigma=1){
  1/(sqrt(2*pi)*sigma)*exp(-((x - mu)^2/(2*sigma^2)))}


###################################################
### code chunk number 4: lecture2.Rnw:59-62
###################################################
plot(function(x) normal.density.function(x), -3, 3,
      main = "Normal density function",ylim=c(0,.4),
              ylab="density",xlab="X")


###################################################
### code chunk number 5: lecture2.Rnw:69-72
###################################################
plot(function(x) dnorm(x), -3, 3,
      main = "Normal density",ylim=c(0,.4),
              ylab="density",xlab="X")


###################################################
### code chunk number 6: lecture2.Rnw:77-78
###################################################
integrate(function(x) dnorm(x, mean = 0, sd = 1), -Inf, +Inf)


###################################################
### code chunk number 7: lecture2.Rnw:83-84
###################################################
integrate(function(x) dnorm(x, mean = 0, sd = 1), -2, +2)


###################################################
### code chunk number 8: normal2SD
###################################################
## plot multiple figures:
## replace ugly par... specification with 
## something easier to remember:
multiplot <- function(row,col){
     par(mfrow=c(row,col),pty="s")
   }

main.title<-"Area within 2 SD of the mean"

multiplot(1, 2)
plot(function(x) dnorm(x, mean = 0, sd = 1), 
xlim=c(-3, 3),main="SD 1",xlab="x",ylab="",cex=2)
segments(-2, 0, -2, 0.4)
segments(2, 0, 2, 0.4)

plot(function(x) dnorm(x, mean = 0, sd = 4), 
xlim=c(-12, 12),main="SD 4",xlab="x",ylab="",cex=2)
segments(-8, 0, -8, 0.1)
segments(8, 0, 8, 0.1)


###################################################
### code chunk number 9: lecture2.Rnw:123-124
###################################################
## plot multiple figures:
## replace ugly par... specification with 
## something easier to remember:
multiplot <- function(row,col){
     par(mfrow=c(row,col),pty="s")
   }

main.title<-"Area within 2 SD of the mean"

multiplot(1, 2)
plot(function(x) dnorm(x, mean = 0, sd = 1), 
xlim=c(-3, 3),main="SD 1",xlab="x",ylab="",cex=2)
segments(-2, 0, -2, 0.4)
segments(2, 0, 2, 0.4)

plot(function(x) dnorm(x, mean = 0, sd = 4), 
xlim=c(-12, 12),main="SD 4",xlab="x",ylab="",cex=2)
segments(-8, 0, -8, 0.1)
segments(8, 0, 8, 0.1)


###################################################
### code chunk number 10: lecture2.Rnw:132-143
###################################################
## Prob. of getting 2 or less:
pnorm(2) 

## Prob. of getting more than 2:
1-pnorm(2) 

## Prob. of getting -2 or less:
pnorm(-2) 

## Prob. of being between -2 and 2:
pnorm(2)-pnorm(-2)


###################################################
### code chunk number 11: lecture2.Rnw:167-173
###################################################
## figure out area between the unknown bounds:
prob<-round(pnorm(2)-pnorm(-2),digits=4)
## figure out lower bound:
(lower<-qnorm((1-prob)/2,mean=0,sd=1,lower.tail=T))
## figure out upper bound:
(upper<-qnorm((1-prob)/2,mean=0,sd=1,lower.tail=F))


###################################################
### code chunk number 12: lecture2.Rnw:180-189
###################################################
plot(function(x) dnorm(x, mean = 0, sd = 1), 
xlim=c(-4, 4),main="mean 1, SD 1",xlab="x",ylab="",cex=2)
segments(-2, 0, -2, 0.1)
segments(2, 0, 2, 0.1)
text(0,.05,"probability=0.9545")
text(-3,.05,"lower tail")
text(-3,.005,"probability=0.02275")
text(3,.05,"upper tail")
text(3,.005,"probability=0.02275")


###################################################
### code chunk number 13: lecture2.Rnw:208-214
###################################################
#1000 samples of 40 taken repeatedly:
sample.means <- rep(NA,1000)
for(i in 1:1000){
  sample.40 <- rnorm(40,mean=60,sd=4)
  sample.means[i] <- mean(sample.40)
}


###################################################
### code chunk number 14: lecture2.Rnw:219-221
###################################################
means40<-mean(sample.means)
sd40<-sd(sample.means)


###################################################
### code chunk number 15: sdsmplot40
###################################################
hist(sample.means)


###################################################
### code chunk number 16: lecture2.Rnw:242-243
###################################################
hist(sample.means)


###################################################
### code chunk number 17: lecture2.Rnw:254-261
###################################################
sample.means <- rep(NA,1000)

for(i in 1:1000){
  sample.100 <- rnorm(100,mean=60,sd=4)
  sample.means[i] <- mean(sample.100)
}



###################################################
### code chunk number 18: lecture2.Rnw:264-266
###################################################
means100 <- mean(sample.means)
sd100 <- sd(sample.means)


###################################################
### code chunk number 19: sdsmplot100
###################################################
hist(sample.means)


###################################################
### code chunk number 20: lecture2.Rnw:282-283
###################################################
hist(sample.means)


###################################################
### code chunk number 21: lecture2.Rnw:316-318
###################################################
4/sqrt(40)
4/sqrt(100)


###################################################
### code chunk number 22: exppopulation
###################################################
sample.100 <- rexp(100, 1/60)
hist(sample.100)


###################################################
### code chunk number 23: lecture2.Rnw:362-363
###################################################
sample.100 <- rexp(100, 1/60)
hist(sample.100)


###################################################
### code chunk number 24: exponentialsdsm
###################################################
sample.means <- rep(NA,1000)

for(i in 1:1000){ 
	sample.100 <- rexp(100, 1/60)
	sample.means[i] <- mean(sample.100)
}

hist(sample.means)


###################################################
### code chunk number 25: lecture2.Rnw:405-406
###################################################
sample.means <- rep(NA,1000)

for(i in 1:1000){ 
	sample.100 <- rexp(100, 1/60)
	sample.means[i] <- mean(sample.100)
}

hist(sample.means)


###################################################
### code chunk number 26: lecture2.Rnw:439-440
###################################################
sample.11 <- rnorm(11,mean=60,sd=4)


###################################################
### code chunk number 27: lecture2.Rnw:452-456
###################################################
estimated.mean <- mean(sample.11)
SD.population <- 4 
n <- length(sample.11)
SD.distribution <- SD.population/sqrt(n)


###################################################
### code chunk number 28: lecture2.Rnw:487-491
###################################################
sample.44 <- rnorm(44,mean=60,sd=4)
estimated.mean <- mean(sample.44)
n <- length(sample.44)
(SD.distribution <- SD.population/sqrt(n))


###################################################
### code chunk number 29: varsample
###################################################
sample.var <- rep(NA,1000)
for(i in c(1:1000)){
  sample.40 <- rnorm(40,mean=60,sd=4)
  sample.var[i] <- var(sample.40)
}
hist(sample.var)


###################################################
### code chunk number 30: lecture2.Rnw:558-565
###################################################
sample.var <- rep(NA,1000)
for(i in c(1:1000)){
  sample.40 <- rnorm(40,mean=60,sd=4)
  sample.var[i] <- var(sample.40)
}
hist(sample.var)



###################################################
### code chunk number 31: varsampleexp
###################################################
sample.var <- rep(NA,1000)
for(i in c(1:1000)){
  sample.var[i] <- var(rexp(40))
}
hist(sample.var)


###################################################
### code chunk number 32: lecture2.Rnw:586-591
###################################################
sample.var <- rep(NA,1000)
for(i in c(1:1000)){
  sample.var[i] <- var(rexp(40))
}
hist(sample.var)


###################################################
### code chunk number 33: tversusnorm
###################################################
range <- seq(-4,4,.01)  
 
multiplot(2,2)

 for(i in c(2,5,15,20)){
   plot(range,dnorm(range),type="l",lty=1,
        xlab="",ylab="",
        cex.axis=1)
   lines(range,dt(range,df=i),lty=2,lwd=1)
   mtext(paste("df=",i),cex=1.2)
 }


###################################################
### code chunk number 34: lecture2.Rnw:635-637
###################################################
range <- seq(-4,4,.01)  
 
multiplot(2,2)

 for(i in c(2,5,15,20)){
   plot(range,dnorm(range),type="l",lty=1,
        xlab="",ylab="",
        cex.axis=1)
   lines(range,dt(range,df=i),lty=2,lwd=1)
   mtext(paste("df=",i),cex=1.2)
 }



###################################################
### code chunk number 35: lecture2.Rnw:687-688
###################################################
sample <- rnorm(11,mean=60,sd=4)


###################################################
### code chunk number 36: lecture2.Rnw:700-701
###################################################
t.test(sample)$conf.int


###################################################
### code chunk number 37: lecture2.Rnw:709-710
###################################################
sample <- rnorm(100,mean=60,sd=4)


###################################################
### code chunk number 38: lecture2.Rnw:713-714
###################################################
t.test(sample)$conf.int


###################################################
### code chunk number 39: lecture2.Rnw:745-761
###################################################

se <- function(x)
      {
        y <- x[!is.na(x)] # remove the missing values, if any
        sqrt(var(as.vector(y))/length(y))
}


ci <- function (scores){
m <- mean(scores,na.rm=TRUE)
stderr <- se(scores)
len <- length(scores)
upper <- m + qt(.975, df=len-1) * stderr 
lower <- m + qt(.025, df=len-1) * stderr 
return(data.frame(lower=lower,upper=upper))
}


###################################################
### code chunk number 40: lecture2.Rnw:768-781
###################################################

lower <- rep(NA,100)
upper <- rep(NA,100)

for(i in 1:100){ 
  sample <- rnorm(100,mean=60,sd=4)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
}
  
cis <- cbind(lower,upper)

head(cis)


###################################################
### code chunk number 41: lecture2.Rnw:810-832
###################################################
store <- rep(NA,100)

pop.mean<-60
pop.sd<-4

for(i in 1:100){ 
  sample <- rnorm(100,mean=pop.mean,sd=pop.sd)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
  if(lower[i]<pop.mean & upper[i]>pop.mean){
    store[i] <- TRUE} else {
      store[i] <- FALSE}
}

## need this for the plot below:
cis <- cbind(lower,upper)


## convert store to factor:
store<-factor(store)

summary(store)


###################################################
### code chunk number 42: repeatedCIsplot
###################################################
main.title<-"95% CIs in 100 repeated samples"

line.width<-ifelse(store==FALSE,2,1)
cis<-cbind(cis,line.width)
x<-0:100
y<-seq(55,65,by=1/10)
plot(x,y,type="n",xlab="i-th repeated sample",ylab="Scores",main=main.title)
abline(60,0,lwd=2)
x0<-x
x1<-x
arrows(x0,y0=cis[,1],
       x1,y1=cis[,2],length=0,lwd=cis[,3])


###################################################
### code chunk number 43: lecture2.Rnw:859-860
###################################################
main.title<-"95% CIs in 100 repeated samples"

line.width<-ifelse(store==FALSE,2,1)
cis<-cbind(cis,line.width)
x<-0:100
y<-seq(55,65,by=1/10)
plot(x,y,type="n",xlab="i-th repeated sample",ylab="Scores",main=main.title)
abline(60,0,lwd=2)
x0<-x
x1<-x
arrows(x0,y0=cis[,1],
       x1,y1=cis[,2],length=0,lwd=cis[,3])


###################################################
### code chunk number 44: lecture2.Rnw:941-954
###################################################
# re-define variance to see whether it underestimates:
new.var <- function(x){
	sum((x-mean(x))^2) / length(x)
}

correct <- rep(NA,1000)
incorrect <- rep(NA,1000)

for(i in 1:1000){
  sample.10 <- rnorm(10, mean=0, sd=1)
  correct[i] <- var(sample.10)
  incorrect[i] <- new.var(sample.10)
}       


###################################################
### code chunk number 45: nminus1
###################################################
multiplot(1,2)
hist(correct,main=paste("Mean ",round(mean(correct),digits=2),sep=" "))
hist(incorrect,main=paste("Mean ",round(mean(incorrect),digits=2),sep=" "))


###################################################
### code chunk number 46: lecture2.Rnw:970-971
###################################################
multiplot(1,2)
hist(correct,main=paste("Mean ",round(mean(correct),digits=2),sep=" "))
hist(incorrect,main=paste("Mean ",round(mean(incorrect),digits=2),sep=" "))


###################################################
### code chunk number 47: lecture2.Rnw:1057-1058
###################################################
SD.distribution = 4/sqrt(11)


###################################################
### code chunk number 48: nullhypexample
###################################################
range <- seq(55,85,0.01)

plot(range,dnorm(range,mean=70,
     sd=SD.distribution),type="l",ylab="",main="The null hypothesis")


###################################################
### code chunk number 49: lecture2.Rnw:1076-1077
###################################################
range <- seq(55,85,0.01)

plot(range,dnorm(range,mean=70,
     sd=SD.distribution),type="l",ylab="",main="The null hypothesis")


###################################################
### code chunk number 50: lecture2.Rnw:1087-1088
###################################################
sample <- rnorm(11,mean=60,sd=4)


###################################################
### code chunk number 51: lecture2.Rnw:1091-1092
###################################################
sample.mean <- mean(sample)


###################################################
### code chunk number 52: sampleMeanVsZ
###################################################

sample.means <- rep(NA, 1000)
zs <- rep(NA, 1000)

for(i in 1:1000){
  sample.11 <- rnorm(11,mean=70,sd=4)
  sample.means[i] <- mean(sample.11)
  zs[i] <- ( mean(sample.11) - 70 ) / (4/sqrt(11))	  
}


multiplot(2, 2)
sd.dist <- 4/sqrt(11)
plot(density(sample.means,kernel="gaussian"),xlim=range(70-(4*sd.dist), 
70+(4*sd.dist)),xlab="",ylab="",main="")	
plot(density(zs,kernel="gaussian"),xlim=range(-4, 4),xlab="",ylab="",main="")
plot(function(x) dnorm(x, 70, 4/sqrt(11)), 
70-(4*sd.dist), 70+(4*sd.dist),xlab="",ylab="",main="")	
plot(function(x) dnorm(x, 0, 1), -4, 4,xlab="",ylab="",main="")


###################################################
### code chunk number 53: lecture2.Rnw:1230-1231
###################################################

sample.means <- rep(NA, 1000)
zs <- rep(NA, 1000)

for(i in 1:1000){
  sample.11 <- rnorm(11,mean=70,sd=4)
  sample.means[i] <- mean(sample.11)
  zs[i] <- ( mean(sample.11) - 70 ) / (4/sqrt(11))	  
}


multiplot(2, 2)
sd.dist <- 4/sqrt(11)
plot(density(sample.means,kernel="gaussian"),xlim=range(70-(4*sd.dist), 
70+(4*sd.dist)),xlab="",ylab="",main="")	
plot(density(zs,kernel="gaussian"),xlim=range(-4, 4),xlab="",ylab="",main="")
plot(function(x) dnorm(x, 70, 4/sqrt(11)), 
70-(4*sd.dist), 70+(4*sd.dist),xlab="",ylab="",main="")	
plot(function(x) dnorm(x, 0, 1), -4, 4,xlab="",ylab="",main="")


###################################################
### code chunk number 54: lecture2.Rnw:1242-1245
###################################################
integrate(function(x) dnorm(x, mean = 0, sd = 1), -Inf, -8.291562)
## alternative, more standard way:
pnorm(mean=0,sd=1,-8.291562)


###################################################
### code chunk number 55: lecture2.Rnw:1250-1252
###################################################
integrate(function(x) dnorm(x, mean = 70, sd = 4/sqrt(11)), -Inf, 60)
pnorm(60,mean=70,sd=4/sqrt(11))


###################################################
### code chunk number 56: lecture2.Rnw:1257-1258
###################################################
(67.58-70)/(4/sqrt(11))


###################################################
### code chunk number 57: lecture2.Rnw:1263-1265
###################################################
integrate(function(x) dnorm(x, 0, 1), -Inf, -2.0)
pnorm(-2,mean=0,sd=1)


###################################################
### code chunk number 58: lecture2.Rnw:1273-1274
###################################################
pnorm(-8.291562)


###################################################
### code chunk number 59: lecture2.Rnw:1336-1342
###################################################
sample.11 <- rnorm(11,mean=60,sd=4)

t.test(sample.11,
       alternative = "two.sided",
       mu = 70, 
       conf.level = 0.95)


###################################################
### code chunk number 60: lecture2.Rnw:1351-1352
###################################################
(mean(sample.11)-70)/se(sample.11)


###################################################
### code chunk number 61: lecture2.Rnw:1445-1452
###################################################
d <- rep(NA,1000)

for(i in 1:1000){
  sample1 <- rnorm(10,mean=30,sd=43)
  sample2 <- rnorm(20,mean=7,sd=25)
  d[i] <- mean(sample1) - mean(sample2)
}


###################################################
### code chunk number 62: lecture2.Rnw:1458-1462
###################################################

30-7

mean(d)


###################################################
### code chunk number 63: diff
###################################################
hist(d)


###################################################
### code chunk number 64: lecture2.Rnw:1482-1483
###################################################
hist(d)


###################################################
### code chunk number 65: lecture2.Rnw:1491-1492
###################################################
newsigma<-round(sqrt((43^2/10)+(25^2/20)),digits=4)


###################################################
### code chunk number 66: lecture2.Rnw:1501-1502
###################################################
newsigma<-round(sqrt((43^2/10)+(25^2/20)),digits=4)


###################################################
### code chunk number 67: lecture2.Rnw:1559-1564
###################################################

t.test.result<-t.test(sample1,sample2,
       mu=0,
       alternative = "two.sided",
        conf.level = 0.95,var.equal=FALSE)


