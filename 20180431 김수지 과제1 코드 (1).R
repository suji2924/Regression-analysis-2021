#문제 5번#

#a#
85-qnorm(p=0.05,mean=0,sd=1,lower.tail=F)*(sqrt(40)/sqrt(200))
85+qnorm(p=0.05,mean=0,sd=1,lower.tail=F)*(sqrt(40)/sqrt(200))



#b#
(85-90)/(sqrt(40)/sqrt(200))

z0 <- (85-90)/(sqrt(40)/sqrt(200))
z0

pnorm(z0,mean=0,sd=1,lower.tail=T)*2



#문제 6번#

Ex_Data <- data.frame(X=c(75,43,50,92,81),Y=c(80,38,70,91,87)) 
Ex_Data

#a#
plot(x=Ex_Data$X,y=Ex_Data$Y,pch=19,cex=2,xlab="중간고사성적",ylab="기말고사성적")

#b#
Cov <- mean((Ex_Data$X-mean(Ex_Data$X))*(Ex_Data$Y-mean(Ex_Data$Y)))
Cov

SSX <- sd(Ex_Data$X)*sqrt(5-1)
SSX

SSY <- sd(Ex_Data$Y)*sqrt(5-1)
SSY

Corr <- (Cov*5)/(SSX*SSY)
Corr


#c#
t0 <- sqrt(5-2)*Corr/sqrt(1-Corr^2)
t0

pt(t0,df=5-2,lower.tail=F)*2

#using function
cor.test(x=Ex_Data$X,y=Ex_Data$Y)



