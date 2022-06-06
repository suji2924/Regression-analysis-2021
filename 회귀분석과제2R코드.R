#문제 1번

electronic_data <- data.frame(Seller=c(5,8,7,3,6,10,4,2),Increase=c(55,80,67,30,48,92,20,10))
electronic_data

X <-electronic_data$Seller
Y <- electronic_data$Increase

mean(X)
mean(Y)

#c
Sxy <- sum((X-mean(X))*(Y-mean(Y)))
Sxy
Sxx <- sum((X-mean(X))^2)
Sxx
beta1_hat <- Sxy/Sxx
beta1_hat

beta0_hat <- mean(Y)-beta1_hat*mean(X)
beta0_hat

model <- lm(Increase~Seller,data=electronic_data)
model

#d
Y_hat <- beta0_hat+beta1_hat*X
res <- Y-Y_hat
res

resid(model)

s2 <- sum(res^2)/(length(X)-2)
s2

sqrt(s2)

#문제2번
#a
SST <- sum((Y-mean(Y))^2)
SSR <- sum((Y_hat-mean(Y))^2)
SSE <- sum((Y-Y_hat)^2)
SST;SSR;SSE

MSE <- SSE/(length(Y)-2);MSE
MSR <- SSR/1;MSR

F_val <- MSR/MSE;F_val

pf(F_val,df1=1,df2=length(Y)-2,lower.tail=F)

anova(model)

#b
r2 <- SSR/SST
r2

#문제3
#a
s <- sqrt(SSE/(length(Y)-2))
s;

SE0 <- s*sqrt((1/length(Y))+(mean(X)^2)/Sxx);SE0
beta0_t0 <- beta0_hat/SE0;beta0_t0

pt(beta0_t0,df=length(Y)-2,lower.tail = T)*2;

#b
beta0_hat-qt(p=0.05,df=length(Y)-2,lower.tail=F)*SE0;
beta0_hat+qt(p=0.05,df=length(Y)-2,lower.tail=F)*SE0;

#c
SE1 <- s/sqrt(Sxx)
beta1_t0 <- beta1_hat/SE1;beta1_t0

pt(beta1_t0,df=length(Y)-2,lower.tail = F)*2

#d
beta1_hat-qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE1
beta1_hat+qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE1

summary(model)


#문제4
#a
Est_value <- beta0_hat+beta1_hat*9;Est_value
SE_mean <- s*sqrt((1/(length(Y))+(9-mean(X))^2/sum((X-mean(X))^2)))
Est_value-qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE_mean
Est_value+qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE_mean

#b
SE_pre <- s*sqrt((1+(1/(length(Y)))+(9-mean(X))^2/sum((X-mean(X))^2)))
Est_value-qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE_pre
Est_value+qt(p=0.05,df=length(Y)-2,lower.tail = F)*SE_pre

predict(model,newdata=data.frame(Seller=9),interval="prediction")


#문제5
#a
#1. 표준화 잔차 vs 예측값의 산점도
hii <- (1/length(Y))+(X-mean(X))^2/sum((X-mean(X))^2);hii

res <- (Y-Y_hat);res

std_resi <- res/(s*sqrt(1-hii));std_resi

plot(x=Y_hat,y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="fitted value",main="std residual VS fitted value")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

#2. 표준화 잔차 VS 설명변수의 산점도
plot(x=X,y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="Seller(X)",main="std residual VS X")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

#3. 표준화 잔차 VS 관측순서
plot(x=c(1:8),y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="observation i",main="std residual VS i")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

#b 
qqnorm(std_resi,cex=2,pch=19,ylab="standardized residual")
qqline(std_resi,col="red",lty=2,lwd=2)