setwd("C:/Users/suji/Desktop")
Boston_dat <- read.csv('Boston.csv',header=T)

#문제 1
#a
Y <- Boston_dat[,13]
X1 <- Boston_dat[,1]
X2 <- Boston_dat[,2]
X3 <- Boston_dat[,3]
X4 <- Boston_dat[,4]
X5 <- Boston_dat[,5]
X6 <- Boston_dat[,6]
X7 <- Boston_dat[,7]
X8 <- Boston_dat[,8]
X9 <- Boston_dat[,9]
X10 <- Boston_dat[,10]
X11 <- Boston_dat[,11]
X12 <- Boston_dat[,12]


#b
#Full model
Full_model <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12)
summary(Full_model)

Y_hat_F <- predict(Full_model,newdata = data.frame(X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6,X7=X7,X8=X8,X9=X9,X10=X10,X11=X11,X12=X12))
head(Y_hat_F)

SST <- sum((Y-mean(Y))^2)
SST

SSR_F <- sum((Y_hat_F-mean(Y))^2)
SSR_F

SSE_F <- sum((Y-Y_hat_F)^2)
SSE_F

MSR_F <- SSR_F/(13-1)
MSR_F

MSE_F <- SSE_F/(506-13)
MSE_F

F0_F <- MSR_F/MSE_F
F0_F

pf(F0_F,df1=11,df2=493,lower.tail = F)

#Reduced model (X3,X6제외)
Reduced_model <- lm(Y~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
summary(Reduced_model)

Y_hat_R <- predict(Reduced_model,newdata = data.frame(X1=X1,X2=X2,X4=X4,X5=X5,X7=X7,X8=X8,X9=X9,X10=X10,X11=X11,X12=X12))
head(Y_hat_R)

SST <- sum((Y-mean(Y))^2)
SST

SSR_R <- sum((Y_hat_R-mean(Y))^2)
SSR_R

SSE_R <- sum((Y-Y_hat_R)^2)
SSE_R

MSR_R <- SSR_R/(11-1)
MSR_R

MSE_R <- SSE_R/(506-11)
MSE_R

F0_R <- MSR_R/MSE_R
F0_R

pf(F0_R,df1=10,df2=495,lower.tail = F)

anova(Reduced_model,Full_model)


#2
Y_std <- (Y-mean(Y))/sqrt(sum((Y-mean(Y))^2))
X1_std <- (X1-mean(X1))/sqrt(sum((X1-mean(X1))^2))
X2_std <- (X2-mean(X2))/sqrt(sum((X2-mean(X2))^2))
X4_std <- (X4-mean(X4))/sqrt(sum((X4-mean(X4))^2))
X5_std <- (X5-mean(X5))/sqrt(sum((X5-mean(X5))^2))
X7_std <- (X7-mean(X7))/sqrt(sum((X7-mean(X7))^2))
X8_std <- (X8-mean(X8))/sqrt(sum((X8-mean(X8))^2))
X9_std <- (X9-mean(X9))/sqrt(sum((X9-mean(X9))^2))
X10_std <- (X10-mean(X10))/sqrt(sum((X10-mean(X10))^2))
X11_std <- (X11-mean(X11))/sqrt(sum((X11-mean(X11))^2))
X12_std <- (X12-mean(X12))/sqrt(sum((X12-mean(X12))^2))

model_std <- lm(Y~X1_std+X2_std+X4_std+X5_std+X7_std+X8_std+X9_std+X10_std+X11_std+X12_std)
summary((model_std))

Reduced_model <- lm(Y~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
summary(Reduced_model)

#3
#a
#Y축
model212 <- lm(Y~X2+X4+X5+X7+X8+X9+X10+X11+X12)
model212

#X축
modelX212 <- lm(X1~X2+X4+X5+X7+X8+X9+X10+X11+X12)
modelX212

y.x2x12 <- resid(model212)
x1.x2x12 <- resid(modelX212)

plot(x1.x2x12,y.x2x12,pch=19,cex=2,xlab="X1|X2~X12",ylab="Y|X2~X12")
model_parX1 <- lm(y.x2x12~x1.x2x12)
model_parX1

#b
#estimated model
model <- lm(Y~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
model


#4
#a
model <- lm(Y~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
model
par(mfrow=c(2,2))
plot(model)
summary(model)

#b
library(MASS)
par(mfrow=c(1,1))
boxcox_result <- boxcox(model)
boxcox_result

#c
Y_log <- log(Y)
model_log <- lm(Y_log~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
summary(model_log)
par(mfrow=c(2,2))
plot(model_log)

model_0.10 <- lm(Y^0.1010~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
std_0.10 <- rstandard(model_0.10)
qqnorm(std_0.10,cex=2,pch=19,ylab="standardized residual")
qqline(std_0.10,col='red',lwd=2,lty=2)



#5
summary(model_log)

X <- cbind(X1,X2,X4,X5,X7,X8,X9,X10,X11,X12)
R <- cor(X)
round(R,3)

diag(solve(R))

#partial F-test
Reduced_model_X8 <- lm(log(Y)~X1+X2+X4+X5+X7+X9+X10+X11+X12)
Full_model_X8 <- lm(log(Y)~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
anova(Reduced_model_X8,Full_model_X8)

Reduced_model_X9 <- lm(log(Y)~X1+X2+X4+X5+X7+X8+X10+X11+X12)
Full_model_X9 <- lm(log(Y)~X1+X2+X4+X5+X7+X8+X9+X10+X11+X12)
anova(Reduced_model_X9,Full_model_X9)