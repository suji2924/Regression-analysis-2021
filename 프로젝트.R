setwd("C:/Users/suji/Desktop")
Final_data <- read.csv('Hn19_all.csv',header=T)

#데이터 6284개 

Y <- Final_data[,8]
X1 <- Final_data[,1]
X2 <- Final_data[,2]
X3 <- Final_data[,3]
X4 <- Final_data[,4]
X5 <- Final_data[,5]
X6 <- Final_data[,6]
X7 <- Final_data[,7]

#상관계수

cor(Y,X1)
cor(Y,X2)
cor(Y,X3)
cor(Y,X4)
cor(Y,X5)
cor(Y,X6)
cor(Y,X7)
cor(Final_data[,1:7])

#산점도 
par(mfrow=c(1,1))
plot(x=X1,y=Y,cex=2,pch=19,ylab="BMI",xlab="sex")
plot(x=X2,y=Y,cex=2,pch=19,ylab="BMI",xlab="age")
plot(x=X3,y=Y,cex=2,pch=19,ylab="BMI",xlab="edu")
plot(x=X4,y=Y,cex=2,pch=19,ylab="BMI",xlab="ainc")
plot(x=X5,y=Y,cex=2,pch=19,ylab="BMI",xlab="BP16_1(sleep)")
plot(x=X6,y=Y,cex=2,pch=19,ylab="BMI",xlab="BE8_1(sit)")
plot(x=X7,y=Y,cex=2,pch=19,ylab="BMI",xlab="HE_wc(waist)")



Full_model <- lm(Y~factor(X1)+X2+X3+X4+X5+X6+X7)
summary(Full_model)

#adj_R2
library(leaps)
Final_reg <- Final_data[,c("HE_BMI","sex","age","edu","ainc","BP16_1","BE8_1","HE_wc")]
regfit_fin <- regsubsets(x=HE_BMI~.,data=Final_reg,method="exhaustive",nbest=7)
summary(regfit_fin)

result_regfit <- summary(regfit_fin)
result_regfit$adjr2
plot(result_regfit$adjr2,ylim = c(0,1),pch=19,cex=2,ylab = "adj_R2",xlab = "model",type = "b")

#결과 -> 29번째 x1,x2,x5,x6,x7만 있는 모형 adj_R2값 제일 높음 8.384174e-01

final_model_R2 <- lm(Y~factor(X1)+X2+X5+X6+X7)
summary(final_model_R2)
# summary결과 x6값 0.0562 안됨 / x5 = 0.0145
null_model_R2 <- lm(Y~1)
anova(final_model_R2,null_model_R2)

#null 모형과 비교 결과 -> 귀무가설 기각은 가능 

#cp
result_regfit <- summary(regfit_fin)
result_regfit$cp
#결과 = 29번째 (4.299611 / x12567)
plot(result_regfit$cp,pch=19,cex=2,ylab="Mallows-Cp",xlab="model",type="b")

final_model_Cp <- lm(Y~factor(X1)+X2+X5+X6+X7)
summary(final_model_Cp)
#결과 역시 x6는 p값 0.0562로 기각 못함 / x5는 0.0145 

null_model_Cp <- lm(Y~1)
anova(final_model_Cp,null_model_Cp)

#결과 -> 귀무가설 기각

#BIC
Final_reg <- Final_data[,c("HE_BMI","sex","age","edu","ainc","BP16_1","BE8_1","HE_wc")]
regfit_fin <- regsubsets(x=HE_BMI~.,data=Final_reg,method="exhaustive",nbest=7)
summary(regfit_fin)

result_regfit <- summary(regfit_fin)
result_regfit$bic
plot(result_regfit$bic,pch=19,cex=2,ylab="BIC",xlab="model",type="b")

#-11415.386796 15번째 - > x 127
final_model_bic <- lm(Y~factor(X1)+X2+X7)
summary(final_model_bic)

null_model_bic <- lm(Y~1)
anova(final_model_bic,null_model_bic)

# 127 은 확실 x5, x6 검정 필요 

#press는 변수 개수 너무 많은 관계로 12567만 살펴보기로 함 
library(qpcR)

#model1 <- lm(Y~1)
model2 <- lm(Y~factor(X1))
model3 <- lm(Y~X2)
model4 <- lm(Y~X5)
model5 <- lm(Y~X6)
model6 <- lm(Y~X7)
model7 <- lm(Y~factor(X1)+X2)
model8 <- lm(Y~factor(X1)+X5)
model9 <- lm(Y~factor(X1)+X6)
model10 <- lm(Y~factor(X1)+X7)
model11 <- lm(Y~X2+X5)
model12 <- lm(Y~X2+X6)
model13 <- lm(Y~X2+X7)
model14 <- lm(Y~X5+X6)
model15 <- lm(Y~X5+X7)
model16 <- lm(Y~X6+X7)
model17 <- lm(Y~factor(X1)+X2+X5)
model18 <- lm(Y~factor(X1)+X2+X6)
model19 <- lm(Y~factor(X1)+X2+X7)
model20 <- lm(Y~factor(X1)+X5+X6)
model21 <- lm(Y~factor(X1)+X5+X7)
model22 <- lm(Y~factor(X1)+X6+X7)
model23 <- lm(Y~X2+X5+X6)
model24 <- lm(Y~X2+X5+X7)
model25 <- lm(Y~X5+X6+X7)
model26 <- lm(Y~factor(X1)+X5+X6+X7)
model27 <- lm(Y~factor(X1)+X2+X5+X7)
model28 <- lm(Y~factor(X1)+X2+X6+X7)
model29 <- lm(Y~X2+X5+X6+X7)
model30 <- lm(Y~factor(X1)+X2+X5+X6)
model31 <- lm(Y~factor(X1)+X2+X5+X6+X7)


#PRESS1 <- PRESS(model1)
PRESS2 <- PRESS(model2)
PRESS3 <- PRESS(model3)
PRESS4 <- PRESS(model4)
PRESS5 <- PRESS(model5)
PRESS6 <- PRESS(model6)
PRESS7 <- PRESS(model7)
PRESS8 <- PRESS(model8)
PRESS9 <- PRESS(model9)
PRESS10 <- PRESS(model10)
PRESS11 <- PRESS(model11)
PRESS12 <- PRESS(model12)
PRESS13 <- PRESS(model13)
PRESS14 <- PRESS(model14)
PRESS15 <- PRESS(model15)
PRESS16 <- PRESS(model16)
PRESS17 <- PRESS(model17)
PRESS18 <- PRESS(model18)
PRESS19 <- PRESS(model19)
PRESS20 <- PRESS(model20)
PRESS21 <- PRESS(model21)
PRESS22 <- PRESS(model22)
PRESS23 <- PRESS(model23)
PRESS24 <- PRESS(model24)
PRESS25 <- PRESS(model25)
PRESS26 <- PRESS(model26)
PRESS27 <- PRESS(model27)
PRESS28 <- PRESS(model28)
PRESS29 <- PRESS(model29)
PRESS30 <- PRESS(model30)
PRESS31 <- PRESS(model31)

     
Y_PRESS <- c(PRESS2$stat,PRESS3$stat,PRESS4$stat,PRESS5$stat,PRESS6$stat,PRESS7$stat,PRESS8$stat,PRESS9$stat,PRESS10$stat,PRESS11$stat,PRESS12$stat,PRESS13$stat,PRESS14$stat,PRESS15$stat,PRESS16$stat,PRESS17$stat,PRESS18$stat,PRESS19$stat,PRESS20$stat,PRESS21$stat,PRESS22$stat,PRESS23$stat,PRESS24$stat,PRESS25$stat,PRESS26$stat,PRESS27$stat,PRESS28$stat,PRESS29$stat,PRESS30$stat,PRESS31$stat)
Y_PRESS
plot(Y_PRESS,pch=19,cex=2,ylab="PRESS",xlab="model",type="b")

#PRESS 결과 : model31 : X12567 그대로 쓰는 것이 제일 낮음 
final_model_PRESS <- lm(Y~factor(X1)+X2+X5+X6+X7)
summary(final_model_PRESS)
#x6이 0.0562로 p값 넘음 

null_model_PRESS <- lm(Y~1)
anova(final_model_PRESS,null_model_PRESS)
#근데 아노바 결과는 또 p값 괜찮음


#부분 F검정으로 X6검정하자 
full_model_F <- lm(Y~factor(X1)+X2+X5+X6+X7)
summary(full_model_F)
reduced_model_F <- lm(Y~factor(X1)+X2+X5+X7)
summary(reduced_model_F)
anova(reduced_model_F,full_model_F)
#부분 F 검정 결과 최종 x6제외하기로 결정 


# model x1257 검정해보자 
null_model_test <- lm(Y~1)
full_model_test <- lm(Y~factor(X1)+X2+X5+X7)
step(null_model_test,scope = ~factor(X1)+X2+X5+X7,direction = "both",test="F")
step(full_model_test,direction= "backward",test="F")

summary(full_model_test)
#full model 좋음 -> X1,X2,X5,X7

#교호작용 적용 
test_model <- lm(Y~factor(X1)+X2+X5+X7+X2*factor(X1)+X5*factor(X1)+X7*factor(X1))
summary(test_model)

#결과 -> X2 X7과는 유의 x5와는 ㄴㄴ -> 교호작용항 추가해야함 
full_model_g <- lm(Y~factor(X1)+X2+X5+X7+X2*factor(X1)+X7*factor(X1))
summary(full_model_g)

#오차항의 등분산성, 정규성 검정, 분산안정화 변환 (로그, 루트),  다중공선성, 잔차분석, 예측정확도



#오차항의 등분산성
par(mfrow=c(2,2))
plot(full_model_g)
summary(full_model_g)

#정규성 검정 box-cox :  57번째, 0.26262626
library(MASS)
par(mfrow=c(1,1))
boxcox_result <- boxcox(full_model_g)
boxcox_result

#변환 
#원래꺼 
par(mfrow=c(1,1))
std_model_g <- rstandard(full_model_g)
qqnorm(std_model_g,cex=2,pch=19,ylab="standardized residual original")
qqline(std_model_g,col="red",lwd=2,lty=2)
summary(full_model_g)
par(mfrow=c(2,2))
plot(full_model_g)

model_y_0.25 <- lm(Y^0.25~factor(X1)+X2+X5+X7+X2*factor(X1)+X7*factor(X1))
std_y_0.25 <- rstandard(model_y_0.25)
qqnorm(std_y_0.25,cex=2,pch=19,ylab="standardized residual 0.25")
qqline(std_y_0.25,col="red",lwd=2,lty=2)
summary(model_y_0.25)
par(mfrow=c(2,2))
plot(model_y_0.25)

model_y_0.3 <- lm(Y^0.3~factor(X1)+X2+X5+X7+X2*factor(X1)+X7*factor(X1))
std_y_0.3 <- rstandard(model_y_0.3)
qqnorm(std_y_0.3,cex=2,pch=19,ylab="standardized residual 0.3")
qqline(std_y_0.3,col="red",lwd=2,lty=2)
summary(model_y_0.3)
par(mfrow=c(2,2))
plot(model_y_0.3)

Y_sqrt <- sqrt(Y)
model_sqrt <- lm(Y_sqrt~factor(X1)+X2+X5+X7+X2*factor(X1)+X7*factor(X1))
std_sqrt <- rstandard(model_sqrt)
qqnorm(std_sqrt,cex=2,pch=19,ylab="standardized residual sqrt")
qqline(std_sqrt,col="red",lwd=2,lty=2)
summary(model_sqrt)
par(mfrow=c(2,2))
plot(model_sqrt)


#다중공선성 ; 다중공선성 결과 낮아서 따로 조치 필요 x 


X <- cbind(factor(X1),X2,X5,X7)
R <- cor(X)
round(R,3)

diag(solve(R))



#예측  #총 6284개 / 4399개 훈련자료 / 1885개 확인자료 
set.seed(1234)
rn <- sample(x=c(1:6284),size=6284,replace=F)
Final_data$rn <- rn
head(Final_data)

train_data <- Final_data[Final_data$rn>1885,]
test_data <- Final_data[Final_data$rn<=1885,]
dim(train_data)
dim(test_data)

train_model <- lm(sqrt(HE_BMI)~factor(sex)+age+BP16_1+HE_wc+age*factor(sex)+HE_wc*factor(sex),data=train_data)
summary(train_model)

predict_value <- predict(train_model,newdata=test_data[,c("sex","age","BP16_1","HE_wc")])
predict_error <- sum((test_data$HE_BMI-predict_value)^2)
predict_error

#press
library(qpcR)
Final_model <- lm(Y_sqrt~factor(X1)+X2+X5+X7+X2*factor(X1)+X7*factor(X1))
PRESS_Final <- PRESS(Final_model)
PRESS_Final$stat

SST <- sum((Final_data$HE_BMI-mean(Final_data$HE_BMI))^2);SST
SSE <- sum(resid(Final_model)^2);SSE
SSR <- SST-SSE
SSR

1-(SSE/SST)

1-(PRESS_Final$stat/SST)

#결론
summary(Final_model)
