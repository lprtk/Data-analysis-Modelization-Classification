############ R : Data Analysis Project ############ 

## Data loading 
simu=read.table("simu.txt", header=T)
xsimu=read.table("xsimutest.txt", header=T)

## Library used
library(MASS)

## Statistical and predictive modeling
simu$Y<-as.factor(simu$Y)

model <- glm(Y~ 1 + X1 + X2 + X1:X2, data = simu, family = binomial())
summary(model)

stepAIC(model,direction="backward",k=2)
stepAIC(model,direction="backward",k=log(dim(simu)[1]))

model1<-glm(Y~1+ X1+ X2+ X1:X2, data=simu, family=binomial())
summary(model1)

model2<-glm(Y~ 1 + X1 + X2, data=simu, family=binomial())
summary(model2)

model3<-glm(Y~ 1 + X2 + X1:X2, data=simu, family=binomial())
summary(model3)

model4<-glm(Y~ 1 + X1 + X1:X2, data=simu, family=binomial())
summary(model4)

model5 <- glm(Y~ 1 + X1 , data = simu, family = binomial())
summary(model5)

model6 <- glm(Y~ 1 + X2 , data = simu, family = binomial())
summary(model6)

model7 <- glm(Y~ 1 + X2:X1 , data=simu, family=binomial())
summary(model7)

#

model8<-glm(Y~ -1+ X1+ X2+ X1:X2, data=simu, family=binomial())
summary(model8)

model9<-glm(Y~ -1+ X1+ X2, data=simu, family=binomial())
summary(model9)

model10<-glm(Y~ -1+ X2+ X1:X2, data=simu, family=binomial())
summary(model10)

model11<-glm(Y~ -1+ X1+ X1:X2, data=simu, family=binomial())
summary(model11)

model12 <- glm(Y~ -1 + X1 , data = simu, family = binomial())
summary(model12)

model13 <- glm(Y~ -1 + X2 , data = simu, family = binomial())
summary(model13)

model14 <- glm(Y~ -1 + X2:X1 , data=simu, family=binomial())
summary(model14)


## For each model the predictions are retrieved
Y.pred1 <- predict(model1, type = "response", newdata = simu)
head(Y.pred1)
T1 <- table(Y.pred1 > 0.5, simu$Y)
T1
(T1[1,2]+T1[2,1])/2000

Y.pred2 <- predict(model2, type = "response", newdata = simu)
head(Y.pred2)
T2 <- table(Y.pred2 > 0.5, simu$Y)
T2
(T2[1,2]+T2[2,1])/2000

Y.pred3 <- predict(model3, type = "response", newdata = simu)
head(Y.pred3)
T3 <- table(Y.pred3 > 0.5, simu$Y)
T3
(T3[1,2]+T3[2,1])/2000

Y.pred4 <- predict(model4, type = "response", newdata = simu)
head(Y.pred4)
T4 <- table(Y.pred4 > 0.5, simu$Y)
T4
(T4[1,2]+T4[2,1])/2000

Y.pred5 <- predict(model5, type = "response", newdata = simu)
head(Y.pred5)
T5 <- table(Y.pred5 > 0.5, simu$Y)
T5
(T5[1,2]+T5[2,1])/2000

Y.pred6 <- predict(model6, type = "response", newdata = simu)
head(Y.pred6)
T6 <- table(Y.pred6 > 0.5, simu$Y)
T6
(T6[1,2]+T6[2,1])/2000

Y.pred7 <- predict(model7, type = "response", newdata = simu)
head(Y.pred7)
T7 <- table(Y.pred7 > 0.5, simu$Y)
T7
(T7[1,2]+T7[2,1])/2000

Y.pred8 <- predict(model8, type = "response", newdata = simu)
head(Y.pred8)
T8 <- table(Y.pred8 > 0.5, simu$Y)
T8
(T8[1,2]+T8[2,1])/2000

Y.pred9 <- predict(model9, type = "response", newdata = simu)
head(Y.pred9)
T9 <- table(Y.pred9 > 0.5, simu$Y)
T9
(T9[1,2]+T9[2,1])/2000

Y.pred10 <- predict(model10, type = "response", newdata = simu)
head(Y.pred10)
T10 <- table(Y.pred10 > 0.5, simu$Y)
T10
(T10[1,2]+T10[2,1])/2000

Y.pred11 <- predict(model11, type = "response", newdata = simu)
head(Y.pred11)
T11 <- table(Y.pred11 > 0.5, simu$Y)
T11
(T11[1,2]+T11[2,1])/2000

Y.pred12 <- predict(model12, type = "response", newdata = simu)
head(Y.pred12)
T12 <- table(Y.pred12 > 0.5, simu$Y)
T12
(T12[1,2]+T12[2,1])/2000

Y.pred13 <- predict(model13, type = "response", newdata = simu)
head(Y.pred13)
T13 <- table(Y.pred13 > 0.5, simu$Y)
T13
(T13[1,2]+T13[2,1])/2000

Y.pred14 <- predict(model14, type = "response", newdata = simu)
head(Y.pred14)
T14 <- table(Y.pred14 > 0.5, simu$Y)
T14
(T14[1,2]+T14[2,1])/2000

## we write the predictions in a txt file
xsimu$Y.xpred <- predict(model3, type = "response", newdata = xsimu )

xsimu$predictions[xsimu$Y.xpred>0.5] <- 2
xsimu$predictions[xsimu$Y.xpred<=0.5] <- 1

write.table(xsimu$predictions,"predictions.txt", row.names=F,col.names=F)
