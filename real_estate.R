#Real estate prediction - LM 

Real_estate$No<-NULL
Real_estate$X1.transaction.date<-NULL

str(Real_estate)

boxplot(Real_estate$Y.house.price.of.unit.area)
#no outliers

set.seed(1)
index<-sample(nrow(Real_estate),0.75*nrow(Real_estate))
head(index)

train_real_estate<-Real_estate[index,]
test_real_estate<-Real_estate[-index,]

real_estate_model<-lm(Y.house.price.of.unit.area~.,data=Real_estate)
summary(real_estate_model)

library(car)
vif(real_estate_model)
#no multicollinearity

durbinWatsonTest(real_estate_model)
#no autocorrelation


train_real_estate$price<-predict(real_estate_model,train_real_estate)
head(train_real_estate)

library(caret)
RMSE(train_real_estate$price, train_real_estate$Y.house.price.of.unit.area)
#9.37

test_real_estate$price<-predict(real_estate_model,test_real_estate)
head(test_real_estate)

RMSE(test_real_estate$price,test_real_estate$Y.house.price.of.unit.area)
#7.31



#Real estate - Random Forest

library(randomForest)

set.seed(2)
real_estate_rf<-randomForest(Y.house.price.of.unit.area~., data=Real_estate)
real_estate_rf

pred_real_estate<-predict(real_estate_rf,Real_estate)

RMSE(pred_real_estate,Real_estate$Y.house.price.of.unit.area)
#4.06