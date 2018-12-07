#Quiz 5

##Question1
library(ElemStatLearn)
library(caret)
library(dplyr)

data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

rfModel <- train(y ~ ., method="rf", data=vowel.train)
boostModel <- train(y ~., method="gbm", data=vowel.train)

rfPred <- predict(rfModel, vowel.test)
boostPred <- predict(boostModel, vowel.test)

final <- data.frame(factor(rfPred),factor(boostPred),factor(vowel.test$y))

confusionMatrix(final$factor.rfPred.,final$factor.vowel.test.y.)
confusionMatrix(final$factor.boostPred.,final$factor.vowel.test.y.)

final$agree <- ifelse(final$factor.rfPred.==final$factor.boostPred.,final$factor.rfPred.,final$factor.vowel.test.y.)

final2 <- final %>% filter(agree != "No")
confusionMatrix(factor(final2$agree), final2$factor.vowel.test.y.)

##Question2
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

rfModel <- train(diagnosis ~ ., model="rf", data=training)
gbmModel <- train(diagnosis ~ ., model="gbm", data=training)
ldaModel <- train(diagnosis ~ ., model="lda",data=training)

rfPred <- predict(rfModel, testing)
gbmPred <- predict(gbmModel, testing)
ldaPred<- predict(ldaModel, testing)

stack <- data.frame(rfPred,gbmPred,ldaPred,testing$diagnosis)

stackModel <- train(testing.diagnosis ~ ., method="rf", data=stack)

stackPred <- predict(stackModel, stack)

confusionMatrix(rfPred, testing$diagnosis)
confusionMatrix(gbmPred, testing$diagnosis)
confusionMatrix(ldaPred, testing$diagnosis)
confusionMatrix(stackPred, testing$diagnosis)

##Question3

set.seed(3523)

library(AppliedPredictiveModeling)
library(caret)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

lassModel <- train(CompressiveStrength ~ ., method ="lasso", data = training)
?plot.enet

par(mfrow=c(2,2))
plot(lassModel$finalModel)


##Question4
library(lubridate) # For year() function below
library(forecast)
temp<-tempfile()
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", destfile = temp)
dat = read.csv(temp)

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

bModel<- bats(tstrain)
plot(forecast(bModel))
lines(testing$visitsTumblr,col="red")

##Question 5
set.seed(3523)

library(AppliedPredictiveModeling)
library(caret)
library(e1071)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
svmModel <- svm(CompressiveStrength ~ ., data = training)
svmPred <- predict(svmModel, testing)

confusionMatrix(svmPred,factor(testing$CompressiveStrength))

final <- data.frame(a=unname(svmPred), b=testing$CompressiveStrength)

confusionMatrix(final$b,final$a)
?confusionMatrix

rmse(testing$CompressiveStrength,svmPred)
