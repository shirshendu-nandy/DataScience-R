library(ISLR)
library(caret)
wage <- subset(Wage, select = -c(logwage))

# create a validation and building data set
inBuild <- createDataPartition(y = wage$wage, p = 0.7, list = F)
validation <- wage[-inBuild,]; build <- wage[inBuild,]

# create a test and trainning data set splitting the building set
inTrain <- createDataPartition(y = build$wage, p = 0.7, list = F)
test <- build[-inTrain,]; train <- build[inTrain,]

dim(validation);dim(test);dim(train)


# build two different models

mod1 <- train(wage~., method = "glm", data = train)
mod2 <- train(wage~., method = "rf", data = train, trControl = trainControl(method = "cv"), number =3)

# predict on the test set
pred1 <- predict(mod1, test);pred2 <- predict(mod2, test)

# combinning the predictors
df <- data.frame(pred1, pred2, wage = test$wage)
modC <- train(wage ~ ., method = "gam", data = df )
predT <- predict(modC, df)
# error 
sqrt(sum((predT - test$wage)^2))

# predict on validation set
predV1 <- predict(mod1, validation);predV2 <- predict(mod2, validation)
dfV <- data.frame(pred1 = predV1, pred2 = predV2)
head(dfV)
predV <- predict(modC, dfV)

# error
sqrt(sum((predV - validation$wage)^2))







#q1


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- factor(vowel.train$y); vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

mod1 <- train( y~., method = "rf", data = vowel.train)
mod2 <- train( y~., method = "gbm", data = vowel.train, verbose = F)

pred1 <- predict(mod1, vowel.test)
pred2 <- predict(mod2, vowel.test)

cf1 <- confusionMatrix(pred1, vowel.test$y)
cf1$overall[1]

cf2 <- confusionMatrix(pred2, vowel.test$y)
cf2$overall[1]

# Accuracy among the test set samples where the two methods agree
df <- data.frame(pred1, pred2, y = vowel.test$y)

# both model agrees
sum(pred1[df$pred1 == df$pred2] == df$y[df$pred1 == df$pred2]) / sum(df$pred1 == df$pred2)



#q2


library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)


mod1 <- train(diagnosis ~., method = "rf", data = training)
mod2 <- train(diagnosis ~., method = "gbm", data = training, verbose = F)
mod3 <- train(diagnosis ~., method ="lda", data = training)

pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
pred3 <- predict(mod3, testing)


df <- data.frame(pred1, pred2, pred3, diagnosis = testing$diagnosis)
modC <- train(diagnosis ~ ., method = "rf", data = df )
predT <- predict(modC, df)

round(confusionMatrix(pred1, testing$diagnosis)$overall[1],3)
round(confusionMatrix(pred2, testing$diagnosis)$overall[1],3)
round(confusionMatrix(pred3, testing$diagnosis)$overall[1],3)
round(confusionMatrix(predT, testing$diagnosis)$overall[1],3)



#q3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
mod <- train(CompressiveStrength ~ ., method = "lasso", data = training)
mod$finalModel

library(elasticnet)
plot.enet(mod$finalModel, xvar = "penalty", use.color = T)
#The coefficient path shows that the variable Cement is the last coefficient to be set to zero as the penalty increases.


#q5

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
error <- testing$CompressiveStrength - pred_svm
sqrt(mean(error^2))


#q4
https://rpubs.com/calin/predmachlearn-033-finalQuiz

