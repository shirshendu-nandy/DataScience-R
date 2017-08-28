data(iris)
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
train <- iris[inTrain,]
test <- iris[-inTrain,]
dim(train);dim(test)
qplot(Petal.Width, Sepal.Width, colour = Species, data = train)

modFit <- train(Species ~ . , method = "rpart", data = train)
modFit$finalModel


#install.packages("rattle")
library(rattle)
library(rpart)
fancyRpartPlot(modFit$finalModel)
#install.packages('rpart.plot')


## Bagging

# install.packages("ElemStatLearn")
library(ElemStatLearn)
data("ozone")
dim(ozone)
head(ozone)
ozone <- ozone[order(ozone$ozone),]

ll <- matrix(NA, nrow = 10, ncol = 155)
for(i in 1:10) {
        ss <- sample(1:dim(ozone)[1], replace = T)
        ozone0 <- ozone[ss,];ozone0 <- ozone0[order(ozone0$ozone),]
        loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
        ll[i,] <- predict(loess0, newdata = data.frame(ozone=1:155))
}



plot(ozone$ozone, ozone$temperature, pch =19, cex =0.5)
for(i in 1:10){lines(1:155, ll[i,], col = "grey", lwd =2)}        
lines(1:155, apply(ll,2,mean), col ="red", lwd =2)




## random forest

data(iris)
library(ggplot2)
library(caret)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
train <- iris[inTrain,]
test <- iris[-inTrain,]

modFit <- train(Species ~ . , method = "rf", data = train, prox = T)
modFit
pred <- predict(modFit, test) 
cf <- confusionMatrix(pred, test$Species)
cf$table


## boosting
install.packages("ISLR")
library(ISLR)
data(Wage)
head(Wage)
library(ggplot2)
library(caret)
wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y= wage$wage, p =0.7, list = F)
train <- wage[inTrain,]
test <- wage[-inTrain,]

modFit <- train(wage ~ ., method = "gbm", data = train, verbose = F)
pred <- predict(modFit, test) 
qplot(pred, wage, data = test)




##quiz
#q1
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
head(segmentationOriginal)
inTrain <- which(segmentationOriginal$Case == "Train")
train <- segmentationOriginal[inTrain,]
test <- segmentationOriginal[-inTrain,]
dim(segmentationOriginal)
dim(train)
dim(test)
table(segmentationOriginal$Case)
set.seed(125)
modFit <- train(Class ~ . , method = "rpart", data = train)
modFit$finalModel
#install.packages("rattle")
installed.packages("rpart.plot")
library(rattle)
library(rpart)
fancyRpartPlot(modFit$finalModel)


#q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

modFit <- train(y ~ . , method = "rf", data = vowel.train, prox = T)
modFit
pred <- predict(modFit, vowel.test) 
cf <- confusionMatrix(pred, vowel.test$y)
cf$table
varImp(modFit)

modFitRF <- randomForest(y~., data = vowel.train, importance = T, proximity =T )
varImp(modFitRF)
modFitRF$importance


#q3

install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
head(olive)
modFit <- train(Area ~ . , method = "rpart", data = olive)
modFit$finalModel
fancyRpartPlot(modFit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
pred <- predict(modFit, newdata) 
pred



#q4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modFit <-  glm(chd ~ age+alcohol+obesity+typea+tobacco+ldl, family = binomial, data = trainSA)
pred <- predict(modFit, testSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA, pred)
missClass(trainSA, pred)
cf <- confusionMatrix(pred, testSA$)
cf$table
