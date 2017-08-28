install.packages("caret")
library(caret)
library(kernlab)
data("spam")
head(spam)
str(spam)

# data spliting

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
head(inTrain)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)
dim(spam)

# regression
modelFit <- train(type ~ ., data = training, method = "glm")
modelFit$finalModel
modelFit
modelFit$finalModel



# predictions
predictions <- predict(modelFit, newdata = testing)
head(predictions)


# validation
confusionMatrix(predictions, testing$type)

#Principle component analysis
modelFit <- train(training$type ~ ., data = training, method = "glm", preProcess = "pca")
confusionMatrix(testing$type, predict(modelFit, testing))



########################################################

# ISLR package
install.packages("ISLR")
library(ISLR)
library(ggplot2)
data("Wage")
head(Wage)
wage
# data spliting
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
head(inTrain)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

# exploratory plots
featurePlot(x = training[,c("age", "education", "jobclass")]
            ,y = training$wage
            ,plot = "pairs")

qplot(age, wage, colour = jobclass, data = training)
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1

t1 <- table(cutWage, training$jobclass) 
t1
#proportions by row and columns
prop.table(t1,1)


qplot(wage, colour = education, data = training, geom = "density")

# preprocessing 
preObj <- preProcess(training[,-58], method = c("center", "scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve


# regression
modFit <- train(wage ~ age+jobclass+education, method = "lm", data = training)
modFit$finalModel
modFit
plot(modFit$finalModel)

pred <- predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing)

# Another regression
data("faithful")
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list = F)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)
lm1 <-  lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)$coef
modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit$finalModel)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")
lines(trainFaith$waiting, lm1$fitted, lwd =3)
predict(lm1, newdata = data.frame(waiting = 80))

plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = "blue", xlab = "Waiting", ylab = "Duration")
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd =3)


# Quiz#2

#q1
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
str(predictors); attributes(predictors)
head(predictors)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


#q4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
head(training)
head(testing)
trainIL <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
head(trainIL)
pca <- preProcess(trainIL, method = "pca", thresh = .9)
pca$numComp
pca


#q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# grep all columns with IL and diagnosis in the traning and testing set
trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL <- testing[,grep("^IL|diagnosis", names(testing))]

# non-PCA
model <- train(diagnosis ~ ., data = trainingIL, method = "glm")
predict_model <- predict(model, newdata= testingIL)
matrix_model <- confusionMatrix(predict_model, testingIL$diagnosis)
matrix_model$overall[1]

# PCA
modelPCA <- train(diagnosis ~., data = trainingIL, method = "glm", preProcess = "pca",trControl=trainControl(preProcOptions=list(thresh=0.8)))
matrix_modelPCA <- confusionMatrix(testingIL$diagnosis, predict(modelPCA, testingIL))
matrix_modelPCA$overall[1]


#q2
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
install.packages("GGally")

library(GGally)
library(Hmisc)
## Using ggpair
training2 <- training
#cut CompressiveStrength into 4 levels.  This is the only way to work with colour in ggpair
training2$CompressiveStrength <- cut2(training2$CompressiveStrength, g=4)
ggpairs(data = training2, columns = c("FlyAsh","Age","CompressiveStrength"), mapping = ggplot2::aes(colour = CompressiveStrength))

#q3
hist(log(training$Superplasticizer))
hist(log(training$Superplasticizer+1))