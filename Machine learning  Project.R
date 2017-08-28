## download data
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileURL,destfile="./data/pml-training.csv")
fileURLT <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileURLT,destfile="./data/pml-testing.csv")


# read file
train <- read.csv("./data/pml-training.csv")
test <- read.csv("./data/pml-testing.csv")
dim(train)
dim(test)
str(train)

# data processing
library(caret)
# remove near zero variance variables
NZV <- nearZeroVar(train)
train <- train[, -NZV]
test  <- test[,-NZV]

# remove first 5 variables
train <- train[, -(1:5)]
test  <- test[,-(1:5)]


# remove variables that are mostly NA's (> 95%) 
indices <- sapply(train, is.na)
percent <- colSums(indices)/(dim(train)[1])
rm <- percent>.95

train <- train[,!rm]
test <- test[,!rm]


# Splitting the trainning set into a train and validation set
inTrain <- createDataPartition(y = train$classe, p = 0.7, list = F)
trainT <- train[inTrain,]
valT <- train[-inTrain,]



# models 
# classification tree
set.seed(110415)
library(rpart)
modCT <- train(classe ~ . , method = "rpart", trControl = trainControl(method = "cv", number = 5), data = trainT)
modCT$finalModel
#install.packages("rattle")
library(rattle)
fancyRpartPlot(modCT$finalModel)

# test modCT
confusionMatrix(predict(modCT, valT), valT$classe)$overall[1]



# cheking corelations
trainC <- trainT
trainC$classe <- as.numeric(trainC$classe)
cormatrix <- data.frame(cor(trainC[,-c(1)]))
library(plyr)
tail(arrange(data.frame(cbind(names(cormatrix), cormatrix$classe)),X2),10)

# parallel processing
# 
# library(parallel)
# library(doParallel)
# cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
# registerDoParallel(cluster)
# 
# fitControl <- trainControl(method = "cv",
#                            number = 10,
#                            allowParallel = TRUE)
# 
# modRFall <- train(classe ~ ., method="rf",data= trainT,trControl = fitControl)
# 
# stopCluster(cluster)
# registerDoSEQ()


library(randomForest)

modRFcor <- randomForest(classe ~ pitch_forearm + magnet_arm_x + accel_arm_x + total_accel_forearm + magnet_dumbbell_z + accel_dumbbell_x + roll_arm
                , data = trainT
                )
confusionMatrix(predict(modRFcor, valT), valT$classe)$overall[1]   
        

        
modRFall<- randomForest(classe ~ ., data=trainT)
confusionMatrix(predict(modRFall, valT), valT$classe)$overall[1]


# test 
q <- predict(modFitB1, test)
