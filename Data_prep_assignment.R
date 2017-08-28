## download and unzip
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Assignment.zip")
unzip(zipfile="./data/Assignment.zip",exdir="./data")

## view directories and files
list.dirs()
list.files("./data/UCI HAR Dataset")
list.files("./data/UCI HAR Dataset/test")
list.files("./data/UCI HAR Dataset/train")


## read data into vectors
## Test set
xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")  ## contains features data
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")  ## contains activity ids
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")  ## contains subject ids
## Trainning set
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")  ## contains features data
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")  ## contains activity ids
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")  ## contains subject ids
## Feature names and activity labels
features <- read.table('./data/UCI HAR Dataset/features.txt')
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

## Task 4 : Appropriately labels the data set with descriptive variable names
colnames(xTest) <- features[,2]
colnames(xTrain) <- features[,2]
colnames(yTest) <- "activityId"
colnames(yTrain) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(subjectTrain) <- "subjectId"
colnames(activityLabels) <- c("activityId", "activityName")

## Task 1 : Merge the training and the test sets to create one data set
test <- cbind(subjectTest, yTest, xTest)
train <- cbind(subjectTrain, yTrain, xTrain)
mergedSet <- rbind(train,test)

## Task 2 : Extract only the measurements on the mean and standard deviation for each measurement.
columnSelect <-  (grepl("subjectId" , colnames(mergedSet))|grepl("activityId" , colnames(mergedSet))|grepl("mean." , colnames(mergedSet))|grepl("std." , colnames(mergedSet)))
meanStd <- mergedSet[, columnSelect]

## Task 3 : Use descriptive activity names to name the activities in the data set
meanStd <- merge(meanStd, activityLabels, by = "activityId")

## Task 5 : Creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidySet <- aggregate(. ~subjectId + activityId + activityName, meanStd, mean)
tidySetSorted <-tidySet[order(tidySet$subjectId, tidySet$activityId),]

## output
write.table(tidySetSorted, "./data/tidySet.txt", row.name= F )

library(httr)
set_config(use_proxy(url="10.42.253.9",port=80, username="shnandy", password="June##2017"))
install_from_swirl("Exploratory Data Analysis")

