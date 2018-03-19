## Loading necessary packages
library(dplyr)
library(tidyr)

## Checking for and creating the directory
if(!file.exists("data")) {
    dir.create("data")
}

## Downloading dataset from the web
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/activity.zip")
list.files("./data")
if(!file.exists("UCI HAR Dataset")) {
    unzip("./data/activity.zip")
}

## Importing labels and features
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

## Importing "train" sets
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainX <- read.table("./UCI HAR Dataset/train/x_train.txt")
trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")

## Importing "test" sets
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testX <- read.table("./UCI HAR Dataset/test/x_test.txt")
testY <- read.table("./UCI HAR Dataset/test/y_test.txt")

## Naming the columns
colnames(activityLabels) <- c("Activity_ID", "Type")
colnames(subjectTrain) <- ("Subject_ID")
colnames(subjectTest) <- ("Subject_ID")
colnames(trainX) <- features[,2] 
colnames(trainY) <- "Activity_ID"
colnames(testX) <- features[,2]
colnames(testY) <- "Activity_ID"

## Merging the "train" and "test" sets into one data set
trainSet <- cbind(subjectTrain, trainX, trainY)
testSet <- cbind(subjectTest, testX, testY)
completeSet <- rbind(trainSet, testSet)

## Extracting the mean and standard deviation for each measurement
indexOfMean <- grep("mean", colnames(completeSet)) 
indexOfSTD <- grep("std", colnames(completeSet))
indexOfFeatures <- c(1, 563, indexOfMean, indexOfSTD)
datasetMeanSTD <- completeSet[,indexOfFeatures]

## Removing unappealing characters from column names
newColNames <- gsub("\\(\\)","",colnames(datasetMeanSTD)) 
colnames(datasetMeanSTD) <- newColNames

## Replacing activity IDs with activity names
datasetMeanSTD[,"Activity_ID"] <- gsub("1", "Walking", datasetMeanSTD[,"Activity_ID"])
datasetMeanSTD[,"Activity_ID"] <- gsub("2", "WalkingUpstairs", datasetMeanSTD[,"Activity_ID"])
datasetMeanSTD[,"Activity_ID"] <- gsub("3", "WalkingDownstairs", datasetMeanSTD[,"Activity_ID"])
datasetMeanSTD[,"Activity_ID"] <- gsub("4", "Sitting", datasetMeanSTD[,"Activity_ID"])
datasetMeanSTD[,"Activity_ID"] <- gsub("5", "Standing", datasetMeanSTD[,"Activity_ID"])
datasetMeanSTD[,"Activity_ID"] <- gsub("6", "Laying", datasetMeanSTD[,"Activity_ID"])

## Summarizing the tidy data set
result <- gather(datasetMeanSTD, key = "Measurement", "MeasurementValue", -Activity_ID, -Subject_ID) %>%
  group_by(Activity_ID, Subject_ID, Measurement) %>%
  summarize(MeasurementMean = mean(MeasurementValue), MeasurementSTD = sd(MeasurementValue))
write.table(result, "./data/results.txt", row.names = FALSE)