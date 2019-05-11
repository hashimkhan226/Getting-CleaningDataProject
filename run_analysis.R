library("dplyr")

trainData <- read.table("./train/X_train.txt", header = FALSE, sep = "", dec = ".")

testData <- read.table("./test/X_test.txt", header = FALSE, sep = "", dec = ".")

activityTest <- read.table("./test/Y_test.txt", header = FALSE, sep = "", dec = ".")

activityTrain <- read.table("./train/Y_train.txt", header = FALSE, sep = "", dec = ".")

features <- read.table("./features.txt", header = FALSE, sep = "", dec = ".")

subjects_test <- read.table("./test/subject_test.txt", header = FALSE, sep = "", dec = ".")

subjects_train <- read.table("./train/subject_train.txt", header = FALSE, sep = "", dec = ".")

testData <- cbind(subjects_test, activityTest, testData)

trainData <- cbind(subjects_train, activityTrain, trainData)

combinedData <- rbind(trainData, testData)

features <- as.character(features[,2])

features <- sub("t", "(Time) ", features)
features <- sub("f", "(Frequency) ", features)
features <- sub("Acc", "Acceleration ", features)
features <- sub("Gyro", "Angular Velocity ", features)
features <- sub("Mag", "Magnitude ", features)
features <- sub("Jerk", "Jerk ", features)

features <- sub("-mean\\(\\)", "Mean", features)
features <- sub("-std\\(\\)", "Standard Deviation", features)

names(combinedData)[3:563] <- features

std_list <- grep("Standard Deviation",features)
mean_list <- grep("Mean", features)
ms_list <- sort(c(mean_list, std_list))
ms_list <- ms_list + 2
features_ms <- features[ms_list]
combinedData <- combinedData[,c(1,2,ms_list)]

names(combinedData)[1] <- "Subject"
names(combinedData)[2] <- "Activity"

activities <- as.character(combinedData$Activity)
activities <- sub("1", "Walking", activities)
activities <- sub("2", "Walking Upstairs", activities)
activities <- sub("3", "Walking Downstairs", activities)
activities <- sub("4", "Sitting", activities)
activities <- sub("5", "Standing", activities)
activities <- sub("6", "Laying", activities)
combinedData$Activity <- activities

combinedData_tb <- tbl_df(combinedData)

by_subject_activity <- group_by(combinedData_tb, Subject, Activity)

averages <- summarize_all(by_subject_activity, mean)

print("done")