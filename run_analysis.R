#load dplyr library to be able to use table data frame functions
library("dplyr")

#Read in train data
trainData <- read.table("./train/X_train.txt", header = FALSE, sep = "", dec = ".")

#Read in test data
testData <- read.table("./test/X_test.txt", header = FALSE, sep = "", dec = ".")

#Read in activity labels for test data
activityTest <- read.table("./test/Y_test.txt", header = FALSE, sep = "", dec = ".")

#Read in activity labels for train data
activityTrain <- read.table("./train/Y_train.txt", header = FALSE, sep = "", dec = ".")

#Read in list of features
features <- read.table("./features.txt", header = FALSE, sep = "", dec = ".")

#Read in subjects for test data
subjects_test <- read.table("./test/subject_test.txt", header = FALSE, sep = "", dec = ".")

#Read in subjects for train data
subjects_train <- read.table("./train/subject_train.txt", header = FALSE, sep = "", dec = ".")

#Add subject and activity label columsn to test data
testData <- cbind(subjects_test, activityTest, testData)

#Add subject and activity label columsn to train data
trainData <- cbind(subjects_train, activityTrain, trainData)

#combine the test and train datasets together
combinedData <- rbind(trainData, testData)

#Convert features list to character for processing
features <- as.character(features[,2])

#Substitute descriptive full words for variables
features <- sub("t", "(Time) ", features)
features <- sub("f", "(Frequency) ", features)
features <- sub("Acc", "Acceleration ", features)
features <- sub("Gyro", "Angular Velocity ", features)
features <- sub("Mag", "Magnitude ", features)
features <- sub("Jerk", "Jerk ", features)

features <- sub("-mean\\(\\)", "Mean", features)
features <- sub("-std\\(\\)", "Standard Deviation", features)

#Attach revised features list with proper variable names to actual dataset
names(combinedData)[3:563] <- features

#Isolate only variables relating to Mean and Standard Deviation
std_list <- grep("Standard Deviation",features)
mean_list <- grep("Mean", features)
ms_list <- sort(c(mean_list, std_list))
ms_list <- ms_list + 2
features_ms <- features[ms_list]
combinedData <- combinedData[,c(1,2,ms_list)]

#Name the Subject and Activity columns
names(combinedData)[1] <- "Subject"
names(combinedData)[2] <- "Activity"

#Re-name the entries in the Activity column to be more descriptive
activities <- as.character(combinedData$Activity)
activities <- sub("1", "Walking", activities)
activities <- sub("2", "Walking Upstairs", activities)
activities <- sub("3", "Walking Downstairs", activities)
activities <- sub("4", "Sitting", activities)
activities <- sub("5", "Standing", activities)
activities <- sub("6", "Laying", activities)
combinedData$Activity <- activities

#Convert dataframe to a table
combinedData_tb <- tbl_df(combinedData)

#Group table by subject and activity
by_subject_activity <- group_by(combinedData_tb, Subject, Activity)

#Get averages of subjects and activity groups
averages <- summarize_all(by_subject_activity, mean)

#Export to a txt file
write.table(averages, file = "tidy.txt")