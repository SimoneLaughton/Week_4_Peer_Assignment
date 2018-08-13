# Getting and Cleaning Data - Week 4 Assignment
# August 12, 2018

# Reference #1:  Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
#       A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European 
#       Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning,
#       ESANN 2013. Bruges, Belgium 24-26 April 2013. Retrieved from
#       http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.
# Reference #2: Wickham, H. (2014). Tidy data.  Journal of Statistical Software, 59(10).  
#       Retrieved from https://www.jstatsoft.org/article/view/v059i10.

# run_analysis.R fulfills the following requirements:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the mean and standard deviation measurements.
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately label the data set with descriptive variable names.
# 5. From #4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Note this has not been done in order listed above.
# Note I have added ">" in front of the lines of code that were used in R.  To run this file, enter the text beside the ">" character in front of each item below.

# Load reshape2 library
library(reshape2)
filename <- "getdata_dataset.zip"

# Download the dataset and unzip it.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, filename, method="curl")
} 
if (!file.exists("UCI HAR Dataset")) { 
        +     unzip(filename) 
}
# Load the activity labels and the feature variables
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activitylabels[,2] <- as.character(activitylabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Grab only data that indicates by column name that it has mean or standard deviation values
# The item below completes requirement #2 by separating out features for mean and standard deviation
featuresmeanstd <- grep("*.mean.*|*.std.*|^angle.*", features[,2])

# Rename the data to lowercase and remove characters such as double brackets, dash and underscore
# Together with the Codebook, the items below fulfill requirement #4.
rename <- features[featuresmeanstd,2]
rename = gsub("-mean", "mean", rename)
rename = gsub("-std", "std", rename)
rename = gsub("[-()]", "", rename)
rename = gsub("A", "a", rename)
rename = gsub("B", "b", rename)
rename = gsub("F", "f", rename)
rename = gsub("G", "g", rename)
rename = gsub("J", "j", rename)
rename = gsub("M", "m", rename)
rename = gsub("X", "x", rename)
rename = gsub("Y", "y", rename)
rename = gsub("Z", "z", rename)
rename = gsub("[_]", "", rename)

# Read the datasets and bring together the subjects and activities from training and test datasets
# Below are the first steps towards fulfilling requirement #1.
train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresmeanstd]
trainactivities <- read.table("UCI HAR Dataset/train/y_train.txt")
trainsubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainsubjects, trainactivities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresmeanstd]
testactivities <- read.table("UCI HAR Dataset/test/y_test.txt")
testsubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testsubjects, testactivities, test)

# Merge the datasets together
# The item below completes requirement #1.
ucidata <- rbind(train, test)

# Below updates are made to the labels as noted in requirement #4.
colnames(ucidata) <- c("subject", "activity", rename)

# Change subjects and activities into factors
ucidata$subject <- as.factor(ucidata$subject)

# Below fulfills requirement #3 by using the activity labels instead of the numbers.
ucidata$activity <- factor(ucidata$activity, levels = activitylabels[,1], labels = activitylabels[,2])

# Melt the data so that each row is a unque id-variable combination, and cast was used for the observational data averages 
# Below is the first part of completing requirement #5.
ucidatamelt <- melt(ucidata, id = c("subject", "activity"))
ucidatamean <- dcast(ucidatamelt, subject + activity ~ variable, mean)

# Write a tidy table of the resulting data set
# The item below completes requirement #5.
write.table(ucidatamean, "./tidydata.txt", row.names = FALSE, quote = FALSE, sep='\t')
