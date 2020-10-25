# Program's name: Getting and Cleaning Data Final Project 
# Programmer's name: LT
# Creation date: 10/25/2020
# Purpose: To get this course done

######################################
# Part 0: Download and prepare data #
#####################################

# Set up working directory 
setwd("C:/Users/LT/Dropbox/Coursera/GeetingAndCleaningData/Week4")

# Call for dplyr
library(dplyr)

# Create file name
filename <- "Coursera_GettingandCleaning_Final.zip"

# Download data
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
}  

# Unzip data
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

# Read data to create data frames 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

########################################################################
# Part 1: Merges the training and the test sets to create one data set #
########################################################################

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_data <- cbind(Subject, Y, X)
dim(Merged_data)

##################################################################################################
# Part 2: Extracts only the measurements on the mean and standard deviation for each measurement #
##################################################################################################

Tidydata <- Merged_data %>% select(subject, code, contains("mean"), contains("std"))

dim(Tidydata)

##################################################################################
# Part 3: Uses descriptive activity names to name the activities in the data set #
##################################################################################

Tidydata$code <- activities[Tidydata$code, 2]

#############################################################################
# Part 4: Appropriately labels the data set with descriptive variable names #
#############################################################################

names(Tidydata)[2] = "activity"
names(Tidydata)<-gsub("Acc", "Accelerometer", names(Tidydata))
names(Tidydata)<-gsub("Gyro", "Gyroscope", names(Tidydata))
names(Tidydata)<-gsub("BodyBody", "Body", names(Tidydata))
names(Tidydata)<-gsub("Mag", "Magnitude", names(Tidydata))
names(Tidydata)<-gsub("^t", "Time", names(Tidydata))
names(Tidydata)<-gsub("^f", "Frequency", names(Tidydata))
names(Tidydata)<-gsub("tBody", "TimeBody", names(Tidydata))
names(Tidydata)<-gsub("-mean()", "Mean", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-std()", "STD", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-freq()", "Frequency", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("angle", "Angle", names(Tidydata))
names(Tidydata)<-gsub("gravity", "Gravity", names(Tidydata))

######################################################################
# Step 5: From the data set in step 4, creates a second, independent #
# tidy data set with the average of each variable for each activity  #
# and each subject.                                                  # 
# ####################################################################

Finaldata <- Tidydata %>%
        group_by(subject, activity) %>%
        summarise_all(mean)
dim(Finaldata)
write.table(Finaldata, "Finaldata.txt", row.name=FALSE)






