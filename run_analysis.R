library(dplyr)

#Download the dataset

filename<- "assignment_dataset.zip"

#Before downloading let's check if archive already exists.
if(!file.exists(filename)){
  fileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,filename,method = "curl")
}

#Checking if folder exists
if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

#Read and assign data frames

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# 1.Merges the training and the test sets to create one data set

X.data <- rbind(x_train, x_test)
Y.data <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
data.all <- cbind(Subject, Y.data, X.data)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.

data.sub <- data.all %>% select(subject, code, contains("mean"), contains("std"))

# 3.Uses descriptive activity names to name the activities in the data set

data.sub$code <- activities[data.sub$code,2]

# 4.Appropriately labels the data set with descriptive variable names.

names(data.sub)[2] = "activity"
names(data.sub)<-gsub("Acc", "Accelerometer", names(data.sub))
names(data.sub)<-gsub("Gyro", "Gyroscope", names(data.sub))
names(data.sub)<-gsub("BodyBody", "Body", names(data.sub))
names(data.sub)<-gsub("Mag", "Magnitude", names(data.sub))
names(data.sub)<-gsub("^t", "Time", names(data.sub))
names(data.sub)<-gsub("^f", "Frequency", names(data.sub))
names(data.sub)<-gsub("tBody", "TimeBody", names(data.sub))
names(data.sub)<-gsub("-mean()", "Mean", names(data.sub), ignore.case = TRUE)
names(data.sub)<-gsub("-std()", "STD", names(data.sub), ignore.case = TRUE)
names(data.sub)<-gsub("-freq()", "Frequency", names(data.sub), ignore.case = TRUE)
names(data.sub)<-gsub("angle", "Angle", names(data.sub))
names(data.sub)<-gsub("gravity", "Gravity", names(data.sub))

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

IndependentData <- data.sub %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(IndependentData, "IndependentData.txt", row.name=FALSE)

print(str(IndependentData))

