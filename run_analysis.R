#1.Merges the training and the test sets to create one data set

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")

X_data <- rbind(X_test,X_train, make.row.names = FALSE)
Y_data <- rbind(Y_test,Y_train, make.row.names = FALSE)
Subject_data <- rbind(Subject_test,Subject_train, make.row.names = FALSE)

# Set colum names
temp1 <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
colnames(X_data) <- temp1[,2]
colnames(Y_data) <- "Activity_code"
colnames(Subject_data) <- "Subject"

# Merge datasets
Data1 <- cbind(Y_data, Subject_data, X_data)

# 2. Extracts only the measurements on the mean and standard deviation (std) for each measurement

Data1_subject <- grep("Subject", names(Data1), ignore.case = TRUE, perl = FALSE, value = FALSE, fixed = FALSE)
Data1_activity <- grep("Activity_code", names(Data1), ignore.case = TRUE, perl = FALSE, value = FALSE, fixed = FALSE)
Data1_mean <- grep("mean", names(Data1), ignore.case = TRUE, perl = FALSE, value = FALSE, fixed = FALSE)
Data1_std <- grep("std", names(Data1), ignore.case = TRUE, perl = FALSE, value = FALSE, fixed = FALSE)
temp2 <- c(Data1_activity, Data1_subject, Data1_mean, Data1_std)
temp3 <- sort(temp2)
Data2 <- Data1[, temp3]

# 3. Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
colnames(activity_labels) <- c("Activity_code", "Activity_names")

Data3 <- merge(activity_labels, Data2,by ="Activity_code", all.x = TRUE)


# 4. Appropriately labels the data set with descriptive variable names

names(Data3)<-gsub("std()", "SD", names(Data3))
names(Data3)<-gsub("mean()", "MEAN", names(Data3))
names(Data3)<-gsub("^t", "time", names(Data3))
names(Data3)<-gsub("^f", "frequency", names(Data3))
names(Data3)<-gsub("Acc", "Accelerometer", names(Data3))
names(Data3)<-gsub("Gyro", "Gyroscope", names(Data3))
names(Data3)<-gsub("Mag", "Magnitude", names(Data3))
names(Data3)<-gsub("BodyBody", "Body", names(Data3))

# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

Data_tidy <- ddply(Data3, .(Activity_code, Subject), numcolwise(mean))
write.table(Data_tidy, file = "Data_Tidy.txt", sep = "\t", row.names = FALSE)
