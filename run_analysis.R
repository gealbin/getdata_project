## This analysis reads data from the proyect's data set Human Activity Recognition Using Smartphones,
## the dataset can be found on: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## The script reads the data file and merge it content to create two tidy datasets called tidy_data_1 and tidy_data_2
## Those datasets were meet the following conditions:
##     Merges the training and the test sets to create one data set.
##     Extracts only the measurements on the mean and standard deviation for each measurement. 
##     Uses descriptive activity names to name the activities in the data set
##     Appropriately labels the data set with descriptive variable names. 
##     From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Read the files and set column names
features <- read.table("features.txt", col.names = c("id", "feature"))
activity_labels <- read.table("activity_labels.txt", col.names = c("id", "activity_label"))
subject_test <- read.table("./test/subject_test.txt", col.names = c("subject"))
subject_train <- read.table("./train/subject_train.txt", col.names = c("subject"))
X_test <- read.table("./test/X_test.txt", col.names = features$feature)
X_train <- read.table("./train/X_train.txt", col.names = features$feature)

## Read y data and merge with activity label, I added row_number to order the y data as it was after merging
y_test <- read.table("./test/y_test.txt")
y_test <- cbind.data.frame(row_origin = as.integer(rownames(y_test)),y_test)
y_test <- merge(y_test, activity_labels,by.x = "V1", by.y = "id")
y_test <- y_test[order(y_test$row_origin), ]

y_train <- read.table("./train/y_train.txt")
y_train <- cbind.data.frame(row_origin = as.integer(rownames(y_train)),y_train)
y_train <- merge(y_train, activity_labels, by.x = "V1", by.y = "id")
y_train <- y_train[order(y_train$row_origin), ]

## add correspond activity and subject to X_test and X_train which are the set of measured features
data_test <- cbind(subject_test, activity_label = y_test$activity_label, X_test)
data_train <- cbind(subject_train, activity_label = y_train$activity_label, X_train)

## Merge train and test data sets
data <- rbind(data_train,data_test)

## Filter only mean and std features columns (subject and activity_level columns remains in the output data set)
tidy_data_1 <- data[ ,grepl("mean", names(data)) | grepl("std", names(data)) 
                     | names(data) == "subject" | names(data) == "activity_label"]

##-------------------------------------------------------------------------------------------------------

## Split dataset by subject and activity_level
tidy_data_2 <- split(tidy_data_1[ ,grepl("mean", names(tidy_data_1)) | grepl("std", names(tidy_data_1))]
                     , list(tidy_data_1$activity_label,tidy_data_1$subject))

## Apply mean to each column of splitted data frame
tidy_data_2 <- lapply(tidy_data_2,function(x) colMeans(x))

## Reshape data as dataframe
tidy_data_2 <- data.frame(t(as.data.frame(tidy_data_2)))