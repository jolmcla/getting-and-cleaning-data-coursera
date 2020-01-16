
# Load libraries
library(readr)
library(dplyr)
# download in path directory ./project
 download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./motion.zip", mode='wb')
 unzip("./motion.zip")

## establishing paths
train_path<- "./project/train/X_train.txt"
test_path <- "./project/test/X_test.txt"
features_path <- "./project/features.txt"
y_train_path <- "./project/train/y_train.txt"
y_test_path <- "./project/test/y_test.txt"
subject_train_path <- "./project/train/subject_train.txt"
subject_test_path <- "./project/test/subject_test.txt"
activity_path <- "./project/activity_labels.txt"

## Reading
feature_names <- read_table(features_path, col_names = FALSE)
feature_names <- feature_names[[1]]
feature_names <- gsub('[[:digit:]]+', '', feature_names)
subject_train_table <- read_table(subject_train_path, col_names = "subject")
subject_test_table <- read_table(subject_test_path, col_names="subject")
train_data <- read_table(train_path, col_names = feature_names)
train_y <- read_table(y_train_path, col_names = "activity_name")
test_data <- read_table(test_path, col_names = feature_names)
test_y<- read_table(y_test_path, col_names = "activity_name")
activity <- read_table(activity_path, col_names = FALSE)
activity <- select(activity, X2)

# Merging
train_data <- bind_cols(train_data, train_y)
train_data_subject <- bind_cols(train_data, subject_train_table)
test_data <- bind_cols(test_data, test_y)
test_data_subject <- bind_cols(test_data, subject_test_table)
dataset_total<- bind_rows(train_data_subject, test_data_subject)

# Selecting the mean and standard deviation for each measurement
names_mean<-names(select(dataset_total, grep(pattern = "mean", x = names(dataset_total))))
names_std<-names(select(dataset_total, grep(pattern = "std", x = names(dataset_total))))
dataset_total_selected<-select(dataset_total, c(names_mean,names_std))
dataset_total_selected_subject_activity <- bind_cols(dataset_total["subject"], dataset_total["activity_name"], dataset_total_selected)

 #  Doing descriptive activity names to name the activities in the data set

dataset_total_selected_subject_activity["activity_name"] <- sapply(select(dataset_total_selected_subject_activity,activity_name), function(x){activity[x,]})
variable_names <- names(dataset_total_selected_subject_activity )

# Labelling the data set with descriptive variable names
variable_names <- sapply(variable_names, tolower)
variable_names <- unname(obj = variable_names)

variable_names <- sub(pattern = "^t", replacement = "time", x = variable_names)
variable_names <- sub(pattern = "acc", replacement = "Acceleration", x = variable_names)
variable_names <- sub(pattern = "f", replacement = "Frequency", x = variable_names)
variable_names <- gsub(pattern = "-", replacement = "", x = variable_names)
variable_names <- sub(pattern = "gyro", replacement = "Gyroscope", x = variable_names)
variable_names <- sub(pattern = "std", replacement =  "_StandardDeviation", x = variable_names)
variable_names <- sub(pattern = "\\(\\)", replacement = "", x = variable_names)
variable_names <- sub(pattern = "mag", replacement = "Magnitude", x = variable_names)
variable_names <- sub(pattern = "mean", replacement = "_Mean", x = variable_names)
#variable_names <- sub(pattern = "^activity", replacement = "Activity.", x = variable_names)
variable_names <- sub(pattern = "body", replacement = "Body", x = variable_names)
variable_names <- sub(pattern = "jerk", replacement = "Jerk", x = variable_names)
variable_names <- sub(pattern = "y$", replacement = "_Y", x = variable_names)
variable_names <- sub(pattern = "x$", replacement = "_X", x = variable_names)
variable_names <- sub(pattern = "z$", replacement = "_Z", x = variable_names)
names(dataset_total_selected_subject_activity ) <- variable_names

# Generating tidy data 
# average of each variable for each activity and each subject
mean_dataset_total_activity_subject <- dataset_total_selected_subject_activity %>% group_by(subject, activity_name)%>% summarise_all(.funs = mean)
write.table(mean_dataset_total_activity_subject , file = "tidy_data.txt")

