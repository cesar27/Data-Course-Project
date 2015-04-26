# The run_script does the following tasks:

#1. Merges the training and the test sets to create one data set.

train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
x <- rbind(train, test)

train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
y <- rbind(train, test)

train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
z <- rbind(train, test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
index_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x <- x[, index_features]
names(x) <- features[index_features, 2]
names(x) <- gsub("\\(|\\)", "", names(x))
names(x) <- tolower(names(x))


#3. Uses descriptive activity names to name the activities in the data set.

activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
z[,1] = activities[z[,1], 2]
names(z) <- "activity"

#4. Appropriately labels the data set with descriptive variable names. 

names(y) <- "subject"
clean <- cbind(y, z, x)


#5. From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.

unique_subjects = unique(y)[,1]
num_subjects = length(unique(y)[,1])
num_activities = length(activities[,1])
num_cols = dim(clean)[2]
data_result = clean[1:(num_subjects*num_activities), ]

row = 1
for (sub in 1:num_subjects) {
  for (act in 1:num_activities) {
    data_result[row, 1] = unique_subjects[sub]
    data_result[row, 2] = activities[act, 2]
    data_prev <- clean[clean$subject==sub & clean$activity==activities[act, 2], ]
    data_result[row, 3:num_cols] <- colMeans(data_prev[, 3:num_cols])
    row = row+1
  }
}
write.table(data_result, "data_averages.txt")
