# extract data from files
activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt")
features <- read.table("UCI HAR Dataset\\features.txt")
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
x_test <- read.table("UCI HAR Dataset\\test\\x_test.txt")
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt")
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
x_train <- read.table("UCI HAR Dataset\\train\\x_train.txt")
y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt")

# add subject & activity data, then merge the training and test sets
test_df <- cbind(subject_test, y_test)
test_df <- cbind(test_df, x_test)
train_df <- cbind(subject_train, y_train)
train_df <- cbind(train_df, x_train)
merged_df <- rbind(test_df, train_df)

# add column names describing the variables
colnames(merged_df)[1:2] <- c("Subject", "Activity")
colnames(merged_df)[3:length(colnames(merged_df))] <- as.character(features[,2])

# extract a data frame with only the mean and std. deviation variables
means <- merged_df[grep("mean", colnames(merged_df), ignore.case=TRUE)]
std <- merged_df[grep("std", colnames(merged_df), ignore.case=TRUE)]
extracted_df <- cbind(merged_df[1:2], means)
extracted_df <- cbind(extracted_df, std)

# label activity names
for (i in 1:length(extracted_df[,2])) { 
    extracted_df[i,2] <- if (extracted_df[i,2] == 1) { 
        "WALKING" 
    } 
    else if (extracted_df[i,2] == 2) { 
        "WALKING_UPSTAIRS" 
    } 
    else if (extracted_df[i,2] == 3) { 
        "WALKING_DOWNSTAIRS" 
    }
    else if (extracted_df[i,2] == 4) { 
        "SITTING" 
    }
    else if (extracted_df[i,2] == 5) { 
        "STANDING" 
    }
    else if (extracted_df[i,2] == 6) { 
        "LAYING" 
    }
    else extracted_df[i,2]
}

# generate data frame with average values for each subject and each activity
final_df <- as.data.frame(matrix(0, ncol=length(colnames(extracted_df))))
colnames(final_df) <- colnames(extracted_df)
# iterate over subjects
for (i in 1:30) {
    # iterate over activities
    for (j in 1:6) {
        current <- subset(extracted_df, extracted_df[,1] == i)
        current <- subset(current, current[,2] == activity_labels[j,2])
        # iterate over variables
        for (k in 3:length(colnames(extracted_df))) {
            current[1,k] <- mean(current[,k])
        }
        final_df <- rbind(final_df, current[1,])
    }
}
final_df <- final_df[-1,]
rownames(final_df) <- c(1:length(rownames(final_df)))