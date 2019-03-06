#run_analysis.R does the following:
# downloads the file from cla
# unzips the file
# reads the training and test data sets
# merges the training and the test sets to create one data set.
# extracts only the measurements on the mean and standard deviation for each measurement.
# uses descriptive activity names to name the activities in the data set
# appropriately labels the data set with descriptive variable names.
# from the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


run_analysis.R <- {
        # Downloading and unzipping dataset
        
        if(!file.exists("./data")){dir.create("./data")}
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,destfile="./data/Dataset.zip")
        unzip(zipfile="./data/Dataset.zip",exdir="./data")

        
        # Reading files
        # Trainings tables:
        x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
        y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
        subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
        # Testing tables:
        x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
        y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
        subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
        # Feature vector:
        features <- read.table('./data/UCI HAR Dataset/features.txt')
        # Activity labels:
        activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
        
        
        # Define column names
        colnames(x_train) <- features[,2]
        colnames(y_train) <-"activityId"
        colnames(subject_train) <- "subjectId"
        
        colnames(x_test) <- features[,2]
        colnames(y_test) <- "activityId"
        colnames(subject_test) <- "subjectId"
        
        colnames(activityLabels) <- c('activityId','activityType')
        
        
        # Merge Data
        merge_train <- cbind(y_train, subject_train, x_train)
        merge_test <- cbind(y_test, subject_test, x_test)
        comb_file <- rbind(merge_train, merge_test)
        
        # Extract Mean and Standard deviations for each measurement
        colNames <- colnames(comb_file)
        mean_and_std <- (grepl("activityId" , colNames) | 
                                 grepl("subjectId" , colNames) | 
                                 grepl("mean\\(\\)" , colNames) | 
                                 grepl("std.." , colNames) )
        
        comb_file_av_std <- comb_file[ , mean_and_std == TRUE]
        
        
        
        # Dataset with acticity names
        dataset_with_names <- merge(comb_file_av_std, activityLabels,
                                      by='activityId',
                                      all.x=TRUE)
        # Create tidy dataset
        
        TidySet_av_std <- aggregate(. ~subjectId + activityType, dataset_with_names, mean)
        TidySet_av_std <- TidySet_av_std[order(TidySet_av_std$subjectId, TidySet_av_std$activityId),]
        write.table(TidySet_av_std, "TidySet_av_std.txt", row.name=FALSE)
}

