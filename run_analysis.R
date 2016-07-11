#-------------------------------
# run_analysis.R
#-------------------------------
# Revision Log:
#   07/08/2016 cll Original
#-------------------------------

# clear workspace
rm(list = ls())
 
# load required libraries
library(lubridate)

# download zip file

zipurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile<-"projectinputdata.zip"

if (file.exists(destfile)) {
     file.remove(destfile)
}

download.file(zipurl, destfile=destfile)
downloaddatetime<-now()

unzip(destfile)

# load features  
features<-read.table("./UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)
colnames(features) <- c("feature_id","feature")

# load activity labels
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE)
colnames(activity_labels) <- c("activity_id","activity_label")

# load subject ids for training set
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",stringsAsFactors=FALSE)
names(subject_train)[names(subject_train)=="V1"] <- "subject_id"
subject_train$train_row_id <- 1:nrow(subject_train)
#subject_train<-subject_train[c("train_row_id","subject_id")]

# load subject ids for test set
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",stringsAsFactors=FALSE)
names(subject_test)[names(subject_test)=="V1"] <- "subject_id"
subject_test$test_row_id <- 1:nrow(subject_test)
#subject_test<-subject_test[c("test_row_id","subject_id")]

# load activity ids for training set
activity_train<-read.table("./UCI HAR Dataset/train/y_train.txt",stringsAsFactors=FALSE)
names(activity_train)[names(activity_train)=="V1"] <- "activity_id"
activity_train$train_row_id <- 1:nrow(activity_train)
activity_train_with_activity_labels<-merge(activity_train, activity_labels)

# load activity ids for test set
activity_test<-read.table("./UCI HAR Dataset/test/y_test.txt",stringsAsFactors=FALSE)
names(activity_test)[names(activity_test)=="V1"] <- "activity_id"
activity_test$test_row_id <- 1:nrow(activity_test)
activity_test_with_activity_labels<-merge(activity_test, activity_labels)

# identify mean/std deviation column indexes
mean_std_col_indexes<-grep("mean|std",features[,2])

# load x train, limit to mean/std deviation features, add subject ids, add activity ids
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt",stringsAsFactors=FALSE)
names(x_train)<-features[,2]
x_train_mean_std<-x_train[,mean_std_col_indexes]
x_train_mean_std$train_row_id <- 1:nrow(x_train_mean_std)
x_train_mean_std$set_type <- "train"
x_train_mean_std_with_subjects <- merge(subject_train,x_train_mean_std,by="train_row_id")
x_train_mean_std_with_subjects_and_activities <- merge(activity_train_with_activity_labels,x_train_mean_std_with_subjects,by="train_row_id")

# load x test, limit to mean/std deviation features, add subject ids, add activity ids
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt",stringsAsFactors=FALSE)
names(x_test)<-features[,2]
x_test_mean_std<-x_test[,mean_std_col_indexes]
x_test_mean_std$test_row_id <- 1:nrow(x_test_mean_std)
x_test_mean_std$set_type <- "test"
x_test_mean_std_with_subjects <- merge(subject_test,x_test_mean_std,by="test_row_id")
x_test_mean_std_with_subjects_and_activities <- merge(activity_test_with_activity_labels,x_test_mean_std_with_subjects,by="test_row_id")

# merge training and test sets
names(x_train_mean_std_with_subjects_and_activities)[names(x_train_mean_std_with_subjects_and_activities)=="train_row_id"] <- "row_id"
names(x_test_mean_std_with_subjects_and_activities)[names(x_test_mean_std_with_subjects_and_activities)=="test_row_id"] <- "row_id"
combined_training_and_test <- rbind(x_train_mean_std_with_subjects_and_activities, x_test_mean_std_with_subjects_and_activities)

# aggregate by activity and subject
average_by_activity_and_subject<-aggregate(combined_training_and_test[,5:83]
  , list( combined_training_and_test$activity_id
        , combined_training_and_test$activity_label
        , combined_training_and_test$subject_id)
  , mean)
names(average_by_activity_and_subject)[names(average_by_activity_and_subject)=="Group.1"] <- "activity_id"
names(average_by_activity_and_subject)[names(average_by_activity_and_subject)=="Group.2"] <- "activity_label"
names(average_by_activity_and_subject)[names(average_by_activity_and_subject)=="Group.3"] <- "subject_id"

# write output files
write.csv(combined_training_and_test, file = "combined_training_and_test.csv")
write.csv(average_by_activity_and_subject, file = "average_by_activity_and_subject.csv")



