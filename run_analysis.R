#run_analysis.R
library(dplyr)
library(data.table)

#file url and download and unzip
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("./phoneAccelData.zip")) {
        mydir<- paste0(getwd(),"/","phoneAccelData.zip")
        download.file(fileUrl, destfile = mydir)
        unzip("./phoneAccelData.zip")
}
#reading files
features<- read.table("./UCI HAR Dataset/features.txt")
activity_labels<- read.table("./UCI HAR Dataset/activity_labels.txt")

#test files
person_test<- read.table("./UCI HAR Dataset/test/subject_test.txt")
data_test<- read.table("./UCI HAR Dataset/test/x_test.txt")
labels_test<- read.table("./UCI HAR Dataset/test/y_test.txt")

#train files
person_train<- read.table("./UCI HAR Dataset/train/subject_train.txt")
data_train<- read.table("./UCI HAR Dataset/train/x_train.txt")
labels_train<- read.table("./UCI HAR Dataset/train/y_train.txt")

#change the names of the variables in the test and train data
#to match combine them later?
names(data_test)<-features$V2
names(data_train)<-features$V2

#change the names of the variables in the person test and train 
#to match combine them later? rather than use V1 to avoid confusion later
names(person_test)<- "person_code"
names(person_train)<- "person_code"

#ok now changing the colomn name that will tell us what activity was done
#activity labels will probably have to change value to activity name later
names(labels_test)<- "activity_code"
names(labels_train)<- "activity_code"

#ok now combine to test columns then train colomns... labels first :)
test_df<- cbind(labels_test, person_test, data_test)
train_df<- cbind(labels_train, person_train, data_train)

#now just combine rows which should match up perfectly...
total_df<- rbind(test_df,train_df)

#force valid names for the columns so we can use dplyr to "select" mean & std
valid_names<- make.names(names(total_df), unique = TRUE, allow_ = TRUE)
names(total_df)<-valid_names

#grab columns with a measure of mean and or std... 
#lets keep person and activity code...don't forget about order
mean_std_df<-select(total_df, matches("activity_code|person_code|mean|std"))

#use activity labels to label activity code...
#basically, change the value of activity code based on it's own value... 
act_label_df<- mean_std_df%>%
        arrange(activity_code)%>%
        mutate(activity_code = as.character(factor(activity_code, levels=1:6, labels= activity_labels$V2)))

#chang the variable names to something more descriptive...
#this is ridiculous considering the number of columns ugggg...
#copy paste is my friend
names(act_label_df)<-gsub("activity_code", "activity",names(act_label_df))
names(act_label_df)<-gsub("tBodyAcc", "Body Acceleration in Time",names(act_label_df))
names(act_label_df)<-gsub("tGravityAcc", "Gravity Acceleration Time",names(act_label_df))
names(act_label_df)<-gsub("Jerk", " with angular velocity ",names(act_label_df))
names(act_label_df)<-gsub("tBodyGyro", "Body Gyroscopic Angle in Time",names(act_label_df))
names(act_label_df)<-gsub("Mag", " Euclidean norm Magnitude",names(act_label_df))
names(act_label_df)<-gsub("fBodyAcc", "Fast Fourier Transform Body Acceleration",names(act_label_df))
names(act_label_df)<-gsub("fBodyGyro", "Fast Fourier Transform Body Gyroscopic Angle",names(act_label_df))
names(act_label_df)<-gsub("fBodyBodyAcc", "Fast Fourier Transform Body Acceleration",names(act_label_df))
names(act_label_df)<-gsub("fBodyBodyGyro", "Fast Fourier Transform Body Gyroscopic Angle",names(act_label_df))

#tidy things up with group_by and summarise
#seemingly can only get this to work using the pipline operator %>%
tidy_df<- act_label_df %>% group_by(person_code, activity) %>% summarise_all(mean)

#write a table in the working derectory of the tidy data table
#not sure where i saw to do this
write.table(tidy_df, file = "tidy_df.txt", row.names = FALSE)
