#The purpose of this project is to demonstrate your ability to collect, 
#work with, and clean a data set. The goal is to prepare tidy data that 
#can be used for later analysis. You will be graded by your peers on a 
#series of yes/no questions related to the project. 

#You will be required to submit:

# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the 
#    analysis, and 
# 3) a code book that describes the variables, the data, and any 
#    transformations or work that you performed to clean up the data 
#    called CodeBook.md. You should also include a README.md in the repo 
#    with your scripts. This repo explains how all of the scripts work 
#    and how they are connected.  

#One of the most exciting areas in all of data science right now is 
#wearable computing - see for example this article . Companies like Fitbit, 
#Nike, and Jawbone Up are racing to develop the most advanced algorithms 
#to attract new users. The data linked to from the course website represent 
#data collected from the accelerometers from the Samsung Galaxy S smartphone. 
#A full description is available at the site where the data was obtained:

#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#Here are the data for the project: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#You should create one R script called run_analysis.R that does the following: 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation 
#    for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive activity names. 
# 5) Creates a second, independent tidy data set with the average of 
#    each variable for each activity and each subject. 

#-------------------------- Assignment Starts Here ----------------------------#

#Step 1: Load an merge data (points 1, 3 and 4)
#1a: load data
library(reshape2)
datadir  <- paste(getwd(), "UCI HAR Dataset", sep="/")
testdir  <- paste(datadir, "test", sep="/")
traindir <- paste(datadir, "train", sep="/")

subject_test <- read.table(paste(testdir, "subject_test.txt", sep="/"), quote="\"")
X_test <- read.table(paste(testdir, "X_test.txt", sep="/"), quote="\"")
y_test <- read.table(paste(testdir, "y_test.txt", sep="/"), quote="\"")
subject_train <- read.table(paste(traindir, "subject_train.txt", sep="/"), quote="\"")
X_train <- read.table(paste(traindir, "X_train.txt", sep="/"), quote="\"")
y_train <- read.table(paste(traindir, "y_train.txt", sep="/"), quote="\"")

features <- read.table(paste(datadir, "features.txt", sep="/"), quote="\"")
activity_labels <- read.table(paste(datadir, "activity_labels.txt", quote="\"")

#1b: merge datasets by rows
DataSubject <- rbind(subject_test, subject_train)
colnames(DataSubject) <- "SubjectNumber"

DataLabel <- rbind(y_test, y_train)
colnames(DataLabel) <- "Label"
DataLabelActual <- merge(DataLabel, activity_labels, by=1) #Step 3, 4 in assignment
DataLabelActual <- DataLabelActual[,-1]

DataSet <- rbind(X_test, X_train)
colnames(DataSet) <- features[,2]


#1c: merge datasets into one big data set
DataTotal <- cbind(DataSubject, DataLabelActual, DataSet)


#Step 2: Extracts only the measurements on the mean and standard deviation 
#        for each measurement. (point 2)

toMatch <- c("mean\\(\\)", "std\\(\\)") #Matches mean and std
matches <- grep(paste(toMatch,collapse="|"), features[,2], value=FALSE)
matches <- matches+2 #To compensate for the 2 new extra rows in the beginning

#Create new dataset that only includes description labels, subject and mean + std
DataMeanStd <- DataTotal[,c(1,2,matches)]

#Prints the data table to a file
write.table(DataMeanStd, "DataMeanStd.txt" , sep = ";")

#Step 3: Creates a second, independent tidy data set with the average of each 
#        variable for each activity and each subject. (point 5)

# Creates a "skinny data set" on the basis of subject and activity
# Note that there are no measure variables as to collapse dublicate
# variables in the "features" data set
MeltData = melt(DataTotal, id.var = c("SubjectNumber", "DataLabelActual"))

AveLabAct = dcast(MeltData, SubjectNumber + DataLabelActual ~ variable,mean)
#Please note that some the meassure variables are collapsed as the 
#"features" dataset has some dublicate values, i.e. 561-479=82.

#Write the table to the forlder
write.table(AveLabAct, "AveWithLabel.txt" , sep = ";")