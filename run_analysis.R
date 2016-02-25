library(dplyr)
library(reshape2)
# set my working directory
##### IMPORTANT!! If you want to use this script you need to 
##### CHANGE THE LINE BELOW to point to the right directory in your machine
setwd("D:\\Cursos\\Coursera\\DataScience\\Getting_and_Cleaning_Data\\week4\\proyecto\\UCI HAR Dataset")

# read activity names from activity_labels.txt
activityNames<-read.table("activity_labels.txt")

# set names for the columns: activityId for the cardinal and 
# activity for the name of the activity
names(activityNames)
names(activityNames)[1]<-"activityId"
names(activityNames)[2]<-"activity"

##########################################################
#                READ TRAIN DATA                         #
##########################################################
# Read subjects
subjectTrain <- read.table('./train/subject_train.txt')

# set names to the subject column
names(subjectTrain)[1]<-"subject"

# read features and x_train files
features<-read.table("features.txt")
xTrain <- read.table('./train/x_train.txt')

# set names to feature columns
names(features)[1]<-"featureId"
names(features)[2]<-"feature"

# set column names of the xTrain data
colnames(xTrain)<-features$feature

# read y_train file and set names
yTrain <- read.table('./train/y_train.txt')
names(yTrain)[1]<-"activityId"

##########################################################
#                READ TEST DATA                          #
##########################################################

# read subject file
subjectTest <- read.table('./test/subject_test.txt')

# read x_test and y_test files
xTest       <-read.table('./test/x_test.txt') 
yTest       <- read.table('./test/y_test.txt') 

# set columnn name for subject
names(subjectTest)[1]<-"subject"

# set column names of the x test file
colnames(xTest)<-features$feature

# set column name of the y test file
names(yTest)[1]<-"activityId"

##########################################################
#                MERGE TRAIN AND TEST DATA               #
##########################################################


# add columns corresponding to subject and yTest to xTest and store the result 
# in testData
testData<-cbind(xTest,subjectTest,yTest)

# add columns corresponding to subject and yTrain to xTrain and store the result
# in trainData
trainData<-cbind(xTrain,subjectTrain,yTrain)

# merge test and train data appending the rows
mergedData<-rbind(testData,trainData)

##########################################################
#        KEEP REQUIRED COLUMNS OF MERGED DATA            #
##########################################################

# get the index of the columns correspoding to mean and standard deviations
meanStdCols <- grep("-(mean|std)\\(\\)", colnames(mergedData))

# get the colnames corresponding to mean and standard deviation
meanStdCols<-colnames(mergedData)[meanStdCols]

# add activityId and subject to column names to select
meanStdCols<-c(meanStdCols,"activityId","subject")

# retrieve the desired columns
meanStdData<-mergedData[,meanStdCols]

# add a column with the activity name instead of the numeric id
meanStdData$activity <- factor(meanStdData$activity, levels = activityNames$activityId, labels = activityNames$activity)

# we will remove the column with the numeric activity Id since we already have
# a column with the descriptive activity name
auxLabels<-colnames(meanStdData)[colnames(meanStdData)!="activityId"]
meanStdData<-meanStdData[auxLabels]


######################################################@##############
#        MELTING AND CALCULATING MEAN  (using reshape2 package)     #
#####################################################################

# melt data by subject type and activity
meltedData <- melt(meanStdData, id = c("subject", "activity"))

# calculate mean for each 
tidyData <- dcast(meltedData, subject + activity ~ variable, mean)

# write tidy data in file 
write.table(tidyData, "tidyData.txt", row.names = FALSE, quote = FALSE)