Code Book
==========================================

This code book details the processing done to the data obtained from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
In the analysis presented here the data were downloaded on February 20th, 2016 at 22:06:52 GMT.
The script used to process the data is available in the file called run_analysis.R. The steps of the analysis are the following:

 * Merge the training and the test sets to create one data set.
 * Extract only the measurements on the mean and standard deviation for each measurement.
 * Use descriptive activity names to name the activities in the data set
 * Appropriately label the data set with descriptive variable names.
 * From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. This tidy data set can be found in the tidy.txt file.

## Original data

The original data come from a experiment that has been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity.
There are training and tests sets. For each record it is provided:

 * Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
 * Triaxial Angular velocity from the gyroscope. 
 * A 561-feature vector with time and frequency domain variables. 
 * Its activity label. 
 * An identifier of the subject who carried out the experiment.
For more information you can visit [this link](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) 

## Analysis script

The script in run_analysis.R achieves the following five goals:

 * Merges the training and the test sets to create one data set.
 * Extracts only the measurements on the mean and standard deviation for each measurement.
 * Uses descriptive activity names to name the activities in the data set
 * Appropriately labels the data set with descriptive variable names.
 * From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. This tidy data set is stored in a file called tidyData.txt.

#####The script assumes that you have downloaded the files and sets the working directory to an specific one. You need to change the line where the working directory is set so that it points to your local working directory.

## Tidy data set

The tidy data set generated by the run_analysis.R script contains the average of the selected magnitudes (those that are means or standard deviations) per activity and subject.
The first column identifies the subject, the second column identifies the activity, and the rest of the columns contain the mean of each of the variables for the corresponding subject and activity.


