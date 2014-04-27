This repository contains the data_analysis.R file which solves the programming assignment
=============

The data_analysis file contains extensive comments which describe the functionality of the script.

It is being splitted according to the five steps given in the project description.

1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive activity names. 
5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject

For part 3,4 the same code section applies but with two distinctive parts:

i. Setting the activity states

ii. clearing the variable names


I have avoided hardcoding in the project - hopefully this doesn't make the code too complicated.

The txt format of the output is not so readable - I hope we could have the right to upload in csv:)

The qdap library must be installed so as to run the code.
