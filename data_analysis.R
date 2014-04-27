data_analysis <- function() {
  ## 'function which returns the tidy data set required in the assignment
  

## sets the working directory to the folder containing the data
setwd('C:\\Users\\tasos\\Desktop\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset')

## loading a package used for a vectorized gsub operation (mgsub)
library(qdap)

#################################################################################
######### 1.Merges the training and the test sets to create one data set. #######
#################################################################################



## reading the features data
features_data = read.table('features.txt')

column_names = as.character(features_data[,2])

## reading the test data
subject_test = read.table('test\\subject_test.txt')
x_test       = read.table('test\\X_test.txt')
y_test       = read.table('test\\y_test.txt')



## merging them
test_data = cbind(subject_test, y_test, x_test);

## reading the train data
subject_train = read.table('train\\subject_train.txt');
x_train       = read.table('train\\X_train.txt');
y_train       = read.table('train\\y_train.txt');

## merging them
train_data = cbind(subject_train, y_train, x_train);

## merging the train and test data
data_set = rbind(train_data, test_data);


## entering
names(data_set)[3:563] = column_names;
names(data_set)[1:2]   = c('Subject','State');

##################################################################################################
### 2. Extracts only the measurements on the mean and standard deviation for each measurement. ###
##################################################################################################


## using a regular expression to extract the columns containing either the '-mean()
## or the '-std()' pattern
std_or_mean_indices = grep( "-mean\\(\\)|-std\\(\\)", column_names);

## using the indices from the above regexp - taking into account that the first
## two columns will hold the 'subject' and 'state' data.
mean_or_std_data = data_set[,c(1,2,std_or_mean_indices+2)];

#################################################################################
### 3. Uses descriptive activity names to name the activities in the data set ###
### 4. Appropriately labels the data set with descriptive activity names.     ###
#################################################################################

## storing the states of the subjects and their respective indices
states=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
states_indices=1:6;

## getting the index of the 'State' column
state_col_ind = which(names(mean_or_std_data)=='State')

## replacing the indices with the states
mean_or_std_data[,state_col_ind] = mgsub(states_indices, states, mean_or_std_data[,state_col_ind])



#########################################
#### modifying the variable names #######
#########################################

## getting the column names
column_names = names(mean_or_std_data);

## getting the number of columns
num_of_columns = length(column_names);

t_starting=regexpr("t+", column_names, perl=TRUE)==1
f_starting=regexpr("f+", column_names, perl=TRUE)==1

column_names[3:length(column_names)] = substr(column_names[3:length(column_names)],2,nchar(column_names)[3:num_of_columns])

column_names[t_starting] = mapply(function(x, y, collapse) paste(x, y, collapse), x='Time Domain', y=column_names[t_starting], collapse=' ',USE.NAMES=FALSE)
column_names[f_starting] = mapply(function(x, y, collapse) paste(x, y, collapse), x='Frequency Domain', y=column_names[f_starting], collapse=' ',USE.NAMES=FALSE)


to_be_replaced = c('Body','Acc','Gyro','Mag','Jerk','Gravity','-')
new_values     = c('Body ','Acceleration ','Gyroscope ','Magnitude ','Jerk ', 'Gravity ',' ')

column_names = mgsub(to_be_replaced, new_values, column_names)

## writing back the column names
names(mean_or_std_data) = column_names;

#################################################################################
### Creates a second, independent tidy data set with the average of each      ###
### variable for each activity and each subject.                              ###
#################################################################################

number_of_subjects = 30;

## getting the number of states
len_states = length(states);

## initializing the tidy data set
tidy_data <-  data.frame(matrix(ncol = num_of_columns, nrow = number_of_subjects*len_states))

## getting the index of the 'Subject' column
subject_col_ind = which(names(mean_or_std_data)=='Subject')

## going through all the subjects
for (i in 1:number_of_subjects)
{
  ## getting the index of each own and their data
  subject_ind = mean_or_std_data[,subject_col_ind]==i;
  temp_data   = mean_or_std_data[subject_ind,];
  
  ## going through all states
  for (j in 1:len_states)
  {
    ## getting the indices for each state
    state_ind = temp_data[,state_col_ind]==states[j];
    
    ## writing the data
    
    ## writing the subject
    tidy_data[(i-1)*len_states + j, 1 ] = i;
    
    ## the state
    tidy_data[(i-1)*len_states + j, 2 ] = states[j]; 
    
    ## and computing and writing the mean
    tidy_data[(i-1)*len_states + j, 3:num_of_columns ] = mapply(mean,temp_data[state_ind,3:num_of_columns]);
  }
  
}

## writing the column names
names(tidy_data) = column_names;

## return the tidy data
return(tidy_data);

}
