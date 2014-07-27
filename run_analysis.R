# Getting and Cleaning Data - Coursera Course offered by Johns Hopkins
# Course Project
# July 27, 2014
# A. Stevens

# set directory to data file locations
setwd("C:/Users/astevens/Documents/Documents/Business Analytics Team/JH - Data Scientist/Getting and Cleaning Data/Project/data")

# read in data
# no headers

# read in train data
 
# List of activities ie. walking, sitting, etc.
activityLabels = read.table('./activity_labels.txt',col.names=c('activityId','activityType'),header=FALSE)

# List of all features ie accelerometer and gyroscope 3-axial raw signals
features = read.table('./features.txt',col.names=c("feature_id","feature_label"), header=FALSE)
# 
subject_train = read.table('./subject_train.txt',col.names=c("subjectId"),header=FALSE)


# test set
# use the features listed in the features table column 2 as the column names
x_train = read.table('./x_train.txt',col.names=c(features[,2]),header=FALSE)

# test labels
y_train = read.table('./y_train.txt',col.names=c("activityId"),header=FALSE)

# add in row ID
subject_train$ID <- as.numeric(rownames(subject_train))
x_train$ID <- as.numeric(rownames(x_train))
y_train$ID <- as.numeric(rownames(y_train))

# read in test data
subject_test = read.table('./subject_test.txt',col.names=c("subjectId"),header=FALSE)
# train set
x_test = read.table('./x_test.txt',col.names=c(features[,2]),header=FALSE)
# train labels
y_test = read.table('./y_test.txt',col.names=c("activityId"),header=FALSE)

# add in row ids
subject_test$ID <- as.numeric(rownames(subject_test))
x_test$ID <- as.numeric(rownames(x_test))
y_test$ID <- as.numeric(rownames(y_test))

# create training set of all files
#trainingData <- cbind(y_train, subject_train, x_train)
trainingData1 <- merge(y_train, subject_train, all=TRUE)
trainingData2 <- merge(trainingData1, x_train, all=TRUE)

#create test set of all files
#testData <- cbind(y_test, subject_test, x_test)
testData1 <- merge(y_test, subject_test, all=TRUE)
testData2 <- merge(trainingData1, x_test, all=TRUE)

# combine training data with test data into one large dataset. 
# This satisfies requirement #1
#create one data set from all of the training and test data
MergedDataSet <- rbind(trainingData2, testData2)

#write.table(SingleDataSet, file='./MergedDataSet.txt',append=FALSE, row.names=TRUE,sep='\t')
# write.csv(file='./MergedDataSetwID.csv', MergedDataSet)

# 2
# work on extracting the measurements for the mean and std deviation
# use grepl to return a logical vector

onlymeanstd <- features[grepl("mean\\(\\)",features$feature_label)|grepl("std\\(\\)",features$feature_label),]
meanstddata <- MergedDataSet[,c(c(1,2,3), onlymeanstd$feature_id + 3)]

# 3
# add in descriptive labels
activitylabelslist <- merge(meanstddata, activityLabels,by='activityId', all.x=TRUE)
activitylabeldata <- merge(meanstddata, activityLabels)

# 4
# add descriptive activity names
# remove () and - from names to clean up labels
onlymeanstd$feature_label = gsub("\\(\\)", "",onlymeanstd$feature_label)
onlymeanstd$feature_label = gsub("-",".",onlymeanstd$feature_label)
onlymeanstd$feature_label = gsub('BodyBody','Body',onlymeanstd$feature_label)

for (i in 1:length(onlymeanstd$feature_label)){
  colnames(activitylabeldata)[i+3] <- onlymeanstd$feature_label[i]
}

activitylabeldata2 = activitylabeldata

#write.csv(file="test.csv", finaldata4)

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = activitylabeldata[,names(activitylabeldata) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityLabels,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
#write.csv(file="tidyData.csv", tidyData)



