############################### Code Used to Download the Data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/20Dataset.zip",mode="wb")

############################### Code Used to Read the Data and Load All Columns
library("data.table")

# Read the 561-feature vectors with time and frequency domain variables.
featureVectorTrain <- fread("./data/UCI HAR Dataset/train/X_train.txt")
featureVectorTest <- fread("./data/UCI HAR Dataset/test/X_test.txt")

# Read the activity labels. 
activityLabelsTrain <- fread("./data/UCI HAR Dataset/train/y_train.txt") 
activityLabelsTest <- fread("./data/UCI HAR Dataset/test/y_test.txt")

# Read the subject identifiers.
subjectIdentifiersTrain <- fread("./data/UCI HAR Dataset/train/subject_train.txt") 
subjectIdentifiersTest <- fread("./data/UCI HAR Dataset/test/subject_test.txt")

#Read the list of features (catalog, variable names)
featureList <- fread("./data/UCI HAR Dataset/features.txt")

#Read the Catalog of Activities
catalogActivities <- fread("./data/UCI HAR Dataset/activity_labels.txt")

############################### Code Used to Merge the Data by Columns and Then, Rows
# Merge The Feature Vector with the Acitivy Labels and the Subject(Columns)
trainData <- cbind(featureVectorTrain, activityLabelsTrain)
names(subjectIdentifiersTrain) <- "s"
trainData <- cbind(trainData, subjectIdentifiersTrain)
testData <- cbind(featureVectorTest, activityLabelsTest)
names(subjectIdentifiersTest) <- "s"
testData <- cbind(testData, subjectIdentifiersTest)

# Append the Rows of the Train and Test Data.
data <- rbind(trainData, testData)

# Now data contains all the required data in total.

############################### Code Used to Appropriately Name the Variables of the Dataset data
featNames <- as.list(featureList[,2])
allNames <- append (featNames[[1]], c("activity", "subject"))

############################### Code Used to Write the Data to a File with Appropiate Variable Names
write.table(data, "./data/alldata.txt", row.names = FALSE, col.names = allNames)

############################### Code Used to Extract Only the Measurements on the Mean and Standard Deviation
# Read only the columns that contain mean or standard deviations, activity and subjects
logicalVectorToRead <- sapply(allNames, function(x) grepl("mean", x)) | sapply(allNames, function(x) grepl("std", x)) | sapply(allNames, function(x) grepl("activity", x)) | sapply(allNames, function(x) grepl("subject", x))
selectedColumns <- allNames[logicalVectorToRead]
filteredData <- fread("./data/alldata.txt", select = selectedColumns)

############################### Code Used to Substitute numbers to Activity Names
actNumbers <- as.list(filteredData[,"activity"])
actNumbers <- actNumbers[[1]]
actNames <- as.list(catalogActivities[,2])
actNames <- actNames[[1]]
actLevels <- as.list(catalogActivities[,1])
actLevels <- actLevels[[1]]
newActivities <- factor(actNumbers, levels = actLevels, labels = actNames)
newActivities <- as.character(newActivities)
filteredData$activity <- NULL
filteredData <- cbind(filteredData, newActivities)
names(filteredData)[names(filteredData) == "newActivities"] <- "activity"

############################### Appropriately labels the data set with descriptive variable names. 
newVariableNames <- names(filteredData)
newVariableNames <- sapply(newVariableNames, function(x) gsub("^t", "time", x))
newVariableNames <- unname(newVariableNames)
newVariableNames <- sapply(newVariableNames, function(x) gsub("^f", "frequency", x))
newVariableNames <- unname(newVariableNames)
newVariableNames <- sapply(newVariableNames, function(x) gsub("Acc", "Acceleration", x))
newVariableNames <- unname(newVariableNames)
newVariableNames <- sapply(newVariableNames, function(x) gsub("Gyro", "Gyroscope", x))
newVariableNames <- unname(newVariableNames)
newVariableNames <- sapply(newVariableNames, function(x) gsub("-", "_", x))
newVariableNames <- unname(newVariableNames)
newVariableNames <- sapply(newVariableNames, function(x) gsub("[()]", "", x))
newVariableNames <- unname(newVariableNames)
setnames(filteredData, old = names(filteredData), new = newVariableNames)

############################### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectLevels <- unique(filteredData$subject)
subjectLevels <- sort(subjectLevels)
library(sqldf)
tidyData <- data.frame(a=1)
tidyData <- cbind(tidyData, filteredData[1])
tidyData$a <- NULL
tidyData <- tidyData[-c(1), ]
tidyDataVariableNames <- names(tidyData)
tidyDataVariableNames <- sapply(tidyDataVariableNames, function(x) gsub("time", "Average_time", x))
tidyDataVariableNames <- unname(tidyDataVariableNames)
tidyDataVariableNames <- sapply(tidyDataVariableNames, function(x) gsub("frequency", "Average_frequency", x))
tidyDataVariableNames <- unname(tidyDataVariableNames)
setnames(tidyData, old = names(tidyData), new = tidyDataVariableNames)

for (subj in subjectLevels)
{
  for (act in actNames)
  {
    templist <- as.list(c())
    for(i in 1:(ncol(filteredData)-2))
    {
      currentVariable <- names(filteredData)[i]
      avgCurrentVatiable <- fn$sqldf("SELECT AVG( $currentVariable ) 
	  		          			FROM filteredData 
                                  			WHERE activity = '$act' AND subject = '$subj' 
                                		      ")
      templist[[i]] <- avgCurrentVatiable[[1]]  
      #print(paste( i, currentVariable, subj, act, avgCurrentVatiable))
    }
    templist[[80]] <- subj
    templist[[81]] <- act
    templist <- unlist(templist)
    tidyData <- rbind(tidyData, templist)
    #print(templist)
    #print(str(templist))
    #print(tidyData)
    #print(ncol(tidyData))
  }
}

setnames(tidyData, old = names(tidyData), new = tidyDataVariableNames)

write.table(tidyData, "./data/tidydata.txt", row.names = FALSE, col.names = names(tidyData), quote = FALSE)