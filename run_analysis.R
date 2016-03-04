#'
library(reshape2)

#' download Zip file to the Galaxy S folder. 
ZipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#' create Galaxy S folder if not exist, save and extract the zip file
if (!dir.exists("./Galaxy S/"))  dir.create("./Galaxy S/")
if (!grepl("Galaxy S", getwd())) setwd("./Galaxy S/")
ZipFile <- "./GalaxyData.zip"
download.file(url <- ZipUrl, destfile = ZipFile)
unzip(zipfile = ZipFile)

#' Read and merge training data and test data, stored in variable completeGalaxy
#' And label the data set with descriptive variable names.
train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_label <- read.table("./UCI HAR Dataset/train/y_train.txt")
test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_label <- read.table("./UCI HAR Dataset/test/y_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
names(train_data) <- features[,2]
names(test_data) <- features[,2]
trainGalaxy <- cbind(train_label, train_data)
testGalaxy <- cbind(test_label, test_data)
completeGalaxy <- rbind(trainGalaxy, testGalaxy)
colnames(completeGalaxy)[1] <- "label"

#' Extracts only the measurements on the mean and standard deviation for each measuremen
#' and saved in variable MeanStdSet
MeanStdSet <- subset(completeGalaxy, select = grepl("[mM][eE][aA][nN]", colnames(completeGalaxy)) | grepl("[sS][tT][dD]", colnames(completeGalaxy)))  

#' Uses descriptive activity names to name the activities in the data set
ActivityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt")
completeGalaxy[,1] <- as.factor(completeGalaxy$label)
levels(completeGalaxy$label) <- as.character(ActivityLabel$V2)

#' Creates a second, independent tidy data set with the average of each variable 
#' for each activity and each subject. 
SubjectTrainLabel <- read.table("./UCI HAR Dataset/train/subject_train.txt")
SubjectTestLabel <- read.table("./UCI HAR Dataset/test/subject_test.txt")
SubjectLabel <- rbind(SubjectTrainLabel, SubjectTestLabel)
completeGalaxy <- cbind(completeGalaxy, SubjectLabel)
colnames(completeGalaxy)[563] <- "Subject"

#' calculate average for each variable for each subject after split on activities 
ActivitySubset <- split(completeGalaxy, f = completeGalaxy$label)
WalkingAve <- aggregate(ActivitySubset$WALKING[,features$V2], list(ActivitySubset$WALKING$Subject), mean)
UpstairsAve <- aggregate(ActivitySubset$WALKING_UPSTAIRS[,features$V2], list(ActivitySubset$WALKING_UPSTAIRS$Subject), mean)
DownstairsAve <- aggregate(ActivitySubset$WALKING_DOWNSTAIRS[,features$V2], list(ActivitySubset$WALKING_DOWNSTAIRS$Subject), mean)
SittingAve <- aggregate(ActivitySubset$SITTING[,features$V2], list(ActivitySubset$SITTING$Subject), mean)
StandingAve <- aggregate(ActivitySubset$STANDING[,features$V2], list(ActivitySubset$STANDING$Subject), mean)
LayingAve <- aggregate(ActivitySubset$LAYING[,features$V2], list(ActivitySubset$LAYING$Subject), mean)

#' NewDataset is a list containing the average of each variable 
#' for each activity and each subject. 
NewDataset <- list(Walking = WalkingAve, WalkingUpstairs = UpstairsAve, WalkingDownstairs = DownstairsAve, Sitting = SittingAve, Standing = StandingAve, Laying = LayingAve)
write.table(NewDataset, file = "./UCI HAR Dataset/newdata.txt", row.names = FALSE)





