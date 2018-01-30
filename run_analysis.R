    #####################################################################################################################################
    # FILENAME: run_analysis.R
    #
    #
    # OVERVIEW:
    #
    #   Using data collected from the accelerometers from the Samsung Galaxy S 
    #   smartphone, work with the data and make a clean data set, outputting the
    #   resulting tidy data to a file named "tidy_data.txt".
    #   See README.md for details.
    #
    #
    # LOAD APPROPIATE LIBRARIES:
    #
      library (dplyr)
      install.packages('reshape2')
      library(reshape2)
    
    #####################################################################################################################################
    # DOWNLOAD DATA AND UNZIP
    #####################################################################################################################################
    
    # download file if it does not already exist  
       if(!file.exists("./data")){
         dir.create("./data")
       }
    
       fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
       download.file(fileUrl,destfile="./data/dataset.zip")
    
    # unzip file in data directory   
       unzip(zipfile="./data/dataset.zip",exdir="./data")
     
    # list of files in unzipped folder
       path <- file.path("./data", "UCI HAR Dataset")
       files <- list.files(path, recursive = TRUE)
    
    ######################################################################################################################################
    # READ THE DATA  
    ######################################################################################################################################
    
    # read training data
       trainingSubjects <- read.table(file.path(path, "train", "subject_train.txt"), header = FALSE)
       trainingFeatures <- read.table(file.path(path, "train", "X_train.txt"), header = FALSE)
       trainingActivity <- read.table(file.path(path, "train", "y_train.txt"), header = FALSE)
    
    # read test data
       testSubjects <- read.table(file.path(path, "test", "subject_test.txt"), header = FALSE)
       testFeatures <- read.table(file.path(path, "test", "X_test.txt"), header = FALSE)
       testActivity <- read.table(file.path(path, "test", "y_test.txt"), header = FALSE)
      
    
       
    #######################################################################################################################################
    # 1. Merges the training and the test sets to create one data set.
    #######################################################################################################################################
    
   
    # assign column names to "data_table"
       
       names(trainingSubjects)<- "subject"
       names(testSubjects)<- "subject"
       
       names(trainingActivity)<- "activity"
       names(testActivity)<- "activity"
       
       featuresNames <- read.table(file.path(path, "features.txt"))
       names(trainingFeatures)<- featuresNames$V2
       names(testFeatures)<- featuresNames$V2
       
    # concatenate by rows
    
       subjectData <- rbind(trainingSubjects, testSubjects)
       featuresData<- rbind(trainingFeatures, testFeatures)
       activityData<- rbind(trainingActivity, testActivity)
       
    # Merge to one data table
       combine <- cbind(subjectData, activityData)
       data <- cbind(featuresData, combine)
       
       
    #######################################################################################################################################
    # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    #######################################################################################################################################
    
    # determine columns with mean and std
       
       #means_std_cols <- grepl("mean\\(\\)", names(data)) | grepl("std\\(\\)", names(data)) 
       means_std_cols <- grepl("subject|activity|mean\\(\\)|std\\(\\)|", names(data))
       
    # create data table with relevant data
       data <- data[, means_std_cols]
       
    #######################################################################################################################################
    # 3. Uses descriptive activity names to name the activities in the data set
    #######################################################################################################################################
       
    # Read descriptive activity names from "activity_labels.txt"
       
      data$activity <- factor(data$activity, labels=c("WALKING", "WALKING UPSTAIRS", "WALKING DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
       
       
    #######################################################################################################################################
    # 4. Appropriately labels the data set with descriptive variable names.
    #######################################################################################################################################
    
    # column names of data table
       data_table_cols <- names(data)
       
       data_table_cols <- gsub("^f", "Frequency", data_table_cols)
       data_table_cols <- gsub("^t", "Time", data_table_cols)
       data_table_cols <- gsub("Acc", "Accelerometer", data_table_cols)
       data_table_cols <- gsub("Gyro", "Gyroscope", data_table_cols)
       data_table_cols <- gsub("Mag", "Magnitude", data_table_cols)
       data_table_cols <- gsub("BodyBody", "Body", data_table_cols)
       data_table_cols <- gsub("mean()", "Mean", data_table_cols)
       data_table_cols <- gsub("std()", "Std", data_table_cols)
    
    #######################################################################################################################################
    # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
    # variable for each activity and each subject.
    #######################################################################################################################################
    
    # output table to file
       meltData <- melt(data, id=c("subject","activity"))
       tidyData <- dcast(meltData, subject + activity ~ variable, mean)
       write.csv(tidyData, "tidyData.csv", row.names=FALSE)
       