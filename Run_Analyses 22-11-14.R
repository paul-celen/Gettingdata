library(reshape)
library(reshape2)


# -- A. Set Data.frames ---------------------------------------------------------

dttest <- data.frame          #  1. Raw Data  
dttestSubject <- data.frame   #  2. Raw Data
dttestAct <- data.frame       #  3. Raw Data
dtmergedtest <- data.frame    #  4. Combine Raw Data

dttrain <- data.frame         #  5. Raw Data
dttrainSbject<- data.frame    #  6. Raw Data
dttrainAct<- data.frame       #  7. Raw Data
dtmergedtrain <- data.frame   #  8. Combine Raw Data

dtmerged <- data.frame        #  9. Combine Raw Data
dtfeatures <- data.frame      # 11. Raw Data 
dtcolnames <- data.frame      # 12. tidy data, selecting variable
colnames1 <- vector           # 13. tidy data, selecting variable
colnames2 <- vector           # 14. tidy data, selecting variable
dtsplit <- data.frame         # 15. tidy data, Melted dataframe
dtAvg <- data.frame           # 16. tidy data, Dataframe with avg by subject and Activity

# -- B. Read txt files into dataframes --------------------------------------------
# Test
    dttest <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/test/X_test.txt",header = FALSE )
    dttestAct <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/test/y_test.txt",  header = FALSE , col.names = "Activity")
    dttestSubject <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/test/subject_test.txt",  header = FALSE ,  col.names = "Subject")
# Train
    dttrain <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/train/X_train.txt", header = FALSE)
    dttrainAct <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/train/y_train.txt",  header = FALSE , col.names = "Activity")
    dttrainSubject <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/train/subject_train.txt", header = FALSE  , col.names = "Subject")
# Features
    dtfeatures <- read.table("C:/Users/vobis/Desktop/UCI HAR Dataset/features.txt", header = FALSE)

# -- 1 Merge -------------------------------------------------------------

    dttestmerged <- cbind(cbind(dttest, dttestAct), dttestSubject)     ## add Activity and then Add Subject to test dataframe (Column). 
    dttrainmerged <- cbind(cbind(dttrain, dttrainAct), dttrainSubject) ## add Activity and then Add Subject to train dataframe(Column).
    dtmerged <- rbind(dttestmerged, dttrainmerged)                     ## Merge the dataframe together

# -- 2 Extract the mean and std variable ----------------------------------

    dtcolnames <- grep("-mean()|-std()", dtfeatures$V2) # Select colomn with mean and std,index number. With(-) exclude Freqmean, is a different measurement than mean
    colnames1 <- (dtcolnames)                           # Convert to a vector
    dtmerged <- dtmerged[,c(colnames1,562,563)]         # Subset, selecting colomn based index number. 


# -- 3 add activiteis names -----------------------------------------------

    dtmerged$Activity[dtmerged$Activity == 1] <- "Walking"
    dtmerged$Activity[dtmerged$Activity == 2] <- "Walk_Upstrairs"
    dtmerged$Activity[dtmerged$Activity == 3] <- "Walk_Downstairs"
    dtmerged$Activity[dtmerged$Activity == 4] <- "Sitting"
    dtmerged$Activity[dtmerged$Activity == 5] <- "Standing"
    dtmerged$Activity[dtmerged$Activity == 6] <- "Laying"

# -- 4 Add variable names -------------------------------------------------

    colnames2 <- as.character(dtfeatures$V2[colnames1])  ## convert to a character vector
    colnames2 <- c(colnames2,"Activity","Subject")       ## Add two elements voor Subject and Activity
    colnames(dtmerged) <- colnames2                      ## Rename variabele merged data.frame

# -- 5 new tidy data, average ---------------------------------------------

    dtsplit <- melt(dtmerged, id.vars=c("Activity","Subject"))                                    # add id vars and reshape dataframe.
    dtAvg <-dcast(dtsplit, Activity + Subject ~ variable, fun.aggregate = mean, na.rm = TRUE)     # summarize dataframe by mean for activity and subject
    head(dtAvg[,c(1,2,3,4,5,6,7,8)],180)                                                          # show first 8 variable new dataframe for all rows.
    write.table(dtAvg, row.names=FALSE , file="C:/Users/vobis/Desktop/Coursera/3. Getting Data/Run_Analyses.txt")

    summary(dtAvg[,c(1,2,3,4,5,6,7,8)],180)                                                       # show first 8 variable new dataframe for all rows.
