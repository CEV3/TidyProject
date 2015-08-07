Introduction
============

This document explains the code used for this project and the process used to explore the original data and transform it into the tidy dataset requested.

The main steps used to tidy the data included renaming variables (column headers) to better conform with R and general data standards. Some variable names included the repeated word Body, which were corrected. Two pairs of columns had identical data but different column names. The correct column names could not be determined and the issue of which column names were correct for duplicate columns was unresolved. These issues are documented below and noted in the accompanying code book. Finally, the means of each variable were calculated based on 30 subsets of subjects (subject01 through subject30) and 6 subsets of activities (laying, sitting, standing, walking, walking upstairs and walking downstairs). The tidy data was named tidy.txt. For more information about the data structure and explanations, please read CODEBOOK.md.

### Steps Used in Analysis

The first few lines of code loaded the packages needed and downloaded the data files required for analysis. The package data.table was used to summarize the data by mean for the final tidy.txt dataset.

``` r
# Program run_analysis.R

# library("curl") # used with download.file occasionally
library("data.table")

# Download commented out per instructions of code working as long as Samsung data is in working directory
# Download data and prepare for analysis
# url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(url1, destfile = "./UCIHAR.zip", method="libcurl")
# unzip("./UCIHAR.zip", exdir = ".") # unzips to folder "UCI HAR Dataset"
```

The contents of the UCI HAR Dataset directory were listed to see what they contained. This was followed by viewing a few of the .txt files outside of R.

``` r
list.files("./UCI HAR Dataset") # List contents of UCIHAR directory
```

    ## [1] "activity_labels.txt" "features.txt"        "features_info.txt"  
    ## [4] "README.txt"          "test"                "train"

``` r
list.files("./UCI HAR Dataset/test") # List contents of test subdirectory
```

    ## [1] "Inertial Signals" "subject_test.txt" "X_test.txt"      
    ## [4] "y_test.txt"

``` r
list.files("./UCI HAR Dataset/train") # List contents of train subdirectory
```

    ## [1] "Inertial Signals"  "subject_train.txt" "X_train.txt"      
    ## [4] "y_train.txt"

The key files used for the project were loaded using read.table. This was followed by exploratory analysis of the dimensions and stricture of the data. Some of the steps were commented out due to space considerations, although the results are included in the comments.

``` r
# Read in key data .txt files:
test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE) # data for test subjects
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE) # subject ID column
testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE) # labels 1-6 indicating walking, etc.
train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE) # data for training subjects
trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE) # subject ID column
trainLabels <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE) # labels 1-6 indicating walking, etc.
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt") # labels with text descriptions of activity such as "WALKING"
mainHeaders <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE) # variables names to be used to create column headers
```

Exploratory analysis included the following steps, which are commented out with notes.

``` r
# EXPLORATORY ANALYSIS
# Reviewd some downloaded text files outside of R
# Check row and column dimensions
# dim(test) #2,947 rows (observations) and 561 columns (variables)
# dim(testSubjects) # 2,947 rows and 1 column
# dim(testLabels) # 2,947 rows and 1 column
# dim(train) # 7,352 rows and 561 columns
# dim(trainSubjects) # 7,352 rows and 1 column
# dim(trainLabels) # 7,352 rows and 1 column
# dim(activityLabels) # 6 rows and 2 columns (V1 = integer, V2 = character)

# Check structures for data types
# str(test) # data frame with all numeric values in 99 examples
# str(testSubjects) # data frame with all integers
# str(testLabels) # data frame with all integers
# str(train) # data frame with all numeric values in 99 examples
# str(trainSubjects) # data frame with all integers
# str(trainLabels) # data frame with all integers
# str(activityLabels) # data frame with V1=integers 1-6 and V2=Factor with six levels (e.g., "WALKING")
# str(mainHeaders) # V1 is sequential numbers 1-561 and V2 is names
# mainHeaders # headers appear complex and need to be made to conform with R rules

# Check number of unique subjects and labels as data quality check (should have 30 subjects and 6 activities)
# length(unique(testSubjects$V1)) # 9 unique subjects
# length(unique(testLabels$V1)) # 6 labels, as expected, indicating walking, etc.
# length(unique(trainSubjects$V1)) # 21 unique subjects (train and test subjects sum to 30, as expected).
# length(unique(trainLabels$V1)) # 6 labels, as expected.
```

The test and training statistical data, list of subjects and activity labels was combined. This way subsequent header and text clean-up wouldn't need to be repeated for each set of test and training data.

``` r
# Combine data into tidy data, prior to any cleanup & manipulations
tidy <- rbind(train, test)
tidySubjects <- rbind(trainSubjects, testSubjects)
tidyLabels <- rbind(trainLabels, testLabels)
```

The uniquePunct function was written to identify non-alphabetical and non-numeric characters appearing in the variable labels. There are functions in R to automate data clean-up, but the steps below provided more control.

``` r
# Function to show unique punctuation
uniquePunct <- function(x) {
        longstr <- paste(x, collapse = '')
        up <- gsub("[a-zA-Z0-9]", replacement="", longstr)
        splitup <- unlist(strsplit(up, split=""))
        paste(unique(splitup), collapse = '')
}
uniquePunct(mainHeaders$V2) # "-()," are characters to consider removing from headers
```

    ## [1] "-(),"

Gsub, grep and grepl were used to clean up column names (called headers here) and ultimately to select the subset of data to include just the mean, standard deviation and categorical variables of subject IDs and activity labels, such as "walking."

``` r
# Clean up column headers
newHeaders <- mainHeaders$V2
newHeaders <- lapply(newHeaders, function(z) gsub("\\()", "", z)) # remove "()" if in that exact position
newHeaders <- lapply(newHeaders, function(z) gsub(",", "-", z)) # replace commas with hyphens
newHeaders <- lapply(newHeaders, function(z) gsub("[\\()]", ".", z)) # replace individual brackets with periods
newHeaders <- lapply(newHeaders, function(z) gsub("BodyBody", "Body", z)) # replace ...BodyBody... names with Body
newHeaders <- lapply(newHeaders, function(z) gsub("^", "Mean.", z)) # put "Mean." in fromt of each name, which will be averages

colnames(tidy) <- newHeaders # add new headers
colnames(tidySubjects) <- "subject"
colnames(tidyLabels) <- "activity"

tidy <- cbind(tidySubjects, tidyLabels, tidy) # add subject and activity columns at beginning of data frame

# Use grep to create tidy subset
tidy <- tidy[, c(1, 2, grep("mean|std", names(tidy)))] # keep subject, activity, mean and std columns
tidy <- tidy[, !grepl("meanFreq", names(tidy))] # omit meanFreq cols - grepl uses true/false, which works well with !
# names(tidy) # 68 columns, including 66 statistical variables and two categorical variables (subject and activity)

uniquePunct(names(tidy)) # now just hyphens in headers
```

    ## [1] ".-"

``` r
# Replace hyphens with periods, which are more commonly used in headers
newHeaders <- names(tidy)
newHeaders <- lapply(newHeaders, function(z) gsub("-", ".", z))
colnames(tidy) <- newHeaders
```

The next section of code cleaned up the activity labels and used these to replace the numeric 1-6 values in the original data.

``` r
# Remove underscores and capitalization, except on 2nd words, from activity labels (e.g., walkingUpstairs)
activityLabels$V2 <- tolower(activityLabels$V2)
activityLabels$V2 <- gsub("(_)([[:alpha:]])", "\\1\\U\\2", activityLabels$V2, perl=TRUE)
activityLabels$V2 <- gsub("_", "", activityLabels$V2) # wish I knew how to incorporate this in line above

# Replace numeric activity labels with descriptive strings
tidy$activity <- as.character(tidy$activity) # set to chr so it can be replaced with chr
activityLabels$V1 <- as.character(activityLabels$V1) # set to chr to match with tidy$activity
tidy$activity <- activityLabels[match(tidy$activity, activityLabels[, 1]), 2] # replace with chr activity label
```

The following code checked to see if there were any NA values in the data and found none. This would have affected the calculation of means later, which could have used the na.rm = TRUE option if NAs existed.

``` r
# colnames(tidy)[colSums(is.na(tidy)) > 0] # return colnames with NAs -- there were none
# head(tidy) # looks good
```

The next section of code prepared the final tidy.txt dataset submitted for the project. The data.table package was used, with its .SD function to subset, or summarize the data. Means were calculated for data grouped by subject and activity. These two datasets were then merged with rbind. The .SD function doesn't like non-numeric variables, which produce NA values. I removed the categorical variable not used in each means calculation.

``` r
# Use data.table to summarize statistical variables by subject and by activity
tidy2 <- data.table(tidy)
tidy2$subject <- paste0("subject", sprintf("%02d", tidy2$subject)) # Used to ID subset groups used to generate means
tidy2a <- tidy2[, -2, with=FALSE] # drop activity for SD to run properly (dislikes mean with chr variable)
tidy2b <- tidy2[, -1, with=FALSE] # drop subject for SD to run properly (dislikes mean with chr variable)
tidy2a <- tidy2a[order(tidy2a$subject), ] # sort by subject
tidy2b <- tidy2b[order(tidy2b$activity), ] # sort by activity
# options(datatable.optimize=1) # used during testing because allows NAs until issues resolved
tidy2a <- tidy2a[, lapply(.SD, mean), by=subject] # SD stands for subset data, in this case returning mean by subject.
tidy2b <- tidy2b[, lapply(.SD, mean), by=activity] # Subset data, returning mean by activity.
setnames(tidy2a, "subject", "meanGroup")
setnames(tidy2b, "activity", "meanGroup")
tidy2 <- rbind(tidy2a, tidy2b) # combine results into single file with 30 subject rows and 6 activity rows
# head(tidy2) # looks good
# tail(tidy2) # looks good
```

There were three additional data quality checks performed: (1) checked for duplicate rows; (2) checked for duplicate column names; and (3) checked for duplicate columns. Among these checks, two pairs of duplicate columns were identified. These columns were duplicated in the original data, prior to summarizing by means. The correct column names for duplicated columns could not be determined, so all columns will be included in tidy data with a note stating that this issue, documented below, remains unresolved.

``` r
# [1] Checked for duplicate rows and found none
nrow(tidy2) == nrow(unique(tidy2)) # returns TRUE, meaning no duplicate rows
```

    ## [1] TRUE

``` r
# [2] Checked for duplicate column names and found none.
sum(duplicated(names(tidy2)) | duplicated(names(tidy2), fromLast = TRUE)) == 0 # returns TRUE, meaning no duplicate column names
```

    ## [1] TRUE

``` r
# [3] Checked for duplicate columns and found 2 pairs of duplicate columns
#     Checked first for matching column sums
tidyColSums <- as.data.frame(t(tidy2[, lapply(.SD, sum, na.rm=TRUE), .SDcols=2:67 ])) # data frame with 66 colnames and sums
dupSums <- which(duplicated(tidyColSums$V1) | duplicated(tidyColSums$V1, fromLast=TRUE)) # index of 2 sets of matching column sums
```

The following variables comprised the duplicated columns.

``` r
rownames(tidyColSums)[dupSums] # Names of duplicate columns
```

    ## [1] "Mean.tBodyAccMag.mean"    "Mean.tBodyAccMag.std"    
    ## [3] "Mean.tGravityAccMag.mean" "Mean.tGravityAccMag.std"

``` r
tidyDupsIndex <- dupSums+1 # add 1 to each index because index was based on 66 columns (2:67) and tidy2 has 67 columns
tidyDupsIndex # these are the likely duplicated columns to review
```

    ## [1] 32 33 34 35

``` r
head(as.data.frame(tidy2)[, c(32:35)]) # confirmed duplicate columns, as double-checked below
```

    ##   Mean.tBodyAccMag.mean Mean.tBodyAccMag.std Mean.tGravityAccMag.mean
    ## 1            -0.4536329           -0.4970964               -0.4536329
    ## 2            -0.5352818           -0.5528125               -0.5352818
    ## 3            -0.5631408           -0.5912248               -0.5631408
    ## 4            -0.5615821           -0.6065560               -0.5615821
    ## 5            -0.4608664           -0.5216235               -0.4608664
    ## 6            -0.4647700           -0.4837906               -0.4647700
    ##   Mean.tGravityAccMag.std
    ## 1              -0.4970964
    ## 2              -0.5528125
    ## 3              -0.5912248
    ## 4              -0.6065560
    ## 5              -0.5216235
    ## 6              -0.4837906

``` r
tail(as.data.frame(tidy2)[, c(32:35)]) # confirmed duplicate columns, as double-checked below
```

    ##    Mean.tBodyAccMag.mean Mean.tBodyAccMag.std Mean.tGravityAccMag.mean
    ## 31            -0.9411107           -0.9321600               -0.9411107
    ## 32            -0.9546439           -0.9393242               -0.9546439
    ## 33            -0.9541797           -0.9465348               -0.9541797
    ## 34            -0.1679379           -0.3377540               -0.1679379
    ## 35             0.1012497            0.1164889                0.1012497
    ## 36            -0.1002041           -0.2498752               -0.1002041
    ##    Mean.tGravityAccMag.std
    ## 31              -0.9321600
    ## 32              -0.9393242
    ## 33              -0.9465348
    ## 34              -0.3377540
    ## 35               0.1164889
    ## 36              -0.2498752

``` r
# Confirmed by comparning ncol tidy2 (=67) with nrow unique transposed tidy2 (=65), finding 2 fewer columns, as expected. The rows in the transposed data were the original columns, which is why nrow was used on the transposed data.
ncol(tidy2) - nrow(unique(t(tidy2))) # returns value of 2 (67 original cols - 65 unique cols), as expected.
```

    ## [1] 2

``` r
# Note that duplicated columns were also in original raw data, prior to summarizing by means
```

The final list of column names is listed below.

``` r
names(tidy2) # later saved as tidy.txt
```

    ##  [1] "meanGroup"                  "Mean.tBodyAcc.mean.X"      
    ##  [3] "Mean.tBodyAcc.mean.Y"       "Mean.tBodyAcc.mean.Z"      
    ##  [5] "Mean.tBodyAcc.std.X"        "Mean.tBodyAcc.std.Y"       
    ##  [7] "Mean.tBodyAcc.std.Z"        "Mean.tGravityAcc.mean.X"   
    ##  [9] "Mean.tGravityAcc.mean.Y"    "Mean.tGravityAcc.mean.Z"   
    ## [11] "Mean.tGravityAcc.std.X"     "Mean.tGravityAcc.std.Y"    
    ## [13] "Mean.tGravityAcc.std.Z"     "Mean.tBodyAccJerk.mean.X"  
    ## [15] "Mean.tBodyAccJerk.mean.Y"   "Mean.tBodyAccJerk.mean.Z"  
    ## [17] "Mean.tBodyAccJerk.std.X"    "Mean.tBodyAccJerk.std.Y"   
    ## [19] "Mean.tBodyAccJerk.std.Z"    "Mean.tBodyGyro.mean.X"     
    ## [21] "Mean.tBodyGyro.mean.Y"      "Mean.tBodyGyro.mean.Z"     
    ## [23] "Mean.tBodyGyro.std.X"       "Mean.tBodyGyro.std.Y"      
    ## [25] "Mean.tBodyGyro.std.Z"       "Mean.tBodyGyroJerk.mean.X" 
    ## [27] "Mean.tBodyGyroJerk.mean.Y"  "Mean.tBodyGyroJerk.mean.Z" 
    ## [29] "Mean.tBodyGyroJerk.std.X"   "Mean.tBodyGyroJerk.std.Y"  
    ## [31] "Mean.tBodyGyroJerk.std.Z"   "Mean.tBodyAccMag.mean"     
    ## [33] "Mean.tBodyAccMag.std"       "Mean.tGravityAccMag.mean"  
    ## [35] "Mean.tGravityAccMag.std"    "Mean.tBodyAccJerkMag.mean" 
    ## [37] "Mean.tBodyAccJerkMag.std"   "Mean.tBodyGyroMag.mean"    
    ## [39] "Mean.tBodyGyroMag.std"      "Mean.tBodyGyroJerkMag.mean"
    ## [41] "Mean.tBodyGyroJerkMag.std"  "Mean.fBodyAcc.mean.X"      
    ## [43] "Mean.fBodyAcc.mean.Y"       "Mean.fBodyAcc.mean.Z"      
    ## [45] "Mean.fBodyAcc.std.X"        "Mean.fBodyAcc.std.Y"       
    ## [47] "Mean.fBodyAcc.std.Z"        "Mean.fBodyAccJerk.mean.X"  
    ## [49] "Mean.fBodyAccJerk.mean.Y"   "Mean.fBodyAccJerk.mean.Z"  
    ## [51] "Mean.fBodyAccJerk.std.X"    "Mean.fBodyAccJerk.std.Y"   
    ## [53] "Mean.fBodyAccJerk.std.Z"    "Mean.fBodyGyro.mean.X"     
    ## [55] "Mean.fBodyGyro.mean.Y"      "Mean.fBodyGyro.mean.Z"     
    ## [57] "Mean.fBodyGyro.std.X"       "Mean.fBodyGyro.std.Y"      
    ## [59] "Mean.fBodyGyro.std.Z"       "Mean.fBodyAccMag.mean"     
    ## [61] "Mean.fBodyAccMag.std"       "Mean.fBodyAccJerkMag.mean" 
    ## [63] "Mean.fBodyAccJerkMag.std"   "Mean.fBodyGyroMag.mean"    
    ## [65] "Mean.fBodyGyroMag.std"      "Mean.fBodyGyroJerkMag.mean"
    ## [67] "Mean.fBodyGyroJerkMag.std"

The data was saved as tidy.txt. The tidy.txt data was read back into R to verify that it looked as expected, which it did.

``` r
# Save tidy2 as tidy.txt to submit with project
write.table(tidy2, "./tidy.txt", col.names = TRUE, row.names = FALSE)

# check that file was saved and can be read again with formatting as expected
# boo <- read.table("./tidy.txt", header=TRUE)
# names(boo) # looks good
# head(boo) # looks good
# str(boo) # looks good

# Remove objects created during session
```

The objects created during the session were removed.

``` r
rm(list = ls())
```

The credits and references for the data and a helpful related paper are cited below:

``` r
# This data was obtained from: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Source:
#  Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)
#  1 - Smartlab - Non-Linear Complex Systems Laboratory
#  DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy.
#  2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living
#  Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain
#  activityrecognition '@' smartlab.ws

#  Citation: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz.
#  A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European Symposium
#  on Artificial Neural Networks, Computational Intelligence and Machine Learning, ESANN 2013. Bruges,
#  Belgium 24-26 April 2013.
```

This document was written in R markdown and knitted with knitr into this markdown file.
