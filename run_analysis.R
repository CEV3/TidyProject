# Program run_analysis.R
# Some code used for initial exploratory analysis is commented out with notes

library("data.table")

# Download commented out per instructions of code should work as long as Samsung data is in working directory
# Get files
# url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(url1, destfile = "./UCIHAR.zip", method="libcurl")
# unzip("./UCIHAR.zip", exdir = ".") # unzips to folder "UCI HAR Dataset"

# list.files("./UCI HAR Dataset") # List contents of UCIHAR directory
# list.files("./UCI HAR Dataset/test") # List contents of test subdirectory
# list.files("./UCI HAR Dataset/train") # List contents of train subdirectory

# Read in key data .txt files:
test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE) # data for test subjects
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE) # subject ID column
testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE) # labels 1-6 indicating walking, etc.
train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE) # data for training subjects
trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE) # subject ID column
trainLabels <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE) # labels 1-6 indicating walking, etc.
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt") # labels with text descriptions of activity such as "WALKING"
mainHeaders <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE) # variables names to be used to create column headers

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

# Combine data into tidy data, prior to cleanup & manipulations
tidy <- rbind(train, test)
tidySubjects <- rbind(trainSubjects, testSubjects)
tidyLabels <- rbind(trainLabels, testLabels)

# Function to show unique punctuation
uniquePunct <- function(x) {
        longstr <- paste(x, collapse = '')
        up <- gsub("[a-zA-Z0-9]", replacement="", longstr)
        splitup <- unlist(strsplit(up, split=""))
        paste(unique(splitup), collapse = '')
}
uniquePunct(mainHeaders$V2) # "-()," are the four characters to consider removing from headers

# Clean up column headers -- more cleanup to follow
newHeaders <- mainHeaders$V2
newHeaders <- lapply(newHeaders, function(z) gsub("\\()", "", z)) # remove "()" if in that exact position
newHeaders <- lapply(newHeaders, function(z) gsub(",", "-", z)) # replace commas with hyphens
newHeaders <- lapply(newHeaders, function(z) gsub("[\\()]", ".", z)) # replace individual brackets with periods
newHeaders <- lapply(newHeaders, function(z) gsub("BodyBody", "Body", z)) # replace ...BodyBody... names with Body
newHeaders <- lapply(newHeaders, function(z) gsub("^", "Mean.", z)) # put "Mean." in fromt of each variable name to be clear that these are average values

colnames(tidy) <- newHeaders # use new headers
colnames(tidySubjects) <- "subject"
colnames(tidyLabels) <- "activity"

tidy <- cbind(tidySubjects, tidyLabels, tidy) # add subject and activity columns to left side of data frame

# Use grep to create tidy subset
tidy <- tidy[, c(1, 2, grep("mean|std", names(tidy)))] # keep subject, activity, mean and std columns
tidy <- tidy[, !grepl("meanFreq", names(tidy))] # omit meanFreq cols - grepl uses true/false, which works well with !
# names(tidy) # 68 columns, including 66 statistical variables and two categorical variables (subject and activity)

uniquePunct(names(tidy)) # now just periods and hyphens in headers

# Replace hyphens with periods, which are more commonly used in headers (not ideal but it works here given complexity)
newHeaders <- names(tidy)
newHeaders <- lapply(newHeaders, function(z) gsub("-", ".", z))
colnames(tidy) <- newHeaders

# Remove underscores and capitalization, except on 2nd words, from activity labels (e.g., new labels like, walkingUpstairs)
activityLabels$V2 <- tolower(activityLabels$V2)
activityLabels$V2 <- gsub("(_)([[:alpha:]])", "\\1\\U\\2", activityLabels$V2, perl=TRUE)
activityLabels$V2 <- gsub("_", "", activityLabels$V2) # wish I knew how to incorporate this in single line above

# Replace numeric activity labels with descriptive strings
tidy$activity <- as.character(tidy$activity) # set to chr so it can be replaced with chr
activityLabels$V1 <- as.character(activityLabels$V1) # set to chr to match with tidy$activity
tidy$activity <- activityLabels[match(tidy$activity, activityLabels[, 1]), 2] # replace with chr activity label

# colnames(tidy)[colSums(is.na(tidy)) > 0] # return colnames with NAs -- there were none
# head(tidy) # looks good

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
# head(tidy2) # looks good shows meanGroup rows for subject01 through subject01 in first six rows
# tail(tidy2) # looks good shows meanGroup rows for laying, sitting, standing, walking, walkingDownstairs and walkingUpstairs in final 6 rows

# Three more data quality checks:

# [1] Checked for duplicate rows and found none
nrow(tidy2) == nrow(unique(tidy2)) # returns TRUE, meaning no duplicate rows

# [2] Checked for duplicate column names and found none.
sum(duplicated(names(tidy2)) | duplicated(names(tidy2), fromLast = TRUE)) == 0 # returns TRUE, meaning no duplicate column names

# [3] Checked for duplicate columns and found 2 pairs of duplicate columns
#     Checked first for matching column sums
tidyColSums <- as.data.frame(t(tidy2[, lapply(.SD, sum, na.rm=TRUE), .SDcols=2:67 ])) # data frame with 66 colnames and sums
dupSums <- which(duplicated(tidyColSums$V1) | duplicated(tidyColSums$V1, fromLast=TRUE)) # index of 2 sets of matching column sums
rownames(tidyColSums)[dupSums] # Names of duplicate columns
tidyDupsIndex <- dupSums+1 # add 1 to each index because index was based on 66 columns (2:67) and tidy2 has 67 columns
tidyDupsIndex
head(as.data.frame(tidy2)[, c(32:35)]) # duplicate columns, as summarized below
tail(as.data.frame(tidy2)[, c(32:35)]) # duplicate columns, as summarized below
# Confirmed by comparning ncol tidy2 (=67) with nrow unique transposed tidy2 (=65), finding 2 fewer columns, as expected. The rows in the transposed data were the original columns, which is why nrow was used on the transposed data.
ncol(tidy2) - nrow(unique(t(tidy2))) # returns value of 2 (67 original cols - 65 unique cols), as expected.
# Note that duplicated columns were also in original raw data, prior to summarizing by means.

# All of these columns will be included in the tidy data with a note that the duplicatiions were in original data and are unresolved

names(tidy2) # final list of column names. tidy2 was saved as tidy.txt.

# Save tidy2 as tidy.txt to submit with project
write.table(tidy2, "./tidy.txt", col.names = TRUE, row.names = FALSE)

# check that file was saved and can be read again with formatting as expected
# boo <- read.table("./tidy.txt", header=TRUE)
# names(boo) # looks good
# head(boo) # looks good
# str(boo) # looks good

# Remove objects created during session
rm(list = ls())

# This data was obtained from: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Source:
#  Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)
#  1 - Smartlab - Non-Linear Complex Systems Laboratory
#  DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy.
#  2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living
#  Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain
#  activityrecognition '@' smartlab.ws

# Citation: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz.
#  A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European Symposium
#  on Artificial Neural Networks, Computational Intelligence and Machine Learning, ESANN 2013. Bruges,
#  Belgium 24-26 April 2013.
