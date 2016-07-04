# This is R script for programming assignment 4 for "Getting and Cleaning the Data"
###Part1: Create a merged set from raw data
#---------------------1. Get/load packages, we will be using-----------------------------------
    #uncomment string below if you do not have packages installed!
    #install.packages("data.table", "dplyr", "plyr")
#load packages we will be using
library (data.table, dplyr, plyr)


#---------------------Set directory with data files, in my case------------------------ 
setwd("~/Dropbox/Study/DataScienceSpec/DataCleaning")

#---------------------Load data files--------------------------------------------------
X_test <- readLines("UCI HAR Dataset/test/X_test.txt")
labels_test <- readLines("UCI HAR Dataset/test/y_test.txt")
subject_test <- readLines("UCI HAR Dataset/test/subject_test.txt")
X_train <- readLines("UCI HAR Dataset/train/X_train.txt")
labels_train <- readLines("UCI HAR Dataset/train/y_train.txt")
subject_train <- readLines("UCI HAR Dataset/train/subject_train.txt")


#----2. Get all the variables with mean() and std(), total 79 variables and write them in names_ms vector  

features_list <- readLines("UCI HAR Dataset/features.txt")
mean_var <- grep("mean()", features_list) #get all names with numbers mean()
std_var <- grep("std()", features_list) #get all names numbers with std()
mean_std <- sort(c(mean_var, std_var)) #all names numbers for mean and std together, ordered
names_list <- features_list[mean_std] #all NAMES with mean and std
#names in names_list have numbers in them, to delete them:
names_ms <- c(substring(names_list[c(1:6)],3), substring(names_list[c(7:16)], 4), substring(names_list[c(17:79)], 5)) 
#----Function trim2() to transform data in X into a set of columns, labeled according to labels in Y  

trim2 <- function (var, n) {
        x <- c()
        y <- cbind()
        for (i in 1:n) {
        x <- as.numeric(as.character(data.frame(strsplit(var[i], " +"))[,1]))
        m <- length(x)
        x <- x[mean_std+1]
        y <- rbind(y,x)}
        colnames(y) <- names_ms
        rownames(y) <- c(1:n)
        y}

#---apply trim2() to test and train datasets---------
trimmed_test2 <- trim2(X_test, length(X_test))
trimmed_train2 <- trim2(X_train, length(X_train))


# here good idea is to heck dimension of a result, we have 79 variables, so we test this:
#dim(trimmed_test2)
# [1] 2947   79
# dim(trimmed_train2)
# [1] 7352   79


#Create test_set with all the designations using data.table package

test_set <- data.table(obs= 1: length(X_test), subject = subject_test, activity = labels_test, trimmed_test2)

# Again check dimension of a result, remember, we have 79 variables + subject + activity + we introduced obs for obsevation number here, so 82 in total  
# dim(test_set)
# [1] 2947   82  

#Create train_set with all designations using data.table package

train_set <- data.table(obs= (length(X_test)+1): (length(X_train)+length(X_test)), subject = subject_train, activity = labels_train, trimmed_train2) #here we create continuous numbering, test observations will be from 1 to 2947 and train from 2948 to 10299 
# dim(train_set)
# [1] 7352   82


#--------Merge test and train sets----------------------------------------------------------------

fullset <- rbind(test_set, train_set)

#------3.Relabel activities' NUMBERS with ACTIVITIES DESCRIPTIONS---------------------------------

#----Check what activities names and numbers we have:----------------------------------------------
# activities <- readLines("UCI HAR Dataset/activity_labels.txt")
# activities

#[1] "1 WALKING"            "2 WALKING_UPSTAIRS"   "3 WALKING_DOWNSTAIRS" "4 SITTING"           
#[5] "5 STANDING"           "6 LAYING"

activities <- readLines("UCI HAR Dataset/activity_labels.txt")
message("activities numbering:")
print(activities)
    
    #[1] "1 WALKING"            "2 WALKING_UPSTAIRS"   "3 WALKING_DOWNSTAIRS" "4 SITTING"           
    #[5] "5 STANDING"           "6 LAYING"
)

#----Create relabeling vector:---------------------------------------------------------
activities <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

#----Relabel function------------------------------------------------------------------
relabel <- function(m, activities){
        for (i in 1:length(m)) {
        x <- m[i]
        m[i]<- activities[as.numeric(x)]}
        m}

#----Relabelling--------------------------------------------------------------------
fullset2 <- fullset #keep original fullset unaltered just in case
fullset2$activity <- relabel(fullset2$activity, activities) #relabel activitiy row

###Part2: Create a tidy set    


    z <- colnames(fullset2)
    s <- fullset2[ , mean(eval(as.name(z[4]))), by=.(subject, activity)]
            for (i in 5:82){
            m <- fullset2[,mean(eval(as.name(z[i]))), by=.(subject, activity)]
            s <- cbind(s, m$V1)}
        colnames(s) <- z[2:82]
 
#-------The result above here is formally correct, but looks as a mess because of strange arrangement,
#-------so we will arrange it first by subject, then by activity

    s$subject <- as.numeric(s$subject) #subject was a character vector, we make it numeric
    s <- arrange(s, subject, activity) ##this is our final tidyset!
        
    write.table(s, file = "tidyset.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
    tidyset <- read.table("tidyset.txt")
    
    message("The first part of the script creates full dataset, which is a merged dataset for train and test sets. This is an intemediate dataset. It will be used to create a tidy data set")
    message("Full dataset is huge, its dimensions are:")
    print(dim(fullset2))
    message("The second part of the script creates a new dataset 'tidyset.txt'. This is a tidy set made from merged data of Full dataset, refer to Codebook for more detailed description")
    message("The tidy dataset is more compact compared to full set, its dimensions are:")
    print(dim(tidyset))



        
        
