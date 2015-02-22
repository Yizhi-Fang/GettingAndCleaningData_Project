library(data.table)

## this function read data from file and return data table
file2data <- function(file){
    df <- read.table(file.path(getwd(), file))
    data.table(df)
}

## read data from file
subject_train <- fread(file.path(getwd(), 'UCI HAR Dataset/train', 'subject_train.txt'))
subject_test <- fread(file.path(getwd(), 'UCI HAR Dataset/test', 'subject_test.txt'))

activity_train <- fread(file.path(getwd(), 'UCI HAR Dataset/train', 'y_train.txt'))
activity_test <- fread(file.path(getwd(), 'UCI HAR Dataset/test', 'y_test.txt'))

# I can't fread these two files, maybe because they have multiple columns
result_train <- file2data('UCI HAR Dataset/train/X_train.txt')
result_test <- file2data('UCI HAR Dataset/test/X_test.txt')

## merge training and test data
subject <- rbind(subject_train, subject_test)
setnames(subject, 'V1', 'subject')

activity <- rbind(activity_train, activity_test)
setnames(activity, 'V1', 'activityNum')

result <- rbind(result_train, result_test)

data <- cbind(subject, activity, result)

## extract only the mean and standard variation
feature <- file2data('UCI HAR Dataset/features.txt')
setnames(feature, c('V1', 'V2'), c('featureNum', 'featureName'))

feature <- feature[grep('mean\\(\\)|std\\(\\)', featureName)]
select <- c('subject', 'activityNum', paste0('V', feature$featureNum))
data <- subset(data, select = select)

## use descriptive names for activity
activity_labels <- file2data('UCI HAR Dataset/activity_labels.txt')
setnames(activity_labels, c('V1', 'V2'), c('activityNum', 'activityName'))

data <- merge(data, activity_labels, by = 'activityNum', all = T)

## use descriptive names for features
feature$featureName <- as.character(feature$featureName)
setnames(data, paste0('V', feature$featureNum), feature$featureName)

## create a new data set with mean of each feature for each activity and each subject
data$subject <- as.factor(data$subject)
split_subject <- split(data, data$subject)

mean_data <- data.table()
for(i in 1:length(split_subject)){
    temp <- split_subject[[i]]
    temp <- temp[, 3:ncol(temp), with = F]
    temp <- aggregate(. ~ activityName, temp, mean)
    # calculate mean of each feature grouping by activityName
    temp <- data.table(temp)
    temp <- cbind(i, temp)
    setnames(temp, names(temp)[1], 'subject')
    mean_data <- rbind(mean_data, temp)
}

## write all cleaned up data into files
write.table(data, 'cleaned_data.txt', row.names = F, sep = ',')
write.table(mean_data, 'mean_variables_per_activity_subject.txt', row.names = F, sep = ',')
