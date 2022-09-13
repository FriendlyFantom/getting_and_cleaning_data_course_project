# Read in data
X_train <- read.table('UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('UCI HAR Dataset/train/subject_train.txt')

X_test <- read.table('UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('UCI HAR Dataset/test/subject_test.txt')

features <- read.table('UCI HAR Dataset/features.txt')
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt')

# Combine X (measurements) and y (activity labels)
train <- cbind(X_train, y_train, subject_train)
test <- cbind(X_test, y_test, subject_test)

# Merge data
df <- rbind(train, test)

# Filter variables to only mean and std measurements (plus activity and subject labels)
names(df) <- c(features$V2, 'Activity', 'Subject')
match_pattern <- 'mean\\(\\)|std|Activity|Subject'
keep_features <- grepl(match_pattern, names(df))
df <- df[, keep_features]

# Rename activities according to labels
df$Activity <- activity_labels[match(df$Activity, activity_labels$V1), 'V2']

# Rename variables
rename <- function(x){
  x <- sub('tBody', 'Body ', x)
  x <- sub('tGravity', 'Gravity ', x)
  x <- sub('fBody|fBodyBody', 'Fourier Transform Body ', x)
  x <- sub('Acc', 'Accelerometer ', x)
  x <- sub('Gyro', 'Gyroscope ', x)
  x <- sub('Jerk', 'Jerk ', x)
  x <- sub('Mag', 'Magnitude ', x)
  x <- sub('-mean\\(\\)-', 'Mean ', x)
  x <- sub('-std\\(\\)-', 'Std ', x)
  x <- sub('-mean\\(\\)', 'Mean', x)
  x <- sub('-std\\(\\)', 'Std', x)
  x
}

names(df) <- rename(names(df))

# Group by Activity and Subject as new table and write file
library(dplyr)
activity_subject_means <- 
  df %>% 
  group_by(Activity, Subject) %>% 
  summarise(across(everything(), list(mean)))

# Clean up variable names
names(activity_subject_means) <- sub('_1', '', names(activity_subject_means))

write.table(activity_subject_means, file='activity_subject_means.txt', row.names=FALSE)
