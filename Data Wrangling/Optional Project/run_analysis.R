# Load data sets ===========================================================================

# Read the training set into a dataframe
train <- read.table("X_train.txt")

# Read the training labels into a vector
train_labels <- scan("y_train.txt")

# Read the "subject" information for training set into a vector
train_subjects <- scan ("subject_train.txt")

# Read the test set into a dataframe
test <- read.table("X_test.txt")

# Read the test labels into a vector
test_labels <- scan("y_test.txt")

# Read the "subject" information for test set into a vector
test_subjects <- scan ("subject_test.txt")

# Read activity labels (links the class labels with their activity name)
activity <- read.table("activity_labels.txt")

# Read features list into a vector and make the names unique
features <- read.table("features.txt")
features <- as.vector(features[,2])
features <- make.names(names = features, unique = TRUE)

# Assign column names to training and test sets using features data
colnames(x = train) <- features
colnames(x = test) <- features

# Add the variable "Subject" to training and test sets
train$Subject <- train_subjects
test$Subject <- test_subjects

# Add the variable "ActivityLabel" to training and test sets using the label vectors
train$ActivityLabel <- train_labels
test$ActivityLabel <- test_labels

# Rename column names for activity dataset
colnames(activity) <- c("ActivityLabel","ActivityName")

# Join training and test data sets to activity dataset
library(dplyr) #load dplyr package
train <- left_join(train, activity, by = "ActivityLabel")
test <- left_join(test, activity, by = "ActivityLabel")

# Merge training and test data sets ========================================================
UCI <- bind_rows(test, train)

# Add columns for mean and standard deviation ==============================================
UCI$grandMean <- UCI %>% 
  subset(select = c(-ActivityLabel,-ActivityName,- Subject)) %>% 
  rowMeans()

UCI$grandSD <- subset(UCI, select = c(-ActivityLabel,-ActivityName,- Subject)) %>%
  apply(MARGIN = 1,FUN = sd)

# Creating tidy dataset with average of each variable for each activity & each subject =====
UCI_tidy <- UCI %>% 
  group_by(ActivityLabel,ActivityName,Subject) %>% 
  summarise_all(funs(mean(.,na.rm = TRUE)))

# Exporting the tidy data set into a CSV file
write.csv(UCI_tidy, "tidy_data.csv")