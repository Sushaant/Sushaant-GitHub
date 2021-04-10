Jobs <- read.csv("D:\\DMML projects\\Fake Job prediction\\fake_job_postings.csv")
glimpse(Jobs)
Jobs <- Jobs[1:5000,]

#summary(Jobs)

library(tidyverse)
library(caret)
library(dplyr)
library(tinytex)
library(caret)
library(data.table)
library(tidytext)
library(stopwords)
library(readr)
library(tm)
library(wordcloud)
library("RColorBrewer")
library(randomForest)
library("RCurl")

# assigning column names 
columns <- colnames(Jobs)
#Displaying structure of the dataset
str(Jobs)

# Grouping only unique values from the columns
unique_columns <-  Jobs %>% 
  summarise(n_title = n_distinct(title),
            n_location = n_distinct(location),
            n_department = n_distinct(department),
            n_salary_range = n_distinct(salary_range),
            n_employment_type = n_distinct(employment_type),
            n_required_experience = n_distinct(required_experience),
            n_required_education = n_distinct(required_education),
            n_industry = n_distinct(industry),
            n_function = n_distinct(function.),
            n_fraudulent = n_distinct(fraudulent))

print(unique_columns)

# Using ggplot library to distinguish fraudulent and genuine count of jobs
Jobs %>% group_by(fraudulent) %>%  ggplot(aes(fraudulent, group = fraudulent)) + 
  geom_bar(aes(fill = fraudulent), stat = "count") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  geom_text(aes(label=..count..),stat='count',position=position_stack(vjust=0.5)) + 
  ggtitle("Genuine and Fraud Jobs count") + xlab("Fradulent Flag") + ylab("Job Count") + theme_bw()

degree_distribution <- Jobs %>% group_by(required_education, fraudulent) %>% summarise(count = n())

# Using ggplot library to plot the Degree required and the job count

degree_distribution %>%  ggplot(aes(reorder(
  degree_distribution$required_education, -degree_distribution$count), degree_distribution$count)) +
  geom_bar(stat = "identity", aes(fill = fraudulent)) + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ggtitle("Jobs Per Required Education Feature") + xlab("Required Education") + ylab("Job Count")


skim(Jobs)
# Creating a corpus object which represents the collection of text documents
corpus <- Corpus(VectorSource(Jobs$description))
# Using text-mining(tm) to remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# Finding and removing the end words like "en" which forms the end line of each description
corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
# perfomring stemming to find a singular value to each word and it's plural forms
corpus <- tm_map(corpus, stemDocument)

# term document matrix is performed to convert text to matrix form to take high frequency values
frequencies <- DocumentTermMatrix(corpus)
# removing terms more sparse than 0.99
sparse_data <- removeSparseTerms(frequencies, 0.995)
# Converting to dataframe 
sparse_data_df <- as.data.frame(as.matrix(sparse_data))
# Assigning column names for the dataframe
colnames(sparse_data_df) <- make.names(colnames(sparse_data_df))
# Dependent variable column is added
sparse_data_df$fraudulent <- Jobs$fraudulent 
# removing the duplicated column names
colnames(sparse_data_df) <- make.unique(colnames(sparse_data_df), sep = "_")

#Generating random numbers for sampling purpose
set.seed(2000)
#creating Data partition with random samples for the training and test data "times = 1" (single partition)
test_index <- createDataPartition(y = sparse_data_df$fraudulent, times = 1, p = 0.1, list= FALSE)
#Splitting training and test data
train_set <- sparse_data_df[-test_index, ]
test_data <- sparse_data_df[test_index, ]
#converting columns to factors for training and test data
train_set$fraudulent = as.factor(train_set$fraudulent)
test_data$fraudulent = as.factor(test_data$fraudulent)



########SVM
n <- 500

#taking only certain sample of data for making prediction
index <- sample(nrow(train_set), n)

svm_train_data <- train_set[index, ]


#Training the model SVM with training data
svm_fit <- train(fraudulent ~ ., data = svm_train_data, method = "svmLinear")
svm_fit
#predicting against the test data
svm_pred <- predict(svm_fit, newdata = test_data)


#Confusion Matrix
CM_svm <- confusionMatrix(svm_pred, test_data$fraudulent)

CM_svm


#########Random Forrest

#taking only certain sample of data for making prediction
index <- sample(nrow(train_set), n)

rf_train_data <- train_set[index, ]

#Training the model Random Forest of 150 trees with training data
subset_train_rf <- train(fraudulent ~ ., method = "rf", data = rf_train_data, 
                         ntree = 150)
subset_train_rf
#predicting against the test data
predict_rf <- predict(subset_train_rf, newdata = test_data)

#Confusion Matrix
CM_RF <- confusionMatrix(predict_rf, test_data$fraudulent)

CM_RF
