
####################################################

#   AM Expert 2018 (Machine Learning Hackathon)    #

####################################################

# Load Libraries

library(data.table)

library(dplyr)

library(tidyr)

library(lubridate)

library(h2o)

library(ggplot2)

library(corrplot)

library(ggthemes)

library(gridExtra)

library(caTools)

library(catboost)

# Clear Environment and load datasets

rm(list = ls())

setwd("D:/Career Development/Analytics Vidhya/American Express/Data")

train_data <- read.csv("train.csv", stringsAsFactors = F,
                       
                       na.strings = c("", " ", "N/A"))

historical_user_logs <- fread(file = "historical_user_logs.csv",
                              
                              stringsAsFactors = F,
                              
                              nThread = 4,
                              
                              showProgress = T)

test_data <- read.csv("test.csv", stringsAsFactors = F,
                       
                       na.strings = c("", " ", "N/A"))


# First let's concentrate on the historical_user_logs and extract usable features from it

str(historical_user_logs)

# From the user logs extract how many times each user viewed and was interested in each product
historical_user_logs_product_summary <- historical_user_logs %>% 
  group_by(user_id,product, action) %>% summarise(Action_Taken = n()) %>% as.data.frame()

rm(historical_user_logs)

historical_user_logs_product_summary <- spread(historical_user_logs_product_summary,
                                               key = "action",
                                               value = "Action_Taken")
# Where values are NA replace with 0
historical_user_logs_product_summary$interest[is.na(historical_user_logs_product_summary$interest)] = 0
historical_user_logs_product_summary$view[is.na(historical_user_logs_product_summary$view)] = 0

# Add this data at user and product level to train and test data sets
train_data$user_id <- as.character(train_data$user_id)
test_data$user_id <- as.character(test_data$user_id)
historical_user_logs_product_summary$user_id <- as.character(historical_user_logs_product_summary$user_id)
train_data <- left_join(train_data, historical_user_logs_product_summary)
test_data <- left_join(test_data, historical_user_logs_product_summary)

rm(historical_user_logs_product_summary)

###########################################
#### Data Preparation, EDA, & Cleaning ####
###########################################

# Let's check the distribution of the target_variable
prop.table(table(train_data$is_click)) # Only 6.7% clicks indicates data is highly unbalanced

# Let's check if users are common between train and test
sum(unique(train_data$user_id) %in% unique(test_data$user_id)) # 39120
# 39120 users are common between train and test so we can use user_id related aggregated features

# Combine train and test for data prep
combined_data <- rbind(train_data[,-15], test_data)

# Convert DateTime to proper structure
combined_data$DateTime <- parse_date_time(combined_data$DateTime,
                                          orders = c("ymd HM"))

# Extract features which may be useful
combined_data$Date <- substr(date(combined_data$DateTime),9,10)
combined_data$Timeofday <- strftime(combined_data$DateTime, format="%H:%M")
combined_data$Timeofday <-  gsub(":", ".",combined_data$Timeofday)
combined_data$Timeofday <- as.numeric(combined_data$Timeofday)
combined_data <- combined_data[,-2]

# Check missing values in the dataset for each feature
sapply(combined_data, function(x) sum(is.na(x)))

# 1) product_category_2 has nearly 80% missing values. Drop it
combined_data <- combined_data %>% select(-product_category_2)
str(combined_data)

# 2) gender, user_group_id, user_depth and age_level are always missing together. Indicating
# that demographic data is missing for these users. 
# Since there is no reasonable way to impute these values with the available data we will 
# impute with the majority category
combined_data$gender[is.na(combined_data$gender)] = "Male"
combined_data$user_group_id[is.na(combined_data$user_group_id)] = 3
combined_data$age_level[is.na(combined_data$age_level)] = 3
combined_data$user_depth[is.na(combined_data$user_depth)] = 3

# Convert these vars to factors where appropriate
combined_data$gender <- as.factor(combined_data$gender)
combined_data$user_group_id <- as.factor(combined_data$user_group_id)
combined_data$product <- as.factor(combined_data$product)
combined_data$campaign_id <- as.factor(combined_data$campaign_id)
combined_data$webpage_id <- as.factor(combined_data$webpage_id)
combined_data$var_1 <- as.factor(combined_data$var_1)

# For interest and views if they are missing impute with 0's
combined_data$view[is.na(combined_data$view)] = 0
combined_data$interest[is.na(combined_data$interest)] = 0

# Re Create train and test
train_data$is_click <- ifelse(train_data$is_click == 1, "Yes", "No")

is_click <- train_data$is_click
train_data <- cbind(combined_data[1:nrow(train_data),], is_click)
test_data <- combined_data[((nrow(train_data) + 1) : nrow(combined_data)),]

rm(combined_data)
rm(is_click)

# Further EDA, visualizations and feature engineering
# EDA - Data Visualization #

# 1) For categorical variables
# Create a function which outputs two plots. Count of the target variable categories and 
# percentage of the target variable in each category
Plotter_Categorical <- function(data, source_var, target_var){
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar() +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  
  p2 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar(position = "fill") +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  x11() 
  grid.arrange(p1, p2)
  
}

# 2) For numeric variables
Plotter_Numeric <- function(data, source_var, target_var){
  
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
    theme_gdocs() + scale_fill_tableau(name = target_var) + geom_density(alpha = 0.3) +
    labs(x = source_var, y = "density") 
  
  p2 <- ggplot(train_data, aes(x = data[,c(target_var)], y = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_boxplot() + theme_gdocs() + scale_fill_tableau(name = target_var) + 
    labs(x = target_var, y = source_var)
  
  x11() 
  grid.arrange(p1, p2)
  
}

# a) For product
Plotter_Categorical(train_data, "product", "is_click")
# Clicks are nearly same across all products. Product C has highest no. of occurences and clicks.

# b) For campaign_id
Plotter_Categorical(train_data, "campaign_id", "is_click")
# Again no significant difference in clicks across campaigns

# c) For webpage_id
Plotter_Categorical(train_data, "webpage_id", "is_click")
# No significant difference in clicks across webpages. Unlikely to be important feature

# d) For product_category
Plotter_Categorical(train_data, "product_category_1", "is_click")
# Product category 3 seems to have the highest clicks

# e) For user_group_id
Plotter_Categorical(train_data, "user_group_id", "is_click")
# Too many user groups with very little difference among them. We might think of dropping this var

# f) For gender
Plotter_Categorical(train_data, "gender", "is_click")
# Nearly identical distribution and behaviour for different genders. Unlikely to be important

# g) For age_level
Plotter_Numeric(train_data, "age_level", "is_click")
#age_level seems to have a nearly Gaussian distribution with lower age_level users generating
#more clicks

# h) For user_depth
Plotter_Categorical(train_data, "user_depth", "is_click")
# user_depth indicates level of interaction of user with platform. Interestingly higher user_depth
# does not seem to translate to a higher number of clicks

# i) For city_development_index
Plotter_Categorical(train_data, "city_development_index", "is_click")
# Impute city development index with the majority class
train_data$city_development_index[is.na(train_data$city_development_index)] = 2
test_data$city_development_index[is.na(test_data$city_development_index)] = 2

# j) For var_1
Plotter_Categorical(train_data, "var_1", "is_click")
# Anonymised session feature. Again not likely to be useful

# k) For view
Plotter_Numeric(train_data, "view", "is_click")
# Distribution indicates extreme no. of outliers. Let us look at this in more detail
quantile(train_data$view, probs = seq(0,1,by = 0.01))
# Indeed there are outliers but this may simply indicate a higher level of engagement of the user.
# We will not cap this

# l) For interest
train_data$interest <- as.integer(train_data$interest)
Plotter_Numeric(as.data.frame(train_data), "interest", "is_click")

# Distribution indicates extreme no. of outliers. Let us look at this in more detail
quantile(train_data$interest, probs = seq(0,1,by = 0.01))

# We wil not cap this

# m) For TimeofDay
Plotter_Numeric(train_data, "Timeofday", "is_click")
# Clearly indicates that clicks remain same throughout the day

# n) For Date
train_data$Date <- as.numeric(train_data$Date)
test_data$Date <- as.numeric(test_data$Date)
Plotter_Numeric(train_data, "Date", "is_click")
# Dates closer to the begining of the month seem to have higher click rates though very slightly

# Split the train data into train and validation
set.seed(123)
indices <- sample.split(train_data$is_click, SplitRatio = 0.75)
train <- train_data[indices,]
validation <- train_data[!indices,]

train <- as.data.frame(train)
validation <- as.data.frame(validation)
test_data <- as.data.frame(test_data)

# Create a Local h20 cluster
h2o.shutdown()
h2o.init(nthreads = -1, min_mem_size = "3g")

# Transfer data to the cluster
train.h2o <- as.h2o(train %>% select(-session_id, -user_id))
validation.h20 <- as.h2o(validation %>% select(-session_id, -user_id))
test.h2o <- as.h2o(test_data)

# Run a GBMmodel with default params
colnames(train_full.h2o)
y.dep = 17
x.indep = 1:16

gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                     validation_frame = validation.h20, stopping_metric = "AUC",
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123, 
                     keep_cross_validation_predictions = T)
summary(gbm.model)
h2o.varimp(gbm.model)
# AUC of 0.58 on validation_data

# Use the entire train data to make model and test LB score
train_full.h2o <- as.h2o(train_data)
gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                     validation_frame = validation.h20, stopping_metric = "AUC",
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123, 
                     keep_cross_validation_predictions = T)
# Predict on test data
predictions_gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Submission 1- GBM default
submission_gbm_new_features <- as.data.frame(cbind(as.integer(as.character(test_data$session_id)), as.character(predictions_gbm$Yes)))
colnames(submission_gbm_new_features) = c("session_id","is_click")
write.csv(submission_gbm_new_features, "D:/Career Development/Analytics Vidhya/American Express/Submissions/submission_2_GBM_new_features.csv", row.names = F)
# LB score of 0.58 as well.
# Let us try other algorithms

# Naive Bayes

nb.model <- h2o.naiveBayes(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                           validation_frame = validation.h20, seed = 123, nfolds = 3,
                           keep_cross_validation_predictions = T)
summary(nb.model)
# AUC of 0.57

# Random Forest

rf.model <- h2o.randomForest(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                           validation_frame = validation.h20, seed = 123,
                           nfolds = 3, ntrees = 100, keep_cross_validation_predictions = T)
summary(rf.model)
# AUC of 0.57. However the RF model is badly overfitting as indicated by the AUC on training data
ensemble <- h2o.stackedEnsemble(x = x.indep, y = y.dep,
                                training_frame = train.h2o, validation_frame = validation.h20,
                                model_id = "my_ensemble_binomial",
                                base_models = list(gbm.model, nb.model, rf.model))
summary(ensemble)

# Let us try adding some new features based on interaction of users with products, campaigns and web pages
product_interaction <- train_data %>% group_by(user_id, product) %>% summarise(Num_product_interactions = n())
campaign_interaction <- train_data %>% group_by(user_id, campaign_id) %>% summarise(Num_campaign_interactions = n())
web_page_interaction <- train_data %>% group_by(user_id, webpage_id) %>% summarise(Num_web_page_interactions = n())

# Add to train and test data
train_data <- left_join(train_data, product_interaction)
train_data <- left_join(train_data, campaign_interaction)
train_data <- left_join(train_data, web_page_interaction)

test_data <- left_join(test_data, product_interaction)
test_data <- left_join(test_data, campaign_interaction)
test_data <- left_join(test_data, web_page_interaction)

test_data$Num_campaign_interactions[is.na(test_data$Num_campaign_interactions)] = 0
test_data$Num_product_interactions[is.na(test_data$Num_product_interactions)] = 0
test_data$Num_web_page_interactions[is.na(test_data$Num_web_page_interactions)] = 0

rm(product_interaction, campaign_interaction, web_page_interaction)

# Again transfer data to the h2o cluster
train_full.h2o <- as.h2o(train_data %>% select(-session_id, -user_id))
test.h2o <- as.h2o(test_data)

# Run a GBMmodel with default params
colnames(train_full.h2o)
y.dep = 15
x.indep = c(c(1:14),16:18)

gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123, 
                     keep_cross_validation_predictions = T)
summary(gbm.model)
h2o.varimp(gbm.model)
# AUC of 0.58 on validation_data

# Use the entire train data to make model and test LB score
train_full.h2o <- as.h2o(train_data)
gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                     validation_frame = validation.h20, stopping_metric = "AUC",
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123, 
                     keep_cross_validation_predictions = T)
# Predict on test data
predictions_gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Submission 2- GBM with new features
submission_gbm_new_features <- as.data.frame(cbind(as.integer(as.character(test_data$session_id)), as.character(predictions_gbm$Yes)))
colnames(submission_gbm_new_features) = c("session_id","is_click")
write.csv(submission_gbm_new_features, "D:/Career Development/Analytics Vidhya/American Express/Submissions/submission_3_GBM_new_features.csv", row.names = F)
# LB score of 0.608 and CV score of 0.614. Let us try to improve features further

# Interactions for test data. We will add it to existing test_data. 
product_interaction_test <- test_data %>% group_by(user_id, product) %>% summarise(Num_product_interactions_test = n())
campaign_interaction_test <- test_data %>% group_by(user_id, campaign_id) %>% summarise(Num_campaign_interactions_test = n())
web_page_interaction_test <- test_data %>% group_by(user_id, webpage_id) %>% summarise(Num_web_page_interactions_test = n())

test_data <- left_join(test_data, product_interaction_test)
test_data <- left_join(test_data, campaign_interaction_test)
test_data <- left_join(test_data, web_page_interaction_test)

test_data$Num_campaign_interactions <- test_data$Num_campaign_interactions + test_data$Num_campaign_interactions_test
test_data$Num_product_interactions <- test_data$Num_product_interactions + test_data$Num_product_interactions_test
test_data$Num_web_page_interactions <- test_data$Num_web_page_interactions + test_data$Num_web_page_interactions_test

test_data <- test_data %>% select(-Num_campaign_interactions_test, -Num_product_interactions_test, -Num_web_page_interactions_test)

rm(product_interaction_test, campaign_interaction_test, web_page_interaction_test)

test.h2o <- as.h2o(test_data)

# Predict on test data
predictions_gbm_new <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Submission 3- GBM improved interaction features
submission_gbm_new_improved_features <- as.data.frame(cbind(as.integer(as.character(test_data$session_id)), as.character(predictions_gbm_new$Yes)))
colnames(submission_gbm_new_improved_features) = c("session_id","is_click")
write.csv(submission_gbm_new_improved_features, "D:/Career Development/Analytics Vidhya/American Express/Submissions/submission_4_GBM_new_improved_features.csv", row.names = F)
# LB score of 0.625.
# So the GBM is our best model. Let us now try to tune it for the best parameters

hyper_params = list( learn_rate = c(0.01, 0.05, 0.09, 0.1, 0.3, 0.5))
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## which algorithm to run
  algorithm="gbm",
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  ## standard model parameters
  x = x.indep,
  y = y.dep,
  training_frame = train_full.h2o,
  ## fix a random number generator seed for reproducibility
  seed = 123,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10
)
grid
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)
sortedGrid

# So our original depth of 0.1 is already the best model

# Final Model
gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train_full.h2o,
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123, 
                     keep_cross_validation_predictions = T)
# Predict on test data
predictions_gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
#### The End ####