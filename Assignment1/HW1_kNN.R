# CS7641 Assignment 1
# k-Nearest Neighbors

library(tidyverse)
library(readxl)
library(caret)

# k-NN of income dataset 

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/")
df<-read_excel("./default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
# Change factor encodings
df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1")) %>% select(-one_of("ID"))

# Create test and training datasets
set.seed(101) # Set Seed so that same sample can be reproduced in future also
train_rows <- createDataPartition(y = df$default,p = 0.75,list = FALSE)
train_data <- df[train_rows,]
test_data <- df[-train_rows,]

#Checking distibution in origanl data and partitioned data
prop.table(table(train_data$default)) * 100
prop.table(table(test_data$default)) * 100
prop.table(table(df$default)) * 100

# Train model by varying # neighbors and predict test data set
set.seed(400)
ctrl <- trainControl(method="repeatedcv", repeats = 3)
knnFit <- train(default ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit, main="Accuracy of Income Data to Predict Default vs # Neighbors")
knnPredict <- predict(knnFit, newdata = test_data )
confusionMatrix(knnPredict, test_data$default)


# Vary training dataset size at fixed number of neighbors (27)
samp_size<-seq(0.50, 0.99, 0.01)
results<-matrix(nrow=length(samp_size) ,ncol=3)
colnames(results)<-c("Train Size", "Train Accuracy", "Test Accuracy")

for (trial in seq(1:length(samp_size))){
  print(trial)
  train_rows <- createDataPartition(y = df$default,p = samp_size[trial],list = FALSE)
  train_data <- df[train_rows,]
  test_data <- df[-train_rows,]
  ctrl <- trainControl(method="repeatedcv", repeats = 3)
  knnFit <- train(default ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid=expand.grid(k=c(27)))
  results[trial,1]<-samp_size[trial]
  preds<-predict(knnFit, newdata = train_data)
  rst<-table(train_data$default, preds)
  results[trial,2]<-(rst[1]+rst[4])/sum(rst)
  knnPredict <- predict(knnFit, newdata = test_data )
  rstt<-table(test_data$default, knnPredict)
  results[trial,3]<-(rstt[1]+rstt[4])/sum(rstt)
}

# Plot results of sample size
results<-as.data.frame(results)
ggplot(data=results) +
  geom_point(mapping = aes(x=`Train Size`, y=`Train Accuracy`, col='blue')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Train Accuracy`, col='blue')) +
  geom_point(mapping = aes(x=`Train Size`, y=`Test Accuracy`, color='red')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Test Accuracy`, color='red')) +
  labs(title="Accuracy vs. Training Set Size\n", y="Accuracy", color="") +
  scale_color_manual(labels=c("Train", "Test"), values=c("blue", "red"))




# Repeat k-NN with Census dataset
# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/") # Change wd to dataset path
income<-read.table("adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))
# Remove Holand since there is only 1 obs
income<-income[-which(income$country == ' Holand-Netherlands'),]
income$country <- factor(income$country) 

# Create test and training datasets
set.seed(500) # Set Seed so that same sample can be reproduced in future also
inc_train_rows <- createDataPartition(y = income$income  ,p = 0.75,list = FALSE)
inc_train_data <- income[inc_train_rows,]
inc_test_data <- income[-inc_train_rows,]

#Checking distibution in origanl data and partitioned data
prop.table(table(inc_train_data$income)) * 100
prop.table(table(inc_test_data$income)) * 100
prop.table(table(income$income)) * 100

# Check to see if test / train distributions are within 0.1% 
ifelse( (prop.table(table(inc_train_data$income)) * 100) - (prop.table(table(inc_test_data$income)) * 100) <0.001, print("Good data partitions"), print("Data not sampled evenly"))

# Train model by varying # neighbors and predict test data set
set.seed(1400)
inc_ctrl <- trainControl(method="repeatedcv", repeats = 3)
inc_knnFit <- train(income ~ ., data = inc_train_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 15)
plot(inc_knnFit, main="Accuracy of Census Data to Predict Income vs # Neighbors")
inc_knnPredict <- predict(inc_knnFit, newdata = inc_test_data )
confusionMatrix(inc_knnPredict, inc_test_data$income)