# CS7641 Assignment 1
# Decision Tress #

library(tidyverse)
library(readxl)
library(rpart)

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/")
df<-read_excel("./default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
xnam<-paste(colnames(df)[2:(ncol(df)-1)], sep="") 
ynam<-"default"
fmla <- as.formula(paste( paste(ynam, " ~ "), paste(xnam, collapse= "+")))

df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1"))
set.seed(101) # Set Seed so that same sample can be reproduced in future also


# Vary sample size at fixed Cp
samp_size<-seq(0.50, 0.99, 0.01)
results<-matrix(nrow=length(samp_size) ,ncol=3)
colnames(results)<-c("Train Size", "Train Error", "Test Error")
for (trial in seq(1:length(samp_size))){
  print(trial)
  results[trial,1]<-samp_size[trial]
  # Create test and training datasets
  set.seed(1000) # Set Seed so that same sample can be reproduced in future also
  sample <- sample.int(n = nrow(df), size = floor(samp_size[trial]*nrow(df)), replace = F)
  train_data <- df[sample, ]
  test_data  <- df[-sample, ]
  
  # 10x cross-validation on entire tree
  model<-rpart(fmla, data=train_data, method="class", xval=10) 
  
  # Confusion matrix
  preds<-predict(model, newdata = train_data, type="class")
  rst<-table(train_data$default, preds)
  results[trial,2]<-(rst[1]+rst[4])/sum(rst)
  
  # Fit test data
  pred_test<-predict(model, newdata = test_data, type="class")
  rstt<-table(test_data$default, pred_test)
  results[trial,3]<-(rstt[1]+rstt[4])/sum(rstt)
}
# Plot results of sample size
results<-as.data.frame(results)
ggplot(data=results) +
  geom_point(mapping = aes(x=`Train Size`, y=`Train Error`, col='blue')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Train Error`, col='blue')) +
  geom_point(mapping = aes(x=`Train Size`, y=`Test Error`, color='red')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Test Error`, color='red')) +
  labs(title="Accuracy vs. Training Set Size\n", y="Accuracy", color="") +
  scale_color_manual(labels=c("Train", "Test"), values=c("blue", "red"))



# Create test and training datasets at optimal sample size
set.seed(101) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(df), size = floor(.9*nrow(df)), replace = F)
train_data <- df[sample, ]
test_data  <- df[-sample, ]

# 10x cross-validation on entire tree
model<-rpart(fmla, data=train_data, method="class", control=rpart.control(cp = 0.000), xval=10) 
plotcp(model)

# Find optimal tree
min_error<-which.min(model$cptable[,4])
min_cp<-model$cptable[min_error,1]
min_tree<-model$cptable[min_error,2]
abline(v=min_tree)

# Prune tree to optimal cp
model_prune<-prune.rpart(model, cp=min_cp)
plot(model_prune, branch=0.3, compress=T, margin=0.1)
text(model_prune, cex=0.7)

# Confusion matrix
preds<-predict(model_prune, newdata = train_data, type="class")
confusionMatrix(train_data$default, preds)

# Fit test data
pred_test<-predict(model_prune, newdata = test_data, type="class")
confusionMatrix(test_data$default, pred_test)



############################
# Repeat for census dataset#
############################
income<-read.table("./adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))

# Define formula for ease of use        
fmla2 <- as.formula( paste( paste("income", " ~ "), paste(colnames(income)[1:14], collapse= "+")))

# Vary sample size at fixed Cp
samp_size<-seq(0.50, 0.99, 0.01)
results<-matrix(nrow=length(samp_size) ,ncol=3)
colnames(results)<-c("Train Size", "Train Error", "Test Error")
for (trial in seq(1:length(samp_size))){
  print(trial)
  results[trial,1]<-samp_size[trial]
  # Create test and training datasets
  set.seed(1000) # Set Seed so that same sample can be reproduced in future also
  sample <- sample.int(n = nrow(income), size = floor(samp_size[trial]*nrow(income)), replace = F)
  income_train <- income[sample, ]
  income_test  <- income[-sample, ]
  
  # 10x cross-validation on entire tree
  model<-rpart(fmla2, data=income_train, method="class", xval=10) 
  
  # Confusion matrix
  preds<-predict(model, newdata = income_train, type="class")
  rst<-table(income_train$income, preds)
  results[trial,2]<-(rst[1]+rst[4])/sum(rst)
  
  # Fit test data
  pred_test<-predict(model, newdata = income_test, type="class")
  rstt<-table(income_test$income, pred_test)
  results[trial,3]<-(rstt[1]+rstt[4])/sum(rstt)
}

# Plot results of sample size
results<-as.data.frame(results)
ggplot(data=results) +
  geom_point(mapping = aes(x=`Train Size`, y=`Train Error`, col='blue')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Train Error`, col='blue')) +
  geom_point(mapping = aes(x=`Train Size`, y=`Test Error`, color='red')) +
  geom_smooth(mapping = aes(x=`Train Size`, y=`Test Error`, color='red')) +
  labs(title="Accuracy vs. Training Set Size\n", y="Accuracy", color="") +
  scale_color_manual(labels=c("Train", "Test"), values=c("blue", "red"))



# Create test and training datasets at optimal size
sample <- sample.int(n = nrow(income), size = floor(.70*nrow(income)), replace = F)
income_train <- income[sample, ]
income_test  <- income[-sample, ]

# 10x cross-validation on entire tree
census_tree<-rpart(fmla2, data=income_train, method="class", control=rpart.control(cp = 0.000), xval=10)
plotcp(census_tree)

# Find optimal tree
min_cen_error<-which.min(census_tree$cptable[,4])
min_cen_cp<-census_tree$cptable[min_cen_error,1]
min_cen_tree<-census_tree$cptable[min_cen_error,2]
abline(v=min_cen_tree)

# Prune tree to optimal cp
model_cen_prune<-prune.rpart(census_tree, cp=min_cen_cp)
plot(model_cen_prune, branch=0.3, compress=T, margin=0.1)
text(model_cen_prune, cex=0.7)

# Confusion matrix
preds_cen<-predict(model_cen_prune, newdata = income_train, type="class")
confusionMatrix(income_train$income, preds_cen)

# Fit test data
pred_cen_test<-predict(model_cen_prune, newdata = income_test, type="class")
confusionMatrix(income_test$income, pred_cen_test)
