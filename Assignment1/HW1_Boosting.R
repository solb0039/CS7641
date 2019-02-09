# CS7641 Assignment 1

library(tidyverse)
library(readxl)
library(caret)
library(adabag)

#AdaBoost of Credit dataset

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/")
df<-read_excel("./default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
# Change factor encodings
df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1")) %>% select(-one_of("ID"))
xnam<-paste(colnames(df)[2:(ncol(df)-1)], sep="") 
ynam<-"default"
fmla <- as.formula(paste( paste(ynam, " ~ "), paste(xnam, collapse= "+")))

# Create test and training datasets
train_rows <- createDataPartition(y = df$default,p = 0.75,list = FALSE)
train_data <- df[train_rows,]
test_data <- df[-train_rows,]

#Checking distibution in origanl data and partitioned data (really should do chisq test)
ifelse( (prop.table(table(train_data$default)) * 100 - prop.table(table(test_data$default)) * 100) < 0.001, print("Good data partitions"), print("Data not sampled evenly"))

# Train model by varying # neighbors and predict test data set
# Create matrix to hold training grid and results
results<- matrix(0, nrow=24, ncol=4)
colnames(results)<-c("nIter", "cp", "TrainError", "TestError")
results[,1]<-c(rep(1,4),rep(20,4),rep(40,4),rep(60,4),rep(80,4),rep(100,4))
results[,2]<-c(rep(c(0.01,0.001,0.0001,0.0),6))

# Iterate through training grid to build boosted tree model and predict testing data
for (i in 1:nrow(results)){
  print(i)
  tree_control<-rpart.control(cp=results[i,2])
  boost<-boosting(formula=fmla, data=as.data.frame(train_data), boos = FALSE, mfinal = results[i,1], control=tree_control)
  # Train error
  bb<-predict.boosting(boost, newdata=as.data.frame(train_data))
  results[i,3]<-(1-bb$error)*100
  # Test error
  bbt<-predict.boosting(boost, newdata=as.data.frame(test_data))
  results[i,4]<-(1-bbt$error)*100
}

# Plot results together
par(mfrow=c(2,2))
plot( (filter(as.data.frame(results2), cp==0.01))$nIter, (filter(as.data.frame(results2), cp==0.01))$TrainError, main="Train/Test, cp=0.01", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results2), cp==0.01))$nIter, (filter(as.data.frame(results2), cp==0.01))$TrainError, col="green" )
points( (filter(as.data.frame(results2), cp==0.01))$nIter, (filter(as.data.frame(results2), cp==0.01))$TestError )
lines( (filter(as.data.frame(results2), cp==0.01))$nIter, (filter(as.data.frame(results2), cp==0.01))$TestError, col="green" )

plot( (filter(as.data.frame(results2), cp==0.001))$nIter, (filter(as.data.frame(results2), cp==0.001))$TrainError, main="Train/Test, cp=0.001", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results2), cp==0.001))$nIter, (filter(as.data.frame(results2), cp==0.001))$TrainError, col="orange" )
points( (filter(as.data.frame(results2), cp==0.001))$nIter, (filter(as.data.frame(results2), cp==0.001))$TestError )
lines( (filter(as.data.frame(results2), cp==0.001))$nIter, (filter(as.data.frame(results2), cp==0.001))$TestError, col="orange" )

plot( (filter(as.data.frame(results2), cp==0.0001))$nIter, (filter(as.data.frame(results2), cp==0.0001))$TrainError, main="Train/Test, cp=0.0001", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results2), cp==0.0001))$nIter, (filter(as.data.frame(results2), cp==0.0001))$TrainError, col="red" )
points( (filter(as.data.frame(results2), cp==0.0001))$nIter, (filter(as.data.frame(results2), cp==0.0001))$TestError )
lines( (filter(as.data.frame(results2), cp==0.0001))$nIter, (filter(as.data.frame(results2), cp==0.0001))$TestError, col="red" )

plot( (filter(as.data.frame(results2), cp==0.0))$nIter, (filter(as.data.frame(results2), cp==0.0))$TrainError, main="Train/Test, cp=0.0", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results2), cp==0.0))$nIter, (filter(as.data.frame(results2), cp==0.0))$TrainError, col="blue" )
points( (filter(as.data.frame(results2), cp==0.0))$nIter, (filter(as.data.frame(results2), cp==0.0))$TestError )
lines( (filter(as.data.frame(results2), cp==0.0))$nIter, (filter(as.data.frame(results2), cp==0.0))$TestError, col="blue" )



# Repeat with Census dataset

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/") # Change wd to dataset path
income<-read.table("adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))
fmla2 <- as.formula( paste( paste("income", " ~ "), paste(colnames(income)[1:14], collapse= "+")))

# Create test and training datasets
inc_train_rows <- createDataPartition(y = income$income  ,p = 0.75,list = FALSE)
inc_train_data <- income[inc_train_rows,]
inc_test_data <- income[-inc_train_rows,]

# Check to see if test / train distributions are within 0.1% 
ifelse( (prop.table(table(inc_train_data$income)) * 100) - (prop.table(table(inc_test_data$income)) * 100) <0.001, print("Good data partitions"), print("Data not sampled evenly"))

# Train model by varying # neighbors and predict test data set
# Create matrix to hold training grid and results
results<- matrix(0, nrow=24, ncol=4)
colnames(results)<-c("nIter", "cp", "TrainError", "TestError")
results[,1]<-c(rep(1,4),rep(20,4),rep(40,4),rep(60,4),rep(80,4),rep(100,4))
results[,2]<-c(rep(c(0.01,0.001,0.0001,0.0),6))

# Iterate through training grid to build boosted tree model and predict testing data
for (i in 1:nrow(results)){
  print(i)
  tree_control<-rpart.control(cp=results[i,2])
  boost<-boosting(formula=fmla2, data=as.data.frame(inc_train_data), boos = FALSE, mfinal = results[i,1], control=tree_control)
  # Train error
  bb<-predict.boosting(boost, newdata=as.data.frame(inc_train_data))
  results[i,3]<-(1-bb$error)*100
  # Test error
  bbt<-predict.boosting(boost, newdata=as.data.frame(inc_test_data))
  results[i,4]<-(1-bbt$error)*100
}

# Plot results together
par(mfrow=c(2,2))
plot( (filter(as.data.frame(results4), cp==0.01))$nIter, (filter(as.data.frame(results4), cp==0.01))$TrainError, main="Train/Test, cp=0.01", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results4), cp==0.01))$nIter, (filter(as.data.frame(results4), cp==0.01))$TrainError, col="green" )
points( (filter(as.data.frame(results4), cp==0.01))$nIter, (filter(as.data.frame(results4), cp==0.01))$TestError )
lines( (filter(as.data.frame(results4), cp==0.01))$nIter, (filter(as.data.frame(results4), cp==0.01))$TestError, col="green" )

plot( (filter(as.data.frame(results4), cp==0.001))$nIter, (filter(as.data.frame(results4), cp==0.001))$TrainError, main="Train/Test, cp=0.001", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results4), cp==0.001))$nIter, (filter(as.data.frame(results4), cp==0.001))$TrainError, col="orange" )
points( (filter(as.data.frame(results4), cp==0.001))$nIter, (filter(as.data.frame(results4), cp==0.001))$TestError )
lines( (filter(as.data.frame(results4), cp==0.001))$nIter, (filter(as.data.frame(results4), cp==0.001))$TestError, col="orange" )

plot( (filter(as.data.frame(results4), cp==0.0001))$nIter, (filter(as.data.frame(results4), cp==0.0001))$TrainError, main="Train/Test, cp=0.0001", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results4), cp==0.0001))$nIter, (filter(as.data.frame(results4), cp==0.0001))$TrainError, col="red" )
points( (filter(as.data.frame(results4), cp==0.0001))$nIter, (filter(as.data.frame(results4), cp==0.0001))$TestError )
lines( (filter(as.data.frame(results4), cp==0.0001))$nIter, (filter(as.data.frame(results4), cp==0.0001))$TestError, col="red" )

plot( (filter(as.data.frame(results4), cp==0.0))$nIter, (filter(as.data.frame(results4), cp==0.0))$TrainError, main="Train/Test, cp=0.0", ylab="Accurracy, %", xlab="# Iterations", ylim=c(75,100) )
lines( (filter(as.data.frame(results4), cp==0.0))$nIter, (filter(as.data.frame(results4), cp==0.0))$TrainError, col="blue" )
points( (filter(as.data.frame(results4), cp==0.0))$nIter, (filter(as.data.frame(results4), cp==0.0))$TestError )
lines( (filter(as.data.frame(results4), cp==0.0))$nIter, (filter(as.data.frame(results4), cp==0.0))$TestError, col="blue" )


