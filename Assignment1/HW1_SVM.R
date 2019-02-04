library(tidyverse)
library(readxl)
library(caret)
library(e1071)

#SVM of Credit dataset

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/")
df<-read_excel("./default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
# Change factor encodings for `default`
df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1")) %>% select(-one_of("ID"))
xnam<-paste(colnames(df)[2:(ncol(df)-1)], sep="") 
ynam<-"default"
fmla <- as.formula(paste( paste(ynam, " ~ "), paste(xnam, collapse= "+")))

for (i in 1:3){
# Create test and training datasets
train_rows <- createDataPartition(y = df$default,p = 0.75,list = FALSE)
train_data <- df[train_rows,]
test_data <- df[-train_rows,]

#Checking distibution in origanl data and partitioned data (really should do chisq test)
ifelse( (prop.table(table(train_data$default)) * 100 - prop.table(table(test_data$default)) * 100) < 0.001, print("Good data partitions"), print("Data not sampled evenly"))

# SVM with polynomial kernal
cred_svm_poly<-svm(formula=fmla, data=train_data, kernal=polynomial, degree=3)
tr<-table(predict(cred_svm_poly, newdata = train_data), train_data$default)
print((tr[1]+tr[4]) / (tr[1]+tr[2]+tr[3]+tr[4])*100)
tst<-table(predict(cred_svm_poly, newdata = test_data), test_data$default)
print((tst[1]+tst[4]) / (tst[1]+tst[2]+tst[3]+tst[4])*100)
}


# SVM with radial kernal 
for (i in 1:3){
  # Create test and training datasets
  train_rows <- createDataPartition(y = df$default,p = 0.75,list = FALSE)
  train_data <- df[train_rows,]
  test_data <- df[-train_rows,]
  
  cred_svm_radial<-svm(formula=fmla, data=train_data, kernal=radial, gamma=2)
  tr<-table(predict(cred_svm_radial, newdata = train_data), train_data$default)
  print((tr[1]+tr[4]) / (tr[1]+tr[2]+tr[3]+tr[4])*100)
  tst<-table(predict(cred_svm_radial, newdata = test_data), test_data$default)
  print((tst[1]+tst[4]) / (tst[1]+tst[2]+tst[3]+tst[4])*100)
}




# SVM with Census data set

# Read data, pre-process, and split
setwd("~/School/GeorgiaTech/CS7641/Assignment1/") # Change wd to dataset path
income<-read.table("adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))
fmla2 <- as.formula( paste( paste("income", " ~ "), paste(colnames(income)[1:14], collapse= "+")))

for (i in 1:3){
  # Create test and training datasets
  inc_train_rows <- createDataPartition(y = income$income  ,p = 0.75,list = FALSE)
  inc_train_data <- income[inc_train_rows,]
  inc_test_data <- income[-inc_train_rows,]

  # Check to see if test / train distributions are within 0.1% 
  ifelse( (prop.table(table(inc_train_data$income)) * 100) - (prop.table(table(inc_test_data$income)) * 100) <0.001, print("Good data partitions"), print("Data not sampled evenly"))

  # SVM with linear kernal
  census_svm_poly<-svm(formula=fmla2, data=inc_train_data, kernal=polynomial, degree=3)
  tr<-table(predict(census_svm_poly, newdata = inc_train_data), inc_train_data$income)
  print((tr[1]+tr[4]) / (tr[1]+tr[2]+tr[3]+tr[4])*100)
  tst<-table(predict(census_svm_poly, newdata = inc_test_data), inc_test_data$income)
  print((tst[1]+tst[4]) / (tst[1]+tst[2]+tst[3]+tst[4])*100)
}


# SVM with radial kernal 
for (i in 1:3){
  # Create test and training datasets
  inc_train_rows <- createDataPartition(y = income$income  ,p = 0.75,list = FALSE)
  inc_train_data <- income[inc_train_rows,]
  inc_test_data <- income[-inc_train_rows,]
  
  census_svm_radial<-svm(formula=fmla2, data=inc_train_data, kernal=radial, degree=2)
  tr<-table(predict(census_svm_radial, newdata = inc_train_data), inc_train_data$income)
  print((tr[1]+tr[4]) / (tr[1]+tr[2]+tr[3]+tr[4])*100)
  tst<-table(predict(census_svm_radial, newdata = inc_test_data), inc_test_data$income)
  print((tst[1]+tst[4]) / (tst[1]+tst[2]+tst[3]+tst[4])*100)
}