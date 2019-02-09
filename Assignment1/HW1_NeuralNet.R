# CS7641 Assignment 1
##NEURAL NET###

library(tidyverse)
library(readxl)
library(caret)
library(nnet)
library(NeuralNetTools)

# Read, process and split dataset
setwd("~/School/GeorgiaTech/CS7641/Assignment1/")
df<-read_excel("./default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
xnam<-paste(colnames(df)[2:(ncol(df)-1)], sep="") 
ynam<-"default"
fmla <- as.formula(paste( paste(ynam, " ~ "), paste(xnam, collapse= "+")))

df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1"))

# Create test and training datasets
set.seed(101) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train_data <- df[sample, ]
test_data  <- df[-sample, ]

#center and scale
process_train<-preProcess(train_data, method = c("center", "scale"))
process_test<-preProcess(test_data, method = c("center", "scale"))
train_data<-predict(process_train, train_data)
test_data<-predict(process_test, test_data)

# Grid search to tune hyperparameters
nnetGrid<-expand.grid(size = round(seq(from = 0.2*length(xnam), to = 1.25*length(xnam), by = 2)), 
                         decay = seq(from = 0.1, to = 1.0, by = 0.1))

fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 3)
                          # classProbs = TRUE,
                          # summaryFunction = twoClassSummary)


nnetFit <- train(fmla, data = train_data, method = "nnet", 
       tuneGrid = nnetGrid, verbose = TRUE,  trControl=fitControl) #metric = "ROC", 

confusionMatrix(as.factor(ifelse(predict(nnetFit$finalModel, newdata=train_data)>0.5,1,0)), as.factor(ifelse(train_data$default=="NoDefault", 0, 1)))

# Fit the best model from the grid search
nnet_model<-nnet(fmla, size=nnetFit$bestTune$size, decay=nnetFit$bestTune$decay, data=train_data, maxit=5000, softmax=F)

# Predict the test set and create confusion matrix
test_predict<-predict(nnet_model, newdata = test_data)
confusionMatrix(as.factor(ifelse(test_predict>0.5,1,0)), as.factor(ifelse(test_data$default=="NoDefault", 0, 1)))
plotnet(nnet_model)
garson(nnet_model)



# Census dataset analysis #
income<-read.table("./adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))

# Create test and training datasets
sample <- sample.int(n = nrow(income), size = floor(.75*nrow(income)), replace = F)
income_train <- income[sample, ]
income_test  <- income[-sample, ]

#center and scale predictors
processed_income_train<-preProcess(income_train, method = c("center", "scale"))
processed_income_test<-preProcess(income_train, method = c("center", "scale"))
income_train_sc<-predict(processed_income_train, income_train)
income_test_sc<-predict(processed_income_test, income_test)

# Grid search for hyperparameter tuning
fmla2 <- as.formula( paste( paste("income", " ~ "), paste(colnames(income)[1:14], collapse= "+")))
nnetGrid2<-expand.grid(size = round(seq(from = 0.1*length(colnames(income_train_sc)), to = 0.5*length(colnames(income_train_sc)), by = 2)), 
                      decay = seq(from = 0.1, to = 1.0, by = 0.2))
fitControl2 <- trainControl(method = "repeatedcv", number = 5,  repeats = 3)

start<-Sys.time()
nnetFit2 <- train(fmla2, data = income_train_sc, method = "nnet", maxit=1000, 
                 tuneGrid = nnetGrid2, verbose = TRUE, trControl=fitControl2)  
print(Sys.time()-start)

# Best model matrix
confusionMatrix(as.factor(ifelse(predict(nnetFit2$finalModel, newdata=income_train_sc)>0.5,1,0)), as.factor(ifelse(income_train_sc$income=="<=50K",0,1)))

# Fit best model
nnet_model2<-nnet(fmla2, size=nnetFit2$bestTune$size, decay=nnetFit2$bestTune$decay, data=income_train_sc, maxit=5000, softmax=F)

# Predict the test set and create confusion matrix
income_predict<-predict(nnet_model2, newdata = income_test_sc)
confusionMatrix(as.factor(ifelse(income_predict>0.5,1,0)), as.factor(ifelse(income_test_sc$income=="<=50K",0,1)))
plotnet(nnet_model2)
a<-garson(nnet_model2)

# Timing experiment
res<-array(0, dim=c(9,11))
for (i in 1:9){
  for (j in 0:10){
    start<-Sys.time()
    test_model<-nnet(fmla2, size=i, decay=j/10, data=income_train_sc, maxit=5000, softmax=F)
    res[i,j]<-Sys.time()-start
  }
}
colnames(res)<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
rownames(res)<-c(1,2,3,4,5,6,7,8,9)
plot(res[,1])




