# TREES #
library(tidyverse)
library(readxl)
library(rpart)

# Read data, pre-process, and split
df<-read_excel("~/Downloads/default of credit card clients.xls")
colnames(df)[ncol(df)]<-"default"
df$default<-as.factor(df$default)
xnam<-paste(colnames(df)[2:(ncol(df)-1)], sep="") 
ynam<-"default"
fmla <- as.formula(paste( paste(ynam, " ~ "), paste(xnam, collapse= "+")))

df<- df %>% mutate(default = fct_recode(default, "NoDefault" = "0", "Default"="1"))
set.seed(101) # Set Seed so that same sample can be reproduced in future also

# Create test and training datasets
set.seed(101) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
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



# Repeat for census dataset
income<-read.table("~/Downloads/adult.data.txt", header=F, sep=",")
income_names<-c("age","workclass","fnlwgt","education","education_num","marital", "occupation",
                "relationship","race","sex","capitalgain","capitalloss","hoursperweek",
                "country","income")
colnames(income)<-income_names
income<- income %>% mutate(income = fct_recode(income, "<=50K" = " <=50K", ">50K"=" >50K"))

# Create test and training datasets
sample <- sample.int(n = nrow(income), size = floor(.75*nrow(income)), replace = F)
income_train <- income[sample, ]
income_test  <- income[-sample, ]

# Define formula for ease of use        
fmla2 <- as.formula( paste( paste("income", " ~ "), paste(colnames(income)[1:14], collapse= "+")))

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
#86.74%

# Fit test data
pred_cen_test<-predict(model_cen_prune, newdata = income_test, type="class")
confusionMatrix(income_test$income, pred_cen_test)
#85.36%