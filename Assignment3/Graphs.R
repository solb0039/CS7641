library(tidyverse)
library(ggplot2)

setwd("/Users/Sean/School/GeorgiaTech/CS7641/Assignment3/")


# PLOT OF PCA ON ADULT DATASET
pca_adult<-readr::read_csv("./P2_Dimensionality_Reduction/Adult_PCA_explained_variance_ratio.csv", col_names=FALSE)

# Sum the variance explained
pca_adult$sum[1]=pca_adult[1,2]
for(i in (2:nrow(pca_adult))){
  pca_adult$sum[i]<-pca_adult$sum[i-1]+pca_adult[i,2]
}

# Nnet data
adult_nnet_base<-readr::read_csv("./P4_Neural_Networks_Reduced/Adult_Base_nn_results.csv", col_names=TRUE)
pca_adult_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Adult_PCA_nn_results.csv", col_names=TRUE)
a<-pca_adult_nnet%>%group_by(param_clf__n_components)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

pca_adult_data<-as.data.frame(cbind(pca_adult$X2, as.numeric(pca_adult$sum), as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(pca_adult_data)<-c('num', 'sum', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )
adult_nnet_ref<-mean(adult_nnet_base$mean_test_score)

ggplot(data=pca_adult_data)+geom_point(mapping=aes(x=param_clf__n_components, y=sum, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=sum, color='red')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=adult_nnet_ref))+
  geom_text(aes(0, adult_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of PCA Components", y="Percent", title="PCA of Adult Dataset\n", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Tot. Var. Explained"), values = c("red", "green", "blue"))+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# PLOT OF PCA ON CREDIT DATASET
pca_credit<-readr::read_csv("./P2_Dimensionality_Reduction/Credit_PCA_explained_variance_ratio.csv", col_names=FALSE)
# Sum the variance explained
pca_credit$sum[1]=pca_credit[1,2]
for(i in (2:nrow(pca_credit))){
  pca_credit$sum[i]<-pca_credit$sum[i-1]+pca_credit[i,2]
}
# Nnet data
credit_nnet_base<-readr::read_csv("./P4_Neural_Networks_Reduced/Credit_Base_nn_results.csv", col_names=TRUE)
pca_credit_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Credit_PCA_nn_results.csv", col_names=TRUE)
ac<-pca_credit_nnet%>%group_by(param_clf__n_components)
bc<-ac%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
bc<-cbind(bc,tally(ac))
bc$ci_plus<-bc$mean+(bc$sd/sqrt(bc$n))*1.96
bc$ci_minus<-bc$mean-(bc$sd/sqrt(bc$n))*1.96

pca_credit_data<-as.data.frame(cbind(pca_credit$X2, as.numeric(pca_credit$sum), as.numeric(bc$param_clf__n_components), as.numeric(bc$mean), as.numeric(bc$max), as.numeric(bc$ci_plus), as.numeric(bc$ci_minus) ))
colnames(pca_credit_data)<-c('num', 'sum', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )
credit_nnet_ref<-mean(credit_nnet_base$mean_test_score)

ggplot(data=pca_credit_data)+geom_point(mapping=aes(x=param_clf__n_components, y=sum, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=sum, color='red')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of PCA Components", y="Percent", title="PCA of Credit Dataset", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Tot. Var. Explained"), values = c("red", "green", "blue")) +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# PCA COMPONENT INTERPRETATION
credit_data<-readr::read_csv("./default of credit card clients2.csv", col_names=FALSE)
colnames(credit_data)<-c('LIMIT_BAL','SEX','EDUCATION','MARRIAGE','AGE','PAY_0','PAY_2','PAY_3','PAY_4','PAY_5','PAY_6',
                         'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6','PAY_AMT1','PAY_AMT2','PAY_AMT3',
                         'PAY_AMT4','PAY_AMT5','PAY_AMT6','default')
pc<-princomp(credit_data[,1:(ncol(credit_data)-1)])

adult_data<-readr::read_csv("./adult_dum_vars.csv", col_names=TRUE)
pca<-princomp(adult_data[,1:(ncol(adult_data)-1)])


# PLOT OF ICA ON ADULT DATASET 
# Kurtosis and NNet test accuracy vs no components
ica_adult<-readr::read_csv("./P2_Dimensionality_Reduction/Adult_ICA_kurtosis.csv", col_names=FALSE)
ica_adult_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Adult_ICA_nn_results.csv", col_names=TRUE)
a<-ica_adult_nnet%>%group_by(param_clf__n_components)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

ica_adult_data<-as.data.frame(cbind(ica_adult$X2, as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(ica_adult_data)<-c('kurtosis', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )

p1<-ggplot(data=ica_adult_data)+geom_point(mapping=aes(x=param_clf__n_components, y=kurtosis, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=kurtosis, color='red'))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  labs(y="Percent", title="ICA of Adult Dataset\n", y="Kurtosis")+
  scale_color_manual(labels = c("Kurtosis"), values = c("red"))+
  theme(legend.position="right", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

p2<-ggplot(data=ica_adult_data)+geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=adult_nnet_ref))+
  geom_text(aes(0, adult_nnet_ref,label="Reference NNet Model", vjust=1, hjust=0.10)) +
  labs(x="Number of ICA Components", y="Percent") +
  scale_color_manual(labels = c("NNet Test Error","95% CI"), values = c("red", "green", "blue"))+
  theme(legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))



# PLOT OF ICA ON CREDIT DATASET
# Kurtosis and NNet test accuracy vs no components
ica_credit<-readr::read_csv("./P2_Dimensionality_Reduction/Credit_ICA_kurtosis.csv", col_names=FALSE)
ica_credit_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Credit_ICA_nn_results.csv", col_names=TRUE)
a<-ica_credit_nnet%>%group_by(param_clf__n_components)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

ica_credit_data<-as.data.frame(cbind(ica_credit$X2, as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(ica_credit_data)<-c('kurtosis', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )

p1<-ggplot(data=ica_credit_data)+geom_point(mapping=aes(x=param_clf__n_components, y=kurtosis, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=kurtosis, color='red'))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  labs(y="Kurtosis", title="ICA of credit Dataset\n", y="Kurtosis")+
  scale_color_manual(labels = c("Kurtosis"), values = c("red"))+
  theme(legend.position="right", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

p2<-ggplot(data=ica_credit_data)+geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=1, hjust=0.10)) +
  labs(x="Number of ICA Components", y="Test Error") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error"), values = c("red", "green", "blue"))+
  theme(legend.position="right", axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))


# PLOT OF RCA ON ADULT DATASET
# Pairwise distance correlation and nnet test accuracy vs no. components
rca_adult<-readr::read_csv("./P2_Dimensionality_Reduction/Adult_RP_pairwise_distance_corr.csv", col_names=TRUE)
rca_adult_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Adult_RP_nn_results.csv", col_names=TRUE)

a<-rca_adult_nnet%>%group_by(param_clf__n_components)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

rca_adult_data<-as.data.frame(cbind(rca_adult$Average, as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(rca_adult_data)<-c('PairwiseCorr.', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )

ggplot(data=rca_adult_data)+geom_point(mapping=aes(x=param_clf__n_components, y=PairwiseCorr., color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=PairwiseCorr., color='red')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of RCA Components", y="Percent", title="RCA of Adult Dataset", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Pairwise Corr."), values = c("red", "green", "blue")) +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))



# PLOT OF RCA ON CREDIT DATASET
# Pairwise distance correlation and nnet test accuracy vs no. components
rca_credit<-readr::read_csv("./P2_Dimensionality_Reduction/Credit_RP_pairwise_distance_corr.csv", col_names=TRUE)
rca_credit_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Credit_RP_nn_results.csv", col_names=TRUE)

a<-rca_credit_nnet%>%group_by(param_clf__n_components)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

rca_credit_data<-as.data.frame(cbind(rca_credit$Average, as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(rca_credit_data)<-c('PairwiseCorr.', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )

ggplot(data=rca_credit_data)+geom_point(mapping=aes(x=param_clf__n_components, y=PairwiseCorr., color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=PairwiseCorr., color='red')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of RCA Components", y="Percent", title="RCA of Credit Dataset", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Pairwise Corr."), values = c("red", "green", "blue")) +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# PLOT OF RANDOM FOREST ON ADULT DATASET
# Feature Importance, cumulative importance, and NNtest accuracy vs vs. Number of features 
rf_adult<-readr::read_csv("./P2_Dimensionality_Reduction/Adult_RF_feature_importance.csv", col_names=FALSE)
colnames(rf_adult)<-c("No. Features", "Importance", "Sum_Importance")
rf_adult_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Adult_RF_nn_results.csv", col_names=TRUE)
adult_nnet_ref<-max(adult_nnet_base$mean_test_score)
  
a<-rf_adult_nnet%>%group_by(param_filter__n)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

rf_adult_data<-as.data.frame(cbind(rf_adult$Importance, rf_adult$Sum_Importance, as.numeric(b$param_filter__n), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(rf_adult_data)<-c('Importance', 'Sum_Importance', 'param_filter__n', 'mean', 'max', 'ci_plus','ci_minus' )

ggplot(data=rf_adult_data)+geom_point(mapping=aes(x=param_filter__n, y=Sum_Importance, color='red'))+
  geom_line(mapping=aes(x=param_filter__n, y=Sum_Importance, color='red')) +
  geom_point(mapping=aes(x=param_filter__n, y=Importance, color='pink'))+
  geom_line(mapping=aes(x=param_filter__n, y=Importance, color='pink')) +
  geom_point(mapping=aes(x=param_filter__n, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_filter__n, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_filter__n, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_filter__n, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=adult_nnet_ref))+
  geom_text(aes(0, adult_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of Components", y="Percent", title="Random Forest of Adult Dataset", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Importance", "Sum Importance"), values = c("red", "green", "pink", "blue")) +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))



# PLOT OF RANDOM FOREST ON CREDIT DATASET
# Feature Importance, cumulative importance, and NNtest accuracy vs vs. Number of features 
rf_credit<-readr::read_csv("./P2_Dimensionality_Reduction/Credit_RF_feature_importance.csv", col_names=FALSE)
colnames(rf_credit)<-c("No. Features", "Importance", "Sum_Importance")
rf_credit_nnet<-readr::read_csv("./P4_Neural_Networks_Reduced/Credit_RF_nn_results.csv", col_names=TRUE)

a<-rf_credit_nnet%>%group_by(param_filter__n)
b<-a%>%summarise(
  mean=mean(mean_test_score),
  max=max(mean_test_score),
  var=var(mean_test_score),
  sd=sd(mean_test_score)
)
b<-cbind(b,tally(a))
b$ci_plus<-b$mean+(b$sd/sqrt(b$n))*1.96
b$ci_minus<-b$mean-(b$sd/sqrt(b$n))*1.96

rf_credit_data<-as.data.frame(cbind(rf_credit$Importance, rf_credit$Sum_Importance, as.numeric(b$param_filter__n), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(rf_credit_data)<-c('Importance', 'Sum_Importance', 'param_filter__n', 'mean', 'max', 'ci_plus','ci_minus' )

ggplot(data=rf_credit_data)+geom_point(mapping=aes(x=param_filter__n, y=Sum_Importance, color='red'))+
  geom_line(mapping=aes(x=param_filter__n, y=Sum_Importance, color='red')) +
  geom_point(mapping=aes(x=param_filter__n, y=Importance, color='pink'))+
  geom_line(mapping=aes(x=param_filter__n, y=Importance, color='pink')) +
  geom_point(mapping=aes(x=param_filter__n, y=mean, color='green'))+
  geom_line(mapping=aes(x=param_filter__n, y=mean, color='green')) +
  geom_point(mapping=aes(x=param_filter__n, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_filter__n, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of Components", y="Percent", title="Random Forest of Credit Dataset", color = "") +
  scale_color_manual(labels = c("95% CI", "NNet Test Error", "Importance", "Sum Importance"), values = c("red", "green", "pink", "blue")) +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# Transpose datasets
transpose<-function(dataset){
  data_mod<-as.data.frame(t(dataset[,-1]))
  colnames(data_mod)<-t(dataset[,1])
  data_mod$NoClusters<-rownames(data_mod)
  return(data_mod)
}

# CLUSTERING of ADULT DATA NON-DIM REDUCED DATA
adult_MI<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Adult_adjMI.csv")
adult_compl<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Adult_compl.csv")
adult_homo<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Adult_homo.csv")
adult_MI2<-transpose(adult_MI)
adult_compl2<-transpose(adult_compl)
adult_homo2<-transpose(adult_homo)

adult_cluster_gmm<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Adult_Cluster_Select_GMM.csv")
adult_cluster_km<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Adult_Cluster_Select_Kmeans.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(adult_cluster_km$X1, adult_cluster_km$`Adult SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(adult_cluster_gmm$X1, adult_cluster_gmm$`Adult BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(adult_homo2$NoClusters, adult_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(adult_homo2$Kmeans,adult_compl2$Kmeans,adult_MI2$Kmeans),max(adult_homo2$Kmeans,adult_compl2$Kmeans,adult_MI2$Kmeans)))
points(adult_compl2$NoClusters ,adult_compl2$Kmeans, type="l", lwd=3, col="green")
points(adult_MI2$NoClusters ,adult_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=-0.2, y=0.015, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
        cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(adult_homo2$NoClusters ,adult_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(adult_homo2$GMM,adult_compl2$GMM,adult_MI2$GMM),max(adult_homo2$GMM,adult_compl2$GMM,adult_MI2$GMM)))
points(adult_compl2$NoClusters ,adult_compl2$GMM, type="l", lwd=3, col="green")
points(adult_MI2$NoClusters ,adult_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Adult Dataset Clustering",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)



# CLUSTERING of CREDIT DATA NON-DIM REDUCED DATA
credit_MI<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Credit_adjMI.csv")
credit_compl<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Credit_compl.csv")
credit_homo<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Credit_homo.csv")
credit_MI2<-transpose(credit_MI)
credit_compl2<-transpose(credit_compl)
credit_homo2<-transpose(credit_homo)

credit_cluster_gmm<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Credit_Cluster_Select_GMM.csv")
credit_cluster_km<-readr::read_csv("./P1_Clustering_Algorithms_Non_Transformed/Credit_Cluster_Select_Kmeans.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(credit_cluster_km$X1, credit_cluster_km$`Credit SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(credit_cluster_gmm$X1, credit_cluster_gmm$`Credit BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(credit_homo2$NoClusters, credit_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(credit_homo2$Kmeans,credit_compl2$Kmeans,credit_MI2$Kmeans),max(credit_homo2$Kmeans,credit_compl2$Kmeans,credit_MI2$Kmeans)))
points(credit_compl2$NoClusters ,credit_compl2$Kmeans, type="l", lwd=3, col="green")
points(credit_MI2$NoClusters ,credit_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=-0.2, y=0.015, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(credit_homo2$NoClusters ,credit_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(credit_homo2$GMM,credit_compl2$GMM,credit_MI2$GMM),max(credit_homo2$GMM,credit_compl2$GMM,credit_MI2$GMM)))
points(credit_compl2$NoClusters ,credit_compl2$GMM, type="l", lwd=3, col="green")
points(credit_MI2$NoClusters ,credit_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Credit Dataset Clustering",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of PCA ON ADULT DATASET
pca_adult_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_PCA_adjMI.csv")
pca_adult_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_PCA_compl.csv")
pca_adult_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_PCA_homo.csv")
pca_adult_MI2<-transpose(pca_adult_MI)
pca_adult_compl2<-transpose(pca_adult_compl)
pca_adult_homo2<-transpose(pca_adult_homo)

pca_adult_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_BIC_PCA.csv")
pca_adult_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_SSE_PCA.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(pca_adult_cluster_km$X1, pca_adult_cluster_km$`Adult SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(pca_adult_cluster_gmm$X1, pca_adult_cluster_gmm$`Adult BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(pca_adult_homo2$NoClusters, pca_adult_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(pca_adult_homo2$Kmeans,pca_adult_compl2$Kmeans,pca_adult_MI2$Kmeans),max(pca_adult_homo2$Kmeans,pca_adult_compl2$Kmeans,pca_adult_MI2$Kmeans)))
points(pca_adult_compl2$NoClusters ,pca_adult_compl2$Kmeans, type="l", lwd=3, col="green")
points(pca_adult_MI2$NoClusters ,pca_adult_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=-0.2, y=0.01, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(pca_adult_homo2$NoClusters ,pca_adult_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(pca_adult_homo2$GMM,pca_adult_compl2$GMM,pca_adult_MI2$GMM),max(pca_adult_homo2$GMM,pca_adult_compl2$GMM,pca_adult_MI2$GMM)))
points(pca_adult_compl2$NoClusters ,pca_adult_compl2$GMM, type="l", lwd=3, col="green")
points(pca_adult_MI2$NoClusters ,pca_adult_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Adult Dataset Clustering w/ PCA",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of PCA ON CREDIT DATASET
pca_credit_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_PCA_adjMI.csv")
pca_credit_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_PCA_compl.csv")
pca_credit_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_PCA_homo.csv")
pca_credit_MI2<-transpose(pca_credit_MI)
pca_credit_compl2<-transpose(pca_credit_compl)
pca_credit_homo2<-transpose(pca_credit_homo)

pca_credit_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_BIC_PCA.csv")
pca_credit_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_SSE_PCA.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(pca_credit_cluster_km$X1, pca_credit_cluster_km$`Credit SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(pca_credit_cluster_gmm$X1, pca_credit_cluster_gmm$`Credit BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(pca_credit_homo2$NoClusters, pca_credit_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(pca_credit_homo2$Kmeans,pca_credit_compl2$Kmeans,pca_credit_MI2$Kmeans),max(pca_credit_homo2$Kmeans,pca_credit_compl2$Kmeans,pca_credit_MI2$Kmeans)))
points(pca_credit_compl2$NoClusters ,pca_credit_compl2$Kmeans, type="l", lwd=3, col="green")
points(pca_credit_MI2$NoClusters ,pca_credit_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1, y=0.03, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(pca_credit_homo2$NoClusters ,pca_credit_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(pca_credit_homo2$GMM,pca_credit_compl2$GMM,pca_credit_MI2$GMM),max(pca_credit_homo2$GMM,pca_credit_compl2$GMM,pca_credit_MI2$GMM)))
points(pca_credit_compl2$NoClusters ,pca_credit_compl2$GMM, type="l", lwd=3, col="green")
points(pca_credit_MI2$NoClusters ,pca_credit_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Credit Dataset Clustering w/ PCA",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of ICA ON ADULT DATASET
ica_adult_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_ICA_adjMI.csv")
ica_adult_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_ICA_compl.csv")
ica_adult_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_ICA_homo.csv")
ica_adult_MI2<-transpose(ica_adult_MI)
ica_adult_compl2<-transpose(ica_adult_compl)
ica_adult_homo2<-transpose(ica_adult_homo)

ica_adult_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_BIC_ICA.csv")
ica_adult_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_SSE_ICA.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(ica_adult_cluster_km$X1, ica_adult_cluster_km$`Adult SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(ica_adult_cluster_gmm$X1, ica_adult_cluster_gmm$`Adult BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(ica_adult_homo2$NoClusters, ica_adult_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(ica_adult_homo2$Kmeans,ica_adult_compl2$Kmeans,ica_adult_MI2$Kmeans),max(ica_adult_homo2$Kmeans,ica_adult_compl2$Kmeans,ica_adult_MI2$Kmeans)))
points(ica_adult_compl2$NoClusters ,ica_adult_compl2$Kmeans, type="l", lwd=3, col="green")
points(ica_adult_MI2$NoClusters ,ica_adult_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=2, y=0.25, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(ica_adult_homo2$NoClusters ,ica_adult_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(ica_adult_homo2$GMM,ica_adult_compl2$GMM,ica_adult_MI2$GMM),max(ica_adult_homo2$GMM,ica_adult_compl2$GMM,ica_adult_MI2$GMM)))
points(ica_adult_compl2$NoClusters ,ica_adult_compl2$GMM, type="l", lwd=3, col="green")
points(ica_adult_MI2$NoClusters ,ica_adult_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Adult Dataset Clustering w/ ica",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of ICA ON CREDIT DATASET
ica_credit_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_ICA_adjMI.csv")
ica_credit_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_ICA_compl.csv")
ica_credit_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_ICA_homo.csv")
ica_credit_MI2<-transpose(ica_credit_MI)
ica_credit_compl2<-transpose(ica_credit_compl)
ica_credit_homo2<-transpose(ica_credit_homo)

ica_credit_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_BIC_ica.csv")
ica_credit_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_SSE_ica.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(ica_credit_cluster_km$X1, ica_credit_cluster_km$`Credit SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(ica_credit_cluster_gmm$X1, ica_credit_cluster_gmm$`Credit BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(ica_credit_homo2$NoClusters, ica_credit_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(ica_credit_homo2$Kmeans,ica_credit_compl2$Kmeans,ica_credit_MI2$Kmeans),max(ica_credit_homo2$Kmeans,ica_credit_compl2$Kmeans,ica_credit_MI2$Kmeans)))
points(ica_credit_compl2$NoClusters ,ica_credit_compl2$Kmeans, type="l", lwd=3, col="green")
points(ica_credit_MI2$NoClusters ,ica_credit_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1.3, y=0.023, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(ica_credit_homo2$NoClusters ,ica_credit_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(ica_credit_homo2$GMM,ica_credit_compl2$GMM,ica_credit_MI2$GMM),max(ica_credit_homo2$GMM,ica_credit_compl2$GMM,ica_credit_MI2$GMM)))
points(ica_credit_compl2$NoClusters ,ica_credit_compl2$GMM, type="l", lwd=3, col="green")
points(ica_credit_MI2$NoClusters ,ica_credit_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Credit Dataset Clustering w/ ica",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of RCA ON ADULT DATASET
rp_adult_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RP_adjMI.csv")
rp_adult_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RP_compl.csv")
rp_adult_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RP_homo.csv")
rp_adult_MI2<-transpose(rp_adult_MI)
rp_adult_compl2<-transpose(rp_adult_compl)
rp_adult_homo2<-transpose(rp_adult_homo)

rp_adult_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_BIC_RP.csv")
rp_adult_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_SSE_RP.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(rp_adult_cluster_km$X1, rp_adult_cluster_km$`Adult SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(rp_adult_cluster_gmm$X1, rp_adult_cluster_gmm$`Adult BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(rp_adult_homo2$NoClusters, rp_adult_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(rp_adult_homo2$Kmeans,rp_adult_compl2$Kmeans,rp_adult_MI2$Kmeans),max(rp_adult_homo2$Kmeans,rp_adult_compl2$Kmeans,rp_adult_MI2$Kmeans)))
points(rp_adult_compl2$NoClusters ,rp_adult_compl2$Kmeans, type="l", lwd=3, col="green")
points(rp_adult_MI2$NoClusters ,rp_adult_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1, y=0.014, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(rp_adult_homo2$NoClusters ,rp_adult_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(rp_adult_homo2$GMM,rp_adult_compl2$GMM,rp_adult_MI2$GMM),max(rp_adult_homo2$GMM,rp_adult_compl2$GMM,rp_adult_MI2$GMM)))
points(rp_adult_compl2$NoClusters ,rp_adult_compl2$GMM, type="l", lwd=3, col="green")
points(rp_adult_MI2$NoClusters ,rp_adult_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Adult Dataset Clustering w/ RPA",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of RCA ON CREDIT DATASET
rp_credit_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RP_adjMI.csv")
rp_credit_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RP_compl.csv")
rp_credit_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RP_homo.csv")
rp_credit_MI2<-transpose(rp_credit_MI)
rp_credit_compl2<-transpose(rp_credit_compl)
rp_credit_homo2<-transpose(rp_credit_homo)

rp_credit_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_BIC_RP.csv")
rp_credit_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_SSE_RP.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(rp_credit_cluster_km$X1, rp_credit_cluster_km$`Credit SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(rp_credit_cluster_gmm$X1, rp_credit_cluster_gmm$`Credit BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(rp_credit_homo2$NoClusters, rp_credit_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(rp_credit_homo2$Kmeans,rp_credit_compl2$Kmeans,rp_credit_MI2$Kmeans),max(rp_credit_homo2$Kmeans,rp_credit_compl2$Kmeans,rp_credit_MI2$Kmeans)))
points(rp_credit_compl2$NoClusters ,rp_credit_compl2$Kmeans, type="l", lwd=3, col="green")
points(rp_credit_MI2$NoClusters ,rp_credit_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1.3, y=0.027, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(rp_credit_homo2$NoClusters ,rp_credit_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(rp_credit_homo2$GMM,rp_credit_compl2$GMM,rp_credit_MI2$GMM),max(rp_credit_homo2$GMM,rp_credit_compl2$GMM,rp_credit_MI2$GMM)))
points(rp_credit_compl2$NoClusters ,rp_credit_compl2$GMM, type="l", lwd=3, col="green")
points(rp_credit_MI2$NoClusters ,rp_credit_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Credit Dataset Clustering w/ RPA",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# CLUSTERING of RF ON ADULT DATASET
rf_adult_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RF_adjMI.csv")
rf_adult_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RF_compl.csv")
rf_adult_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_RF_homo.csv")
rf_adult_MI2<-transpose(rf_adult_MI)
rf_adult_compl2<-transpose(rf_adult_compl)
rf_adult_homo2<-transpose(rf_adult_homo)

rf_adult_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_BIC_RF.csv")
rf_adult_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Adult/Adult_SSE_RF.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(rf_adult_cluster_km$X1, rf_adult_cluster_km$`Adult SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(rf_adult_cluster_gmm$X1, rf_adult_cluster_gmm$`Adult BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(rf_adult_homo2$NoClusters, rf_adult_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(rf_adult_homo2$Kmeans,rf_adult_compl2$Kmeans,rf_adult_MI2$Kmeans),max(rf_adult_homo2$Kmeans,rf_adult_compl2$Kmeans,rf_adult_MI2$Kmeans)))
points(rf_adult_compl2$NoClusters ,rf_adult_compl2$Kmeans, type="l", lwd=3, col="green")
points(rf_adult_MI2$NoClusters ,rf_adult_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1, y=0.01, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(rf_adult_homo2$NoClusters ,rf_adult_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(rf_adult_homo2$GMM,rf_adult_compl2$GMM,rf_adult_MI2$GMM),max(rf_adult_homo2$GMM,rf_adult_compl2$GMM,rf_adult_MI2$GMM)))
points(rf_adult_compl2$NoClusters ,rf_adult_compl2$GMM, type="l", lwd=3, col="green")
points(rf_adult_MI2$NoClusters ,rf_adult_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Adult Dataset Clustering w/ Random Forest",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)



# CLUSTERING of RF ON CREDIT DATASET
rf_credit_MI<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RF_adjMI.csv")
rf_credit_compl<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RF_compl.csv")
rf_credit_homo<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_RF_homo.csv")
rf_credit_MI2<-transpose(rf_credit_MI)
rf_credit_compl2<-transpose(rf_credit_compl)
rf_credit_homo2<-transpose(rf_credit_homo)

rf_credit_cluster_gmm<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_BIC_RF.csv")
rf_credit_cluster_km<-readr::read_csv("./P3_Clustering_Algorithms_Reduced/Credit/Credit_SSE_RF.csv")

#Composite Plot
par(mfrow=c(2,2), oma = c(3,3,0,0) + 0.1, mar = c(1,1.3,3,1) + 0.1 )
plot(rf_credit_cluster_km$X1, rf_credit_cluster_km$`Credit SSE`, type="l", lwd=3,
     xlab="No. Clusters", ylab="SSE", col="blue")
title("kMeans: SSE vs No. Clusters", line=0.5)

plot(rf_credit_cluster_gmm$X1, rf_credit_cluster_gmm$`Credit BIC`, type="l", lwd=3, 
     xlab="No. Clusters", ylab="SSE", col="blue")
title("EM: SSE vs No. Clusters", line=0.5)

plot(rf_credit_homo2$NoClusters, rf_credit_homo2$Kmeans, type="l", lwd=3, col="red",
     xlab="No. Clusters", ylab="", ylim=c(min(rf_credit_homo2$Kmeans,rf_credit_compl2$Kmeans,rf_credit_MI2$Kmeans),max(rf_credit_homo2$Kmeans,rf_credit_compl2$Kmeans,rf_credit_MI2$Kmeans)))
points(rf_credit_compl2$NoClusters ,rf_credit_compl2$Kmeans, type="l", lwd=3, col="green")
points(rf_credit_MI2$NoClusters ,rf_credit_MI2$Kmeans, type="l", lwd=3, col="orange")
title("kMeans Metrics", line=0.5)
legend(x=1.3, y=0.027, c("Homogeneity", "Completeness", "Adj. MI"), bty="n", xpd=TRUE, 
       cex = 1, pch = c(15, 15, 15), col=c("red", "green", "orange"))

plot(rf_credit_homo2$NoClusters ,rf_credit_homo2$GMM, type="l", lwd=3, col="red", 
     xlab="No. Clusters", ylab="", ylim=c(min(rf_credit_homo2$GMM,rf_credit_compl2$GMM,rf_credit_MI2$GMM),max(rf_credit_homo2$GMM,rf_credit_compl2$GMM,rf_credit_MI2$GMM)))
points(rf_credit_compl2$NoClusters ,rf_credit_compl2$GMM, type="l", lwd=3, col="green")
points(rf_credit_MI2$NoClusters ,rf_credit_MI2$GMM, type="l", lwd=3, col="orange")
title("EM Metrics", line=0.5)
mtext("Credit Dataset Clustering w/ Random Forest",side = 3, line = -1.2, outer = TRUE, cex=1.3)
mtext("No. Clusters",side = 1, line = 1, outer = TRUE, cex=1.1)


# NNET COMPARISON ON ADULT DATASET
# COMP. NNET WITHOUT CLUSTER FEATURES
# Test error of everything on one plot
#PCA
pca_a<-pca_adult_nnet%>%group_by(param_clf__n_components)
pca_b<-pca_a%>%summarise(
  mean_pca=mean(mean_test_score),
  max_pca=max(mean_test_score),
  var_pca=var(mean_test_score),
  sd_pca=sd(mean_test_score)
)
pca_b<-cbind(pca_b, tally(pca_a)[2])
pca_b$ci_plus<-pca_b$mean+(pca_b$sd/sqrt(pca_b$n))*1.96
pca_b$ci_minus<-pca_b$mean-(pca_b$sd/sqrt(pca_b$n))*1.96

#ICA
ica_a<-ica_adult_nnet%>%group_by(param_clf__n_components)
ica_b<-ica_a%>%summarise(
  mean_ica=mean(mean_test_score),
  max_ica=max(mean_test_score),
  var_ica=var(mean_test_score),
  sd_ica=sd(mean_test_score)
)
ica_b<-cbind(ica_b,tally(ica_a)[2])
ica_b$ci_plus<-ica_b$mean+(ica_b$sd/sqrt(ica_b$n))*1.96
ica_b$ci_minus<-ica_b$mean-(ica_b$sd/sqrt(ica_b$n))*1.96

#RCA
rca_a<-rca_adult_nnet%>%group_by(param_clf__n_components)
rca_b<-rca_a%>%summarise(
  mean_rca=mean(mean_test_score),
  max_rca=max(mean_test_score),
  var_rca=var(mean_test_score),
  sd_rca=sd(mean_test_score)
)
rca_b<-cbind(rca_b,tally(rca_a)[2])
rca_b$ci_plus<-rca_b$mean+(rca_b$sd/sqrt(rca_b$n))*1.96
rca_b$ci_minus<-rca_b$mean-(rca_b$sd/sqrt(rca_b$n))*1.96

#RF
rf_a<-rf_adult_nnet%>%group_by(param_filter__n)
rf_b<-rf_a%>%summarise(
  mean_rf=mean(mean_test_score),
  max_rf=max(mean_test_score),
  var_rf=var(mean_test_score),
  sd_rf=sd(mean_test_score)
)
rf_b<-cbind(rf_b,tally(rf_a)[2])
rf_b$ci_plus<-rf_b$mean+(rf_b$sd/sqrt(rf_b$n))*1.96
rf_b$ci_minus<-rf_b$mean-(rf_b$sd/sqrt(rf_b$n))*1.96

pca_adult_data<-as.data.frame(cbind(pca_adult$X2, as.numeric(pca_adult$sum), as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(pca_adult_data)<-c('num', 'sum', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )


ggplot(data=pca_b)+geom_point(mapping=aes(x=param_clf__n_components, y=mean_pca, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean_pca, color='red'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=mean_ica, color='green'))+
  geom_line(data=ica_b, mapping=aes(x=param_clf__n_components, y=mean_ica, color='green'))+
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=mean_rca, color='yellow'))+
  geom_line(data=rca_b, mapping=aes(x=param_clf__n_components, y=mean_rca, color='yellow'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue'))+ 
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=mean_rf, color='orange'))+
  geom_line(data=rf_b, mapping=aes(x=param_filter__n, y=mean_rf, color='orange'))+
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=ci_plus, color='blue'))+
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=adult_nnet_ref))+
  geom_text(aes(0, adult_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of Components", y="Test Error", title="Comparison of NNets with Adult Dataset\n", color = "")+
  scale_color_manual(labels = c("95%CI", "ICA", "PCA", "RF", "RCA"), values = c("blue", "red", "green", "yellow", "orange"))+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# COMP. NNET WITHOUT CLUSTER FEATURES FOR CREDIT
# Test error of everything on one plot
#PCA
pca_a<-pca_credit_nnet%>%group_by(param_clf__n_components)
pca_b<-pca_a%>%summarise(
  mean_pca=mean(mean_test_score),
  max_pca=max(mean_test_score),
  var_pca=var(mean_test_score),
  sd_pca=sd(mean_test_score)
)
pca_b<-cbind(pca_b, tally(pca_a)[2])
pca_b$ci_plus<-pca_b$mean+(pca_b$sd/sqrt(pca_b$n))*1.96
pca_b$ci_minus<-pca_b$mean-(pca_b$sd/sqrt(pca_b$n))*1.96

#ICA
ica_a<-ica_credit_nnet%>%group_by(param_clf__n_components)
ica_b<-ica_a%>%summarise(
  mean_ica=mean(mean_test_score),
  max_ica=max(mean_test_score),
  var_ica=var(mean_test_score),
  sd_ica=sd(mean_test_score)
)
ica_b<-cbind(ica_b,tally(ica_a)[2])
ica_b$ci_plus<-ica_b$mean+(ica_b$sd/sqrt(ica_b$n))*1.96
ica_b$ci_minus<-ica_b$mean-(ica_b$sd/sqrt(ica_b$n))*1.96

#RCA
rca_a<-rca_credit_nnet%>%group_by(param_clf__n_components)
rca_b<-rca_a%>%summarise(
  mean_rca=mean(mean_test_score),
  max_rca=max(mean_test_score),
  var_rca=var(mean_test_score),
  sd_rca=sd(mean_test_score)
)
rca_b<-cbind(rca_b,tally(rca_a)[2])
rca_b$ci_plus<-rca_b$mean+(rca_b$sd/sqrt(rca_b$n))*1.96
rca_b$ci_minus<-rca_b$mean-(rca_b$sd/sqrt(rca_b$n))*1.96

#RF
rf_a<-rf_credit_nnet%>%group_by(param_filter__n)
rf_b<-rf_a%>%summarise(
  mean_rf=mean(mean_test_score),
  max_rf=max(mean_test_score),
  var_rf=var(mean_test_score),
  sd_rf=sd(mean_test_score)
)
rf_b<-cbind(rf_b,tally(rf_a)[2])
rf_b$ci_plus<-rf_b$mean+(rf_b$sd/sqrt(rf_b$n))*1.96
rf_b$ci_minus<-rf_b$mean-(rf_b$sd/sqrt(rf_b$n))*1.96

pca_credit_data<-as.data.frame(cbind(pca_credit$X2, as.numeric(pca_credit$sum), as.numeric(b$param_clf__n_components), as.numeric(b$mean), as.numeric(b$max), as.numeric(b$ci_plus), as.numeric(b$ci_minus) ))
colnames(pca_credit_data)<-c('num', 'sum', 'param_clf__n_components', 'mean', 'max', 'ci_plus','ci_minus' )


ggplot(data=pca_b)+geom_point(mapping=aes(x=param_clf__n_components, y=mean_pca, color='red'))+
  geom_line(mapping=aes(x=param_clf__n_components, y=mean_pca, color='red'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue')) +
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=mean_ica, color='green'))+
  geom_line(data=ica_b, mapping=aes(x=param_clf__n_components, y=mean_ica, color='green'))+
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(data=ica_b, mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=mean_rca, color='yellow'))+
  geom_line(data=rca_b, mapping=aes(x=param_clf__n_components, y=mean_rca, color='yellow'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=ci_plus, color='blue'))+
  geom_point(data=rca_b, mapping=aes(x=param_clf__n_components, y=ci_minus, color='blue'))+ 
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=mean_rf, color='orange'))+
  geom_line(data=rf_b, mapping=aes(x=param_filter__n, y=mean_rf, color='orange'))+
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=ci_plus, color='blue'))+
  geom_point(data=rf_b, mapping=aes(x=param_filter__n, y=ci_minus, color='blue')) +
  geom_hline(aes(yintercept=credit_nnet_ref))+
  geom_text(aes(0, credit_nnet_ref,label="Reference NNet Model", vjust=-1, hjust=0.10)) +
  labs(x="Number of Components", y="Test Error", title="Comparison of NNets with credit Dataset\n", color = "")+
  scale_color_manual(labels = c("95%CI", "ICA", "RF", "PCA", "RCA"), values = c("blue", "red", "green", "yellow", "orange"))+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))


# COMP. NNET WITH K-MEANS CLUSTERS ADDED FOR ADULT
adult_nnet_km_cluster_pca<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_km_PCA.csv", col_names=TRUE)
adult_nnet_km_cluster_ica<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_km_ICA.csv", col_names=TRUE)
adult_nnet_km_cluster_rp<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_km_RP.csv", col_names=TRUE)
adult_nnet_km_cluster_rf<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_km_RF.csv", col_names=TRUE)

pca_kmclust<-adult_nnet_km_cluster_pca%>%group_by(param_addClustKM__n_clusters)
pca_kmclust_b<-pca_kmclust%>%summarise(
  mean_pca=mean(mean_test_score),
  max_pca=max(mean_test_score),
  var_pca=var(mean_test_score),
  sd_pca=sd(mean_test_score)
)
pca_kmclust_b<-cbind(pca_kmclust_b, tally(pca_kmclust)[2])
pca_kmclust_b$ci_plus<-pca_kmclust_b$mean+(pca_kmclust_b$sd/sqrt(pca_kmclust_b$n))*1.96
pca_kmclust_b$ci_minus<-pca_kmclust_b$mean-(pca_kmclust_b$sd/sqrt(pca_kmclust_b$n))*1.96


ica_kmclust<-adult_nnet_km_cluster_ica%>%group_by(param_addClustKM__n_clusters)
ica_kmclust_b<-ica_kmclust%>%summarise(
  mean_ica=mean(mean_test_score),
  max_ica=max(mean_test_score),
  var_ica=var(mean_test_score),
  sd_ica=sd(mean_test_score)
)
ica_kmclust_b<-cbind(ica_kmclust_b, tally(ica_kmclust)[2])
ica_kmclust_b$ci_plus<-ica_kmclust_b$mean+(ica_kmclust_b$sd/sqrt(ica_kmclust_b$n))*1.96
ica_kmclust_b$ci_minus<-ica_kmclust_b$mean-(ica_kmclust_b$sd/sqrt(ica_kmclust_b$n))*1.96



rp_kmclust<-adult_nnet_km_cluster_rp%>%group_by(param_addClustKM__n_clusters)
rp_kmclust_b<-rp_kmclust%>%summarise(
  mean_rp=mean(mean_test_score),
  max_rp=max(mean_test_score),
  var_rp=var(mean_test_score),
  sd_rp=sd(mean_test_score)
)
rp_kmclust_b<-cbind(rp_kmclust_b, tally(rp_kmclust)[2])
rp_kmclust_b$ci_plus<-rp_kmclust_b$mean+(rp_kmclust_b$sd/sqrt(rp_kmclust_b$n))*1.96
rp_kmclust_b$ci_minus<-rp_kmclust_b$mean-(rp_kmclust_b$sd/sqrt(rp_kmclust_b$n))*1.96


rf_kmclust<-adult_nnet_km_cluster_rf%>%group_by(param_addClustKM__n_clusters)
rf_kmclust_b<-rf_kmclust%>%summarise(
  mean_rf=mean(mean_test_score),
  max_rf=max(mean_test_score),
  var_rf=var(mean_test_score),
  sd_rf=sd(mean_test_score)
)
rf_kmclust_b<-cbind(rf_kmclust_b, tally(rf_kmclust)[2])
rf_kmclust_b$ci_plus<-rf_kmclust_b$mean+(rf_kmclust_b$sd/sqrt(rf_kmclust_b$n))*1.96
rf_kmclust_b$ci_minus<-rf_kmclust_b$mean-(rf_kmclust_b$sd/sqrt(rf_kmclust_b$n))*1.96



ggplot()+geom_point(data=pca_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_pca, color="red"))+
  geom_line(data=pca_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_pca, color="red")) +
  geom_point(data=ica_kmclust_b,mapping= aes(x=param_addClustKM__n_clusters, y=max_ica, color="blue"))+
  geom_line(data=ica_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_ica, color="blue"))+
  geom_point(data=rp_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_rp, color="green"))+
  geom_line(data=rp_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_rp, color="green"))+
  geom_point(data=rf_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_rf, color="orange"))+
  geom_line(data=rf_kmclust_b, mapping=aes(x=param_addClustKM__n_clusters, y=max_rf, color="orange")) +
  scale_color_manual("Dim Red", labels = c("ICA","RCA", "RF","PCA"), values = c("red", "blue", "green", "orange"))+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  labs(x="Number of Components", y="Test Error", title="NNet Fit w/ Kmeans Clusters on Adult Dataset\n")
  





# COMP. NNET WITH GMM CLUSTERS ADDED FOR ADULT
adult_nnet_gmm_cluster_pca<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_gmm_PCA.csv", col_names=TRUE)
adult_nnet_gmm_cluster_ica<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_gmm_ICA.csv", col_names=TRUE)
adult_nnet_gmm_cluster_rp<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_gmm_RP.csv", col_names=TRUE)
adult_nnet_gmm_cluster_rf<-readr::read_csv("./P5_Neural_Networks_Reduced_With_Clusters/adult_gmm_RF.csv", col_names=TRUE)


pca_gmmclust<-adult_nnet_gmm_cluster_pca%>%group_by(param_addClustGMM__n_clusters)
pca_gmmclust_b<-pca_gmmclust%>%summarise(
  mean_pca=mean(mean_test_score),
  max_pca=max(mean_test_score),
  var_pca=var(mean_test_score),
  sd_pca=sd(mean_test_score)
)
pca_gmmclust_b<-cbind(pca_gmmclust_b, tally(pca_gmmclust)[2])
pca_gmmclust_b$ci_plus<-pca_gmmclust_b$mean+(pca_gmmclust_b$sd/sqrt(pca_gmmclust_b$n))*1.96
pca_gmmclust_b$ci_minus<-pca_gmmclust_b$mean-(pca_gmmclust_b$sd/sqrt(pca_gmmclust_b$n))*1.96


ica_gmmclust<-adult_nnet_gmm_cluster_ica%>%group_by(param_addClustGMM__n_clusters)
ica_gmmclust_b<-ica_gmmclust%>%summarise(
  mean_ica=mean(mean_test_score),
  max_ica=max(mean_test_score),
  var_ica=var(mean_test_score),
  sd_ica=sd(mean_test_score)
)
ica_gmmclust_b<-cbind(ica_gmmclust_b, tally(ica_gmmclust)[2])
ica_gmmclust_b$ci_plus<-ica_gmmclust_b$mean+(ica_gmmclust_b$sd/sqrt(ica_gmmclust_b$n))*1.96
ica_gmmclust_b$ci_minus<-ica_gmmclust_b$mean-(ica_gmmclust_b$sd/sqrt(ica_gmmclust_b$n))*1.96



rp_gmmclust<-adult_nnet_gmm_cluster_rp%>%group_by(param_addClustGMM__n_clusters)
rp_gmmclust_b<-rp_gmmclust%>%summarise(
  mean_rp=mean(mean_test_score),
  max_rp=max(mean_test_score),
  var_rp=var(mean_test_score),
  sd_rp=sd(mean_test_score)
)
rp_gmmclust_b<-cbind(rp_gmmclust_b, tally(rp_gmmclust)[2])
rp_gmmclust_b$ci_plus<-rp_gmmclust_b$mean+(rp_gmmclust_b$sd/sqrt(rp_gmmclust_b$n))*1.96
rp_gmmclust_b$ci_minus<-rp_gmmclust_b$mean-(rp_gmmclust_b$sd/sqrt(rp_gmmclust_b$n))*1.96


rf_gmmclust<-adult_nnet_gmm_cluster_rf%>%group_by(param_addClustGMM__n_clusters)
rf_gmmclust_b<-rf_gmmclust%>%summarise(
  mean_rf=mean(mean_test_score),
  max_rf=max(mean_test_score),
  var_rf=var(mean_test_score),
  sd_rf=sd(mean_test_score)
)
rf_gmmclust_b<-cbind(rf_gmmclust_b, tally(rf_gmmclust)[2])
rf_gmmclust_b$ci_plus<-rf_gmmclust_b$mean+(rf_gmmclust_b$sd/sqrt(rf_gmmclust_b$n))*1.96
rf_gmmclust_b$ci_minus<-rf_gmmclust_b$mean-(rf_gmmclust_b$sd/sqrt(rf_gmmclust_b$n))*1.96


ggplot()+geom_point(data=pca_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_pca, color="red"))+
  geom_line(data=pca_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_pca, color="red")) +
  geom_point(data=ica_gmmclust_b,mapping= aes(x=param_addClustGMM__n_clusters, y=max_ica, color="blue"))+
  geom_line(data=ica_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_ica, color="blue"))+
  geom_point(data=rp_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_rp, color="green"))+
  geom_line(data=rp_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_rp, color="green"))+
  geom_point(data=rf_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_rf, color="orange"))+
  geom_line(data=rf_gmmclust_b, mapping=aes(x=param_addClustGMM__n_clusters, y=max_rf, color="orange")) +
  scale_color_manual("Dim Red", labels = c("ICA","RCA", "RF","PCA"), values = c("red", "blue", "green", "orange"))+
  theme(legend.position="top", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=13), axis.text.x = element_text(size=13))+
  labs(x="Number of Components", y="Test Error", title="NNet Fit w/ EM Clusters on Adult Dataset\n")










