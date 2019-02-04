# CS7641 Assignment 1

## Background: 
The purpose of this project is to explore some techniques in supervised learning.

## Datasets:
Datasets were obtained from the [UC Irvine Machine Learning Repository](http://archive.ics.uci.edu/).

###Two datasets were selected for comparison:

####-Credit Card Default Dataset 
This dataset was taken from Taiwanese banks to estimate loan default behavior of customers, which is an area of great importance to both banks and financial markets in general. 77.9% of the observations are “default”, so a classifier would need to perform better than this. The dataset notes indicate that in existing research, only ANN was the only classifier that could “accurately estimate the real probability of default.”  


#####-US Census Database 
The second dataset was created from the 1994 US Census database and is a broad slice of information regarding individuals personal and work/educational background.  The objective of the dataset was to predict income, specifically whether or not an individual earned more than $50,000 annually.  The dataset has 75.22% of the observations labeled “<=50K” which will be the benchmark for any classifier to beat (relative to random guessing).


## Methods Explored:
1. Decision Trees
R package `rpart`

2. Neural Net
R package `nnet` and CARET hyperparameter tuning.

3. k-Nearest Neighbors
k-nearest neighbors implemention with CARET hyperparameter tuning.

4. Boosting
Adaboost implementation using R package `adabag`

5. Support Vector Machine
Explore both radial and polynomial kernal functions using `svm` function in R package `e1071`.

## Results:

| |Credit Card Dataset|| Census Dataset||
|--|----------------|--|-------------|--|
|Method|  Training Err| Testing Err | Training Err | Testing Err |
|Decision Tree| 82.10% | 82.40% | 86.94% | 86.06% |
|Neural Net| 82.18% |82.31%  |85.98% |86.16% |
|k-NN| 81.06% |82.03% |82.80% |82.43% |
|Boosting|81.98% |82.03% |86.06% |86.01% |
|SVM | 82.54% |82.68% |85.41% |85.50% |


