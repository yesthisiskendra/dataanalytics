## MODEL: KNN | TAKES: NUMERIC DATA ONLY  | REMOVE LABEL FROM BOTH TEST AND TRAIN
library(ggplot2)
library(class)
library(dplyr)
setwd("/Users/kosburn 1/syracuse/IST707/project/ALI/CSV")
kNN_train_percProf <- read.csv("train_perc_profit.csv", header = TRUE)
kNN_test_percProf <- read.csv("test_perc_profit.csv", header= TRUE)
#Removing the classification label from BOTH the Train and Test dfs and storing it 
kNN_train_percProf_label <- kNN_train_percProf[,colnames(kNN_train_percProf) %in% ('percProf')]
kNN_train_percProf <- kNN_train_percProf[,!colnames(kNN_train_percProf) %in% ('percProf')]
kNN_test_percProf_label <- kNN_test_percProf[,colnames(kNN_test_percProf) %in% ('percProf')]
kNN_test_percProf <- kNN_test_percProf[,!colnames(kNN_test_percProf) %in% ('percProf')]
#Changing everything to numeric from int 
kNN_train_percProf <- kNN_train_percProf %>% mutate_if(is.integer, as.numeric)
kNN_test_percProf <- kNN_test_percProf %>% mutate_if(is.integer, as.numeric)
(k <- round(sqrt(868)))
kNN <- class::knn(train = kNN_train_percProf, test = kNN_test_percProf, cl = kNN_train_percProf_label, k = k, prob = TRUE)
(kNN_table <- table(kNN, kNN_test_percProf_label))
(accuracy_rate <- sum(diag(kNN_table))/sum(kNN_table))

