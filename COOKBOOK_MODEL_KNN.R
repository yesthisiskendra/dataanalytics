## *************************************************
## MODELING: KNN TO PREDICT PERCENT PROFIT
## *************************************************
## 56% accuracy | DF USED: 'powerfulDF' 
## ================================================
## STEP 1: LOAD THE DATA & LIBRARIES
## ================================================
library(ggplot2)
library(class)
library(dplyr)

#Setting working directory 
# setwd("C:\\Users\\ho511\\Desktop\\IST 707\\Team Project\\CSVs")
setwd("/Users/kosburn 1/syracuse/IST707/project/ALI/CSV")

#Reading in the Movies Spreadsheet 
kNN_train_percProf <- read.csv("train_perc_profit.csv", header = TRUE)
kNN_test_percProf <- read.csv("test_perc_profit.csv", header= TRUE)

## =================================================
## STEP 2: PREP DATA FOR SPECIFIC MODEL
## =================================================
#***************************************************
# kNN requires Numeric Data Only
# Remove the label from the training data 
# Save Label in a train_label vector 
#***************************************************
#Removing all columns that are factors: director, genre, name, rating, star, writer
kNN_train_percProf <- kNN_train_percProf[,!colnames(kNN_train_percProf) %in% 
                                           c('director', 'genre', 'name', 'rating', 'star', 'writer','X.1', 'X', 'directorGender','starGender', 'starPopularity')]
kNN_test_percProf <- kNN_test_percProf[,!colnames(kNN_test_percProf) %in% 
                                         c('director', 'genre', 'name', 'rating', 'star', 'writer','X.1', 'X', 'directorGender','starGender', 'starPopularity')]

#Removing gross and profit
kNN_train_percProf <- kNN_train_percProf[,!colnames(kNN_train_percProf) %in% c('gross', 'profit')]
kNN_test_percProf <- kNN_test_percProf[,!colnames(kNN_test_percProf) %in% c('gross', 'profit')]

#Saving DF with label for later
kNN_with_label <- kNN_train_percProf

#Removing the classification label from BOTH the Train and Test dfs and storing it 
kNN_train_percProf_label <- kNN_train_percProf[,colnames(kNN_train_percProf) %in% ('percProf')]
kNN_train_percProf <- kNN_train_percProf[,!colnames(kNN_train_percProf) %in% ('percProf')]

kNN_test_percProf_label <- kNN_test_percProf[,colnames(kNN_test_percProf) %in% ('percProf')]
kNN_test_percProf <- kNN_test_percProf[,!colnames(kNN_test_percProf) %in% ('percProf')]

#Changing everything to numeric from int 
kNN_train_percProf <- kNN_train_percProf %>% mutate_if(is.integer, as.numeric)
kNN_test_percProf <- kNN_test_percProf %>% mutate_if(is.integer, as.numeric)

## =================================================
## STEP 3: RUN THE MODEL
## =================================================
# Setting k as the sqrt of the number of rows of the dataset 
(k <- round(sqrt(868)))
kNN <- class::knn(train = kNN_train_percProf, test = kNN_test_percProf, cl = kNN_train_percProf_label, k = k, prob = TRUE)
(kNN_table <- table(kNN, kNN_test_percProf_label))

(miscalculation_rate <- 1 - sum(diag(kNN_table))/sum(kNN_table))
(accuracy_rate <- sum(diag(kNN_table))/sum(kNN_table))

