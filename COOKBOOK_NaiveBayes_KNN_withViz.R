## ====================================
## STEP 1: IMPORT LIBRARIES
## ====================================
library(e1071)
library(naivebayes)
library(dplyr)

## ====================================
## STEP 2: IMPORT DATA
## ==================================== 
## PUT THE VARIABLES IN, YO
setwd("/Users/kosburn 1/syracuse/IST707/WK7")
varToTestName <- c('Numbers')
columnToRemove <- c('label')


trainNB <- read.csv("handwriting_train.csv", header = TRUE)
## testNB <- read.csv("/Users/kosburn/syracuse/IST707/WK7/handwriting_test.csv", header = TRUE)

## 2a. Working backup
originalTrainNB <- trainNB
## originalTestNB <-testNB

## ====================================
## STEP 3: PREPARE THE DATA
## ==================================== 

## 3a. REMOVE LABEL
trainNB_noLabel <- trainNB[,-1]

## 3b. TURN EVERYTHING != 0 into 1
trainNB_noLabel <- trainNB_noLabel %>%
  mutate_if(is.integer, as.numeric)
trainNB_noLabel[trainNB_noLabel != 0 ] <- 1
max(trainNB_noLabel)

## 3c. ADD LABEL BACK
trainNB_boollabel <- cbind("label"=trainNB$label, trainNB_noLabel)

## 3d. ALL TO FACTOR 
trainNB_preSplit <- trainNB_boollabel %>%
  mutate_all(as.factor)

## 3e. REMOVE FACTOR WITH LEVEL 1
## TODO: UNDERSTAND THIS LINE
trainNB_preSplit <- (trainNB_preSplit[, sapply(trainNB_preSplit, nlevels) > 1])
workingTrain <- trainNB_preSplit 


## ====================================
## STEP 4: SPLIT THE DATA
## ==================================== 

## 4a. SPLIT THE DATA EVENLY (like ham and mad) INTO 1-9
## TODO: REVISIT SUBSET
## subset(dataframe, colimsplittingon == 0)
Zero <- subset(workingTrain, label == 0)
One <- subset(workingTrain, label == 1)
Two <- subset(workingTrain, label == 2)
Three <- subset(workingTrain, label == 3)
Four <- subset(workingTrain, label == 4)
Five <- subset(workingTrain, label == 5)
Six <- subset(workingTrain, label == 6)
Seven <- subset(workingTrain, label == 7)
Eight <- subset(workingTrain, label == 8)
Nine <- subset(workingTrain, label == 9)

## GIVING UP ON LOOPS FOR NOW
## GET SAMPLE
get_sample <- function(df, n) { 
  set.seed(42)
  sample(nrow(df), n)
}

## GET TRAINSET
get_train_set <- function(df, columns) {
  set.seed(42)
  df[columns,]
}

## GET TEST SET
#Creating a function to create a testing df 
get_test_set <- function(df, columns, n) {
  df <- df[-columns,]
  set.seed(42)
  df[sample(nrow(df), n),]
}


sample0 <- get_sample(Zero, 300)
train0 <- get_train_set(Zero, sample0)
test0 <- get_test_set(Zero, sample0, 100)

sample1 <- get_sample(One, 300)
train1 <- get_train_set(One, sample1)
test1 <- get_test_set(One, sample1, 100)

sample2 <- get_sample(Two, 300)
train2 <- get_train_set(Two, sample2)
test2 <- get_test_set(Two, sample2, 100)

sample3 <- get_sample(Three, 300)
train3 <- get_train_set(Three, sample3)
test3 <- get_test_set(Three, sample3, 100)

sample4 <- get_sample(Four, 300)
train4 <- get_train_set(Four, sample4)
test4 <- get_test_set(Four, sample4, 100)

sample5 <- get_sample(Five, 300)
train5 <- get_train_set(Five, sample5)
test5 <- get_test_set(Five, sample5, 100)

sample6 <- get_sample(Six, 300)
train6 <- get_train_set(Six, sample6)
test6 <- get_test_set(Six, sample6, 100)

sample7 <- get_sample(Seven, 300)
train7 <- get_train_set(Seven, sample7)
test7 <- get_test_set(Seven, sample7, 100)

sample8 <- get_sample(Eight, 300)
train8 <- get_train_set(Eight, sample8)
test8 <- get_test_set(Eight, sample8, 100)

sample9 <- get_sample(Nine, 300)
train9 <- get_train_set(Nine, sample9)
test9 <- get_test_set(Nine, sample9, 100)

df_train <- rbind(train0, train1, train2, train3, train4, train5, train6, train7, train8, train9)
df_test <- rbind(test0, test1, test2, test3, test4, test5, test6, test7, test8, test9)

df_train_label <- df_train$label
df_train_nolabel <- df_train[,-1]

df_test_label <- df_test$label
df_test_nolabel <- df_test[,-1]

modelType <- ('Naive Bayes')
modelTitle <- (1)
modelname <- paste(modelType,"_", modelTitle, sep="")
varToTest <- levels(df_train_label)

NB_e1071<-naiveBayes(label~., data=df_train, na.action = na.pass)
NB_e1071_Pred <- predict(NB_e1071, df_test_nolabel)
NB_e1071
table <- table(NB_e1071_Pred,df_test_label)

(miscalculation_rate <- 1 - sum(diag(table))/sum(table))
(accuracy_rate <- sum(diag(table))/sum(table))
exportDf <- data.frame(modelname, accuracy_rate, table)
exportDf
filenameCSV <- paste(modelname, ".csv", sep="")
write.csv(exportDf, file = filenameCSV)


##Visualize
#plot(NB_e1071, ylab = "Density", main = "Naive Bayes Plot")

ggfilename <- paste(modelType, "_GG.png", sep="")
ggplotTitle <- paste("Accuracy Percentage running ", modelType," model for ", varToTestName, sep="")
varToTest <- levels(df_train_label)
nf <- length(varToTest)
nt <- (nrow(df_test)/nf)
model <- replicate(nf, "vis")
accuracy <- c(diag(table)/nt*100)
model_accuracy <- data.frame(varToTest, model, accuracy)
(model_plot <- ggplot(model_accuracy, aes(x = varToTest, y = accuracy, fill = rainbow(nf))) + geom_bar(stat = "identity") + theme(legend.position ="none") + scale_x_discrete(limit = varToTest))
(model_plot <- model_plot + ylab("Accuracy Percentage") + xlab(varToTestName) + ggtitle(ggplotTitle) + ylim(0, 100))+ggsave(ggfilename, plot = last_plot(), device = NULL)







## *************************************************
## MODELING: KNN TO PREDICT PERCENT PROFIT
## *************************************************
## ================================================
## STEP 1: LOAD THE DATA & LIBRARIES
## ================================================
library(ggplot2) 
library(class)
library(dplyr)

## SET THE VARS & WORKING DIRECTORY
modelType <- ('KNN')


# ## REMOVE ANY POSSIBLE Xs in df_train
# for(name in colnames(df_train)){
#   if(grepl('X', name)){
#     df_train <- df_train[,!colnames(df_train) %in% name]
#     print(name)    
#   }
# }
# ## REMOVE ANY POSSIBLE Xs in df_test
# for(name in colnames(df_test)){
#   if(grepl('X', name)){
#     df_test <- df_test[,!colnames(df_test) %in% name]
#   }
# }


## =================================================
## STEP 2: PREP DATA FOR SPECIFIC MODEL
## =================================================
#***************************************************
# kNN requires Numeric Data Only
# Remove the label from the training data 
# Save Label in a train_label vector 
#***************************************************

df_train_label <- df_train[,colnames(df_train) %in% columnToRemove]
df_test_label <- df_test[,colnames(df_test) %in% columnToRemove]

# #Removing all columns that are factors
# factorColumns <- df_train %>% Filter(f = is.factor) %>% names
# df_train <- df_train[,!colnames(df_train) %in% factorColumns]
# df_test <- df_test[,!colnames(df_test) %in% factorColumns]

# #Changing everything to numeric from int 
# df_train <- df_train %>% mutate_if(is.integer, as.numeric)
# df_test <- df_test %>% mutate_if(is.integer, as.numeric)

## =================================================
## STEP 3: RUN THE MODEL
## =================================================
# Setting k as the sqrt of the number of rows of the dataset 
modelTitle <- (1)
modelname <- paste(modelType,"_", modelTitle, sep="")

(k <- round(sqrt(868)))
kNN <- class::knn(train = df_train, test = df_test, cl = df_train_label, k = k, prob = TRUE)
(table <- table(kNN, df_test_label))

(miscalculation_rate <- 1 - sum(diag(table))/sum(table))
(accuracy_rate <- sum(diag(table))/sum(table))
exportDf <- data.frame(modelname, accuracy_rate, table)
exportDf
filenameCSV <- paste(modelname, ".csv", sep="")
write.csv(exportDf, file = filenameCSV)

## =================================================
## STEP 4: MAKE THE VIZ
## =================================================
## nf stands for Number of Factors we are trying to predit
## nt stands for Number of Test objects within each factor
## TODO change percProf to varToTest

ggfilename <- paste(modelType, "_GG.png", sep="")
ggplotTitle <- paste("Accuracy Percentage running KNN model for ", varToTestName, sep="")
varToTest <- levels(df_train_label)
nf <- length(varToTest)
nt <- (nrow(df_test)/nf)
model <- replicate(nf, "vis")
(accuracy <- c(diag(table)/nt*100))
(accuracy_rate <- sum(diag(table))/sum(table))
model_accuracy <- data.frame(varToTest, model, accuracy)
(model_plot <- ggplot(model_accuracy, aes(x = varToTest, y = accuracy, fill = rainbow(nf))) + geom_bar(stat = "identity") + theme(legend.position ="none") + scale_x_discrete(limit = varToTest))
(model_plot <- model_plot + ylab("Accuracy Percentage") + xlab(varToTestName) + ggtitle(ggplotTitle) + ylim(0, 100))+ggsave(ggfilename, plot = last_plot(), device = NULL)


