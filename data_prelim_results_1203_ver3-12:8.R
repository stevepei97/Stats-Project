## Read data
raw_data <- read.csv("Documents/ucla/Stats 101C/final project/HTrainLast.csv")
# View(raw_data)
dim(raw_data)


## Clean data
raw_data$Ob <- NULL

raw_data$Age <- raw_data$YrSold-raw_data$YearBuilt

which(is.na(raw_data$affordabilitty))
raw_data <- raw_data[-c(833,2428),]
na.cols <- which(colSums(is.na(raw_data)) > 0) # Returns indices for all predictors with N/A's
na.cols 
sort(colSums(sapply(raw_data[na.cols], is.na)), decreasing = TRUE) # Order predictors with N/A's in descending order w.r.t. number of N/A's
raw_data <- within(raw_data, rm(PoolQC, MiscFeature, Alley, Fence, FireplaceQu)) # Remove all predictors with over 2000 N/A's
factorColIndex <- sapply(raw_data, is.factor)
which(factorColIndex)
# Some useful functions for data cleaning
naToMinusOne <- function(i){
  for (j in 1:nrow(raw_data)){
    if (is.na(raw_data[j,i]))
      raw_data[j,i] <- -1
  }
  return(raw_data)
}
naToMode <- function(i){
  uniqi <- unique(raw_data[,i])
  raw_data[is.na(raw_data[,i]), i] <- uniqi[which.max(tabulate(match(i, uniqi)))]
  return(raw_data)
}
naToUnf <- function(i){
  raw_data[,i] <- as.character(raw_data[,i])
  for (j in 1:nrow(raw_data)){
    if (is.na(raw_data[j,i]))
      raw_data[j,i] <- "Unf"
  }
  raw_data[,i] <- as.factor(raw_data[,i])
  return(raw_data)
}
### for numerical 
naToMedian <- function(i){
  for (j in 1:nrow(raw_data)){
    if (is.na(raw_data[j,i]))
      raw_data[j,i] <- median(raw_data[,i],na.rm=T)
  }
  return(raw_data)
}
naToMean <- function(i){
  for (j in 1:nrow(raw_data)){
    if (is.na(raw_data[j,i]))
      raw_data[j,i] <- mean(raw_data[,i],na.rm=T)
  }
  return(raw_data)
}

naToModeIndex <- c(2,8,41,52,54)
#naToMinusOneIndex <- c(3,25,33,35:37,46,47,57,59,60)
naToUnfIndex <- c(24,29:32,34,56,58,61,62)
#naToMedianIndex <- c(3,25,33,35:37,46,47,57,59,60)
naToMedianIndex <- c(3,25,33,46,47,57)
#naToMeanIndex <- c()
for (i in naToModeIndex){
  raw_data <- naToMode(i)
}
for (i in naToMinusOneIndex){
  raw_data <- naToMinusOne(i)
}
for (i in naToUnfIndex){
  raw_data <- naToUnf(i)
}
for (i in naToMeanIndex){
  raw_data <- naToMedian(i)
}
for (i in naToMedianIndex){
  raw_data <- naToMedian(i)
}
anyNA(raw_data) # should be no


findex<-which(sapply(raw_data[,-75],is.factor))
for (i in findex)
{
  print(table(raw_data[,i]))
}

##### PCA
a<-which(sapply(raw_data,is.numeric))
a
pca<-prcomp(raw_data[,a],scale=T)
print(pca)
plot(pca)
biplot(pca, col = c("gray", "black"))

######## PCA alter
num_raw<-sapply(raw_data[,-75], as.numeric)
pca<-prcomp(num_raw)
print(pca)
summary(pca)

##### MFA
library(FactoMineR)
sapply(raw_data[,which(sapply(raw_data[,-75],is.factor))],nlevels)
MFA(raw_data[,-75])

##### GBM
library(caret)
set.seed(1234567)
train.index <- sample(nrow(raw_data), nrow(raw_data)*0.7)
raw_data_70 <- raw_data[train.index,]
raw_data_30 <- raw_data[-train.index,]
predictors <- c("OverallQual", "FullBath", "YearBuilt", "GrLivArea", "GarageCars", "YearRemodAdd", "GarageArea", "TotalBsmtSF", "ExterQual", "GarageFinish")
boost1 <- train(raw_data_70[,predictors], raw_data_70[,"affordabilitty"], method='gbm', tuneLength = 3)
boost1.pred <- predict(boost1, raw_data_30)
table(boost1.pred, raw_data_30$affordabilitty)




## Split into training/testing 90/10 just for the sake of testing models (not using actual testing data yet!)
set.seed(1234567)
trainingIndex <- sample(nrow(raw_data), nrow(raw_data)*0.7)
raw_dataTraining <- raw_data[trainingIndex,]
raw_dataTesting <- raw_data[-trainingIndex,]
raw_dataTestingResponse <- raw_data$affordabilitty[-trainingIndex]

## Random forest
library(randomForest)
# using -1
#rf_1 <- randomForest(affordabilitty~Neighborhood+OverallQual+GrLivArea+Age+FullBath+ X1stFlrSF
#                    +TotalBsmtSF+X2ndFlrSF+BsmtQual+BsmtFinSF1+LotArea+GarageArea, data = raw_data, subset=trainingIndex, mtry=4, importance=TRUE)

# using median
rf_1 <- randomForest(affordabilitty~Neighborhood+OverallQual+GrLivArea+Age+FullBath+ X1stFlrSF
                                         +TotalBsmtSF+X2ndFlrSF+BsmtQual+BsmtFinSF1+LotArea+GarageArea+LotFrontage
                     +BsmtFinType1+ExterQual+WoodDeckSF, data = raw_data, subset=trainingIndex, mtry=4, importance=TRUE)
                     

rf_1 <- randomForest(affordabilitty~., data = raw_data, subset=trainingIndex, mtry=21, importance=TRUE)
importance(rf_1)

sort(importance(rf_1)[,3],decreasing = T)[1:15]
sort(importance(rf_1)[,4],decreasing = T)[1:15]

rf_1_prediction <- predict(rf_1, raw_dataTesting, type="class")
table(rf_1_prediction, raw_dataTestingResponse)



## Clean actual testing data
raw_testing_data <- read.csv("Documents/ucla/Stats 101C/final project/HTestLastNoY.csv")
# View(raw_testing_data)
dim(raw_testing_data)
raw_testing_data$Ob <- NULL

raw_testing_data$Age<-raw_testing_data$YrSold-raw_testing_data$YearBuilt

raw_testing_data <- within(raw_testing_data, rm(PoolQC, MiscFeature, Alley, Fence, FireplaceQu))
na.testing.cols <- which(colSums(is.na(raw_testing_data)) > 0)
na.testing.cols 
sort(colSums(sapply(raw_testing_data[na.testing.cols], is.na)), decreasing = TRUE)

naToMinusOneTesting <- function(i){
  for (j in 1:nrow(raw_testing_data)){
    if (is.na(raw_testing_data[j,i]))
      raw_testing_data[j,i] <- -1
  }
  return(raw_testing_data)
}
naToModeTesting <- function(i){
  uniqi <- unique(raw_testing_data[,i])
  raw_testing_data[is.na(raw_testing_data[,i]), i] <- uniqi[which.max(tabulate(match(i, uniqi)))]
  return(raw_testing_data)
}
naToUnfTesting <- function(i){
  raw_testing_data[,i] <- as.character(raw_testing_data[,i])
  for (j in 1:nrow(raw_testing_data)){
    if (is.na(raw_testing_data[j,i]))
      raw_testing_data[j,i] <- "Unf"
  }
  raw_testing_data[,i] <- as.factor(raw_testing_data[,i])
  return(raw_testing_data)
}
### for numerical 
naToMedianTesting <- function(i){
  for (j in 1:nrow(raw_testing_data)){
    if (is.na(raw_testing_data[j,i]))
      raw_testing_data[j,i] <- median(raw_testing_data[,i],na.rm = T)
  }
  return(raw_testing_data)
}
naToMeanTesting <- function(i){
  for (j in 1:nrow(raw_testing_data)){
    if (is.na(raw_testing_data[j,i]))
      raw_testing_data[j,i] <- mean(raw_testing_data[,i],na.rm = T)
  }
  return(raw_testing_data)
}

naToModeTestingIndex <- c(2,22,23,73)
#naToMinusOneTestingIndex <- c(3,25,33,35:37,46,47,57,59,60)
naToUnfTestingIndex <- c(24,29:32,34,56,58,61,62)
naToMedianTestingIndex <- c(3,25,33,35:37,46,47,57,59,60)
#naToMeanTestingIndex <- c()

for (i in naToModeTestingIndex){
  raw_testing_data <- naToModeTesting(i)
}
for (i in naToMinusOneTestingIndex){
  raw_testing_data <- naToMinusOneTesting(i)
}
for (i in naToUnfTestingIndex){
  raw_testing_data <- naToUnfTesting(i)
}
for (i in naToMeanTestingIndex){
  raw_testing_data <- naToMedian(i)
}
for (i in naToMedianTestingIndex){
  raw_testing_data <- naToMedianTesting(i)
}
anyNA(raw_testing_data) # Yes!

# POSSIBLE SOLUTION #1 TO LAST LINE OF CODE AND FAILED
factorColIndex <- sapply(raw_data, is.factor)

which(factorColIndex)
factorColIndexTesting <- sapply(raw_testing_data, is.factor)
fci <- which(factorColIndexTesting)
fci

levels(raw_data[,21]) <- levels(raw_testing_data[,21])
levels(raw_data[,22]) <- levels(raw_testing_data[,22])
levels(raw_data[,38]) <- levels(raw_testing_data[,38])
for (i in fci){
  levels(raw_testing_data[,i]) <- levels(raw_data[,i])
}

rf_1 <- randomForest(affordabilitty~., data = raw_data,subset = trainingIndex, mtry=21, importance=TRUE)
#rf_1 <- randomForest(affordabilitty~Neighborhood+OverallQual+GrLivArea+Age+FullBath+ X1stFlrSF
 #                    +TotalBsmtSF+X2ndFlrSF+BsmtQual+BsmtFinSF1+LotArea+GarageArea, data = raw_data, subset=trainingIndex, mtry=8, importance=TRUE)
raw_dataTesting<-raw_data[-trainingIndex,]
rf_1_prediction <- predict(rf_1, raw_dataTesting, type="class")
table(rf_1_prediction, raw_dataTestingResponse)


# rf_1 <- randomForest(affordabilitty~., data = raw_data, subset=trainingIndex, mtry=21, importance=TRUE)
# rf_1_prediction <- predict(rf_1, raw_dataTesting, type="class")
# table(rf_1_prediction, raw_dataTestingResponse)


rf_full <- randomForest(affordabilitty~., data = raw_data, mtry=21, importance=TRUE)
rf_full <- randomForest(affordabilitty~Neighborhood+OverallQual+GrLivArea+Age+FullBath+ X1stFlrSF
                     +TotalBsmtSF+X2ndFlrSF+BsmtQual+BsmtFinSF1+LotArea+GarageArea, data = raw_data, subset=trainingIndex, mtry=4, importance=TRUE)

rf_full <- randomForest(affordabilitty~Neighborhood+OverallQual+GrLivArea+Age+FullBath+ X1stFlrSF
                        +TotalBsmtSF+X2ndFlrSF+BsmtQual+BsmtFinSF1+LotArea+GarageArea+LotFrontage
                        +BsmtFinType1+ExterQual+WoodDeckSF, data = raw_data, mtry=4, importance=TRUE)

rf_full_output <- predict(rf_full, raw_testing_data, type="class") 

pred_prel <- data.frame(1:1500, rf_full_output)
colnames(pred_prel) <- c("Ob","affordabilitty")
View(pred_prel)

write.csv(pred_prel, file="prelim_prediction_1203(3).csv",row.names = F)
