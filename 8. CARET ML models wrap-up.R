library(caret)

## function for the metric
macroF1 = function(data, lev = NULL, model = NULL){
  cm = as.matrix(table(data$obs, data$pred)) #confusion matrix
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = ifelse(is.na(2 * precision * recall / (precision + recall)),0,2 * precision * recall / (precision + recall))
  macroF1_val = sum(f1)/4
  names(macroF1_val) <- c("F1")
  macroF1_val
}

## remake in a data.frame
data = as.data.frame(data_ft)
data$Target = as.integer(data$Target)

## OHE
dmy <- dummyVars(" ~ .", data = data, fullRank = T)
data <- data.frame(predict(dmy, newdata = data))

## Target in alphanumeric
data$Target = ifelse(data$Target == 1, "T1",
                           ifelse(data$Target == 2, "T2",
                                  ifelse(data$Target == 3, "T3", "T4")))


# Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data$Target, p=0.75, list=FALSE)
trainSet <- data[ index,]
testSet <- data[-index,]

# upSample
trainSet = upSample(trainSet, trainSet$Target) %>%
  select(-c(Class))

# Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 2,
                      verbose = TRUE)

outcomeName<-'Target'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName]
trainSet$Target = as.factor(trainSet$Target)
Target_Pred <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

Target_Pred

## train some models
## gbm
fitControlGBM = trainControl(
                  method = "cv",
                  number = 5,
                  summaryFunction = macroF1)

model_gbm = train(trainSet[,predictors],
                  trainSet[,outcomeName],
                  method='gbm',
                  metric = "F1",
                  trControl = fitControlGBM,
                  tuneLength = 10)


predictors_reduced7 = c('edjefi', 'meaneduc', 'age_IQR', 'house_score','phone_per_capita',
                        'dependency', 'overcrowding')

model_gbm_reduced7 = train(trainSet[,predictors_reduced7],
                  trainSet[,outcomeName],
                  method='gbm',
                  metric = "F1",
                  trControl = fitControlGBM,
                  tuneLength = 10)

## gbm
fitControlGBM = trainControl(
  method = "cv",
  number = 5,
  summaryFunction = macroF1)

model_gbm = train(trainSet[,predictors],
                  trainSet[,outcomeName],
                  method='gbm',
                  metric = "F1",
                  trControl = fitControlGBM,
                  tuneLength = 10)

predictors_reduced7 = c('edjefi', 'meaneduc', 'age_IQR', 'house_score','phone_per_capita',
                        'dependency', 'overcrowding')

model_gbm_reduced7 = train(trainSet[,predictors_reduced7],
                           trainSet[,outcomeName],
                           method='gbm',
                           metric = "F1",
                           trControl = fitControlGBM,
                           tuneLength = 10)


## xgboost
fitControlXGB = trainControl(
                method = "cv",
                number = 5,
                summaryFunction = macroF1)

model_xgb = train(trainSet[,predictors],
                  trainSet[,outcomeName],
                  method='xgbTree',
                  metric = "F1",
                  trControl = fitControlGBM,
                  tuneLength = 10)

predictors_reduced7 = c('edjefi', 'meaneduc', 'age_IQR', 'house_score','phone_per_capita',
                        'dependency', 'overcrowding')

model_xgb_reduced7 = train(trainSet[,predictors_reduced7],
                           trainSet[,outcomeName],
                           method='xgbTree',
                           metric = "F1",
                           trControl = fitControlXGB,
                           tuneLength = 3)


## rf
fitControlRF = trainControl(
              method = "cv",
              number = 10,
              summaryFunction = macroF1)
model_rf = train(trainSet[,predictors],
                 trainSet[,outcomeName],
                 method='rf',
                 trControl = fitControlRF,
                 metric = "F1",
                 tuneLength = 5)


## neural networks
fitControlNN = trainControl(
                method = "cv",
                number = 5,
                summaryFunction = macroF1)
model_nnet = train(trainSet[,predictors],
                   trainSet[,outcomeName],
                   method='nnet',
                   tuneLength = 10,
                   trControl = fitControlNN,
                   metric = "F1")

## KNN
fitControlKNN = trainControl(
                method = "repeatedcv",
                number = 10,
                repeats = 3, 
                summaryFunction = macroF1)

model_knn = train(trainSet[,predictors],
                  trainSet[,outcomeName],
                  method = 'knn', 
                  trControl = fitControlKNN,
                  metric = "F1",
                  tuneLength = 10)

predictors_reduced7 = c('edjefi', 'meaneduc', 'age_IQR', 'house_score','phone_per_capita',
                       'dependency', 'overcrowding')
model_knn_reduced7 = train(trainSet[,predictors_reduced7],
                          trainSet[,outcomeName],
                          method = 'knn', 
                          trControl = fitControlKNN,
                          metric = "F1",
                          tuneLength = 10)

predictors_reduced5 = c('edjefi', 'meaneduc', 'age_IQR', 'house_score','phone_per_capita')
model_knn_reduced5 = train(trainSet[,predictors_reduced5],
                          trainSet[,outcomeName],
                          method = 'knn', 
                          trControl = fitControlKNN,
                          metric = "F1",
                          tuneLength = 10)

## svm Radial
fitControlSVM = trainControl(
                method = "cv",
                number = 5,
                summaryFunction = macroF1,
                verboseIter = T)

model_svm = train(trainSet[,predictors],
                  trainSet[,outcomeName],
                  method = 'svmRadial', 
                  trControl = fitControlSVM,
                  metric = "F1",
                  tuneLength = 10)

## predictions and confusion matrix 

predictionsGBM = predict.train(model_gbm, testSet[,predictors], type = "raw")
cm = confusionMatrix(predictionsGBM, as.factor(testSet$Target), mode = "everything")
mean(cm$byClass[,7])

