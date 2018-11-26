library(dplyr)
library(caret)

## split test / train set
train_indices = sample(nrow(data_ft), nrow(data_ft)*0.80)
data_train = data_ft[train_indices,]
data_test = data_ft[-train_indices,]

## downsample / upsample
data_train_down = downSample(data_train, data_train$Target) %>%
  select(-c(Class))

## upsample
data_train_up = upSample(data_train, data_train$Target) %>%
  select(-c(Class))

## F1 score function
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


## kfold validation 
fitControl = trainControl(method = "cv",
                          number = 10,
                          summaryFunction = macroF1)


## random forest training
randomForestFit = train(as.factor(Target) ~ .,
                 data = data_train_up,
                 method = "rf",
                 trControl = fitControl,
                 tuneLength = 30,
                 metric = "F1")

randomForestFit
varImp(randomForestFit)

predictions = predict(randomForestFit, data_test)
confusionMatrix(predictions, data_test$Target, mode = "everything")
