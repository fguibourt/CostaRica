library(xgboost)
library(tidyverse)

data_xgb = as.data.frame(data_ft)
data_xgb = subset(data_xgb, select = -c(hacdor, refrig, official_union, lugar_region,male))
data_xgb$Target = as.integer(data_xgb$Target)



# Make split index
train_index <- sample(1:nrow(data_xgb), nrow(data_xgb)*0.75)
# Full data set
data_variables <- as.matrix(data_xgb[,-21])
data_label <- data_xgb[,"Target"]-1
data_matrix <- xgb.DMatrix(data = as.matrix(data_xgb), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


numberOfClasses <- length(unique(data_xgb$Target))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "merror",
                   "num_class" = numberOfClasses)
nround    <- 500 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE,
                   tree_method = "hist",
                   nthread = 4)


OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)


# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround,
                       tree_method = "hist")

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")



# get the feature real names
names <-  colnames(data_xgb[,-1])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

table(factor(OOF_prediction$max_prob),
      factor(OOF_prediction$label))

cm = as.matrix(table(factor(OOF_prediction$max_prob),
                     factor(OOF_prediction$label))) #confusion matrix
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
