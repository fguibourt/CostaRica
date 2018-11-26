################################
#MULTINOMIAL LOGISTIC REGRESSION#
#################################

library(e1071)
library(nnet)

train_indices = sample(nrow(data_ft), nrow(data_ft)*0.80)
data_train = data_ft[train_indices,]
data_test = data_ft[-train_indices,]


# Fit the model
model <- nnet::multinom(Target ~
                          +data_train$overcrowding+
                          data_train$edjefi+
                          data_train$phone_per_capita+
                          data_train$meaneduc+
                          data_train$age_IQR, data = data_train)
summary(model)

# Make predictions

predicted.classes <- model %>% predict(data_test)
head(predicted.classes)

# Model accuracy
mean(predicted.classes == data_test$Target)

## F1 score
cm = as.matrix(table(data_test$Target, predicted.classes)) #confusion matrix
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
macroF1_val


## données pas très linéaires, ce modèle n'est donc pas adapté
