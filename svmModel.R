# tuner = svm(data_ft$Target ~., data=data_ft, cost = seq(1,100,1), gamma = seq(0.01,1,0.01))


# library(caret)
# library(nnet)

# multinomFit = multinom(train(as.factor(Target)~data_ft$overcrowding
#                             +data_ft$edjefi,data=data_ft))

vec <- vector()

for (i in 1:100) {
train_indices = sample(nrow(data_ft), nrow(data_ft)*0.80)
data_train = data_ft[train_indices,]
data_test = data_ft[-train_indices,]

model <- svm(data_train$Target~., data=data_train, cost = 100)
pred <- predict(model, data_train)
summary(pred)
pred <- predict(model, data_test)
summary(pred)

mean(pred == data_test$Target)
table(pred, data_test$Target)

F1_Score   

cm = as.matrix(table(data_test$Target,pred)) #confusion matrix
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
vec <- c(vec, macroF1_val)
}
# F1 Score
print(vec)