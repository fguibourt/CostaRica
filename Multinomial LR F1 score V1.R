################################
#MULTINOMIAL LOGISTIC REGRESSION#
#################################


# Fit the model
model <- nnet::multinom(Target ~., data = data_ft)
summary(model)

# Make predictions
predicted.classes <- model %>% predict(data_ft_test)
head(predicted.classes)

# Model accuracy
mean(predicted.classes == data_ft_test$Target)

## accuracy of 68% to describe different categories

## Calcul du F1 Score

library(MLmetrics)
logreg <- glm(formula = Target ~ .,family = binoial(link = "logit"), data = data_ft)
pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)

F1_Score(y_pred = pred, y_true = as.numeric(as.character(data_ft$Target), positive = "0")
