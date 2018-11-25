############################
#### FEATURE SELECTION #####
############################
attach(data_ft)

###### BORUTA ########
library(Boruta)

set.seed(123)
boruta.train = Boruta(Target~., data = data_ft, doTrace = 2)
print(boruta.train)

## plot the results
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

ft_selected = c(getSelectedAttributes(boruta.train),"Target")

## Clean data_ft

data_ft = select(data_ft, ft_selected)
