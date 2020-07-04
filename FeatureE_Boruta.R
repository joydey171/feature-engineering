
#boruta and Recursive feature selection

library(Boruta)

#load the data set

setwd("../Data/loan")
traindata <- read.csv("train.csv", header = T, stringsAsFactors = F)

#removing missing values 

traindata[traindata == ""] <- NA
traindata <- traindata[complete.cases(traindata),]

#converting categorical to factor

convert <- c(2:6, 11:13)
traindata[,convert] <- data.frame(apply(traindata[convert], 2, as.factor))

#applying boruta

set.seed(123)
boruta.train <- Boruta(LoanStatus~.-LoanID, data = traindata, doTrace = 2)


#plotting boruta feature impotance 


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)



#Recursive feature selection using caret

library(caret)
library(randomForest)
set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

rfe.train <- rfe(traindata[,2:12], traindata[,13], sizes=1:12, rfeControl=control)

rfe.train

#visulaize impotance

plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)