getwd()
setwd("Documents/Kaggle/WCB/")

#install.packages('ggplot')
#install.packages('caret')
#install.packages('e1071')
#install.packages('ROCR')
library(caret)
library(corrplot)
library(ggplot2)
library(e1071)
library(ROCR)

wdbc <- read.csv("wdbc.csv.csv",header = T,stringsAsFactors = F)
wdbc <- wdbc[-33]
symptoms <- wdbc[,2]
wdbc <- wdbc[,-c(1:2)]

wdbc.cor <- cor(wdbc)

corrplot(wdbc.cor,method = 'circle',order = "FPC")

HighCorr <- findCorrelation(wdbc.cor, cutoff = .85)
"findCorr :- Returns vector of Integers corresponding to col by reducing pairwise correlations"


length(HighCorr)
#we have 13 highly correlated predictors to be removed
wdbc_filter <- wdbc[, -HighCorr]
wdbc_trans <- preProcess(wdbc_filter, method = c("BoxCox", "center", "scale"))

#apply the trasnformation
wdbc_trans <- predict(wdbc_trans, wdbc_filter)

head(wdbc_trans)

"Split the data"
"75:25"
wdbc_index <- createDataPartition(y = symptoms,p = 0.75,list = FALSE)
wdbc_trai <- wdbc_trans[wdbc_index,]
wdbc_test <- wdbc_trans[-wdbc_index,]

ctrl <- trainControl(method="repeatedcv",repeats = 5) 
knnFit <- train(y= symptoms[wdbc_index] , 
                x = wdbc_trai, method = "knn", 
                trControl = ctrl, 
                tuneLength = 10)

plot(knnFit)
knnFit

#Get the confusion matrix to see accuracy value and other parameter values
knnPredict <- predict(knnFit,newdata =wdbc_test)

confusionMatrix(knnPredict, symptoms[-wdbc_index])

"Split 70:30"
wdbc_index1 <- createDataPartition(y = symptoms,p = 0.70,list = FALSE)

wdbc_trai1 <- wdbc_trans[wdbc_index1,]
wdbc_test1 <-wdbc_trai[-wdbc_index1,]

ctrl1 <- trainControl(method="repeatedcv",repeats = 5) 
knnFit1 <- train(y= symptoms[wdbc_index1] , 
                x = wdbc_trai1, method = "knn", 
                trControl = ctrl, 
                tuneLength = 10)

plot(knnFit1)
knnFit1


#Get the confusion matrix to see accuracy value and other parameter values
knnPredict1 <- predict(knnFit1,newdata = wdbc_test1)
table(knnPredict1)
knnPredict1


