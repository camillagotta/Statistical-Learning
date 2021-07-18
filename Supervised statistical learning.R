
#upload necessary libraries
library(stats)
library(rsample)
library(ggplot2)
library(neuralnet)
library(dplyr)
library(stats)
library(plyr)
library(caret)
library(boot)
library(car)
library("Hmisc")
library(corrplot)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ROCR)
library(e1071)
library(randomForest)
library(pROC)
library(tree)
library(ISLR)

#upload balanced dataset
data<-read.csv(file = "C:/Users/USER/Downloads/ddos_dataset/ddos_dataset.csv")
summary(data)
count(data, data$Label)

#take only columns of interest
data<- data[c(3:8)]
colnames(data)

#remove NA values
data <- na.omit(data)
data

#which variables are more correlated?
res<- cor(data) 
round(res, 2)

#pvalue
symnum(res, abbr.colnames = FALSE)

rcorr(as.matrix(data))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res1<-rcorr(as.matrix(data))

flattenCorrMatrix(res1$r, res1$P)

#enlarging margins
par("mar")
par(mar=c(1,1,1,1))

#heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE, Colv = NA, Rowv = NA)

corrplot(res, type = "upper", method= "circle",
         tl.col = "black",tl.srt = 90,tl.cex = 1)

#Factor
data$Label<-as.factor(data$Label)
data$Label <- ifelse(data$Label=='benign',0,1)

# split data in train and test set (70%)
set.seed(123)
split_train_test = createDataPartition(data$Label, p = 0.7, list = FALSE)

dtrain = data[split_train_test, ]
dtest = data[-split_train_test, ]

#logistic regression
lr_model <- glm(Label~., data = dtrain, family = binomial(link = "logit"))
summary(lr_model)

#check for multicollinearity
vif(lr_model)
alias(lr_model)

#the model is recognizing that Fwd.Seg.Size.Avg is identical, 
#or perfectly predicted by the combination of other predictors
#(highly correlated variables). 
#This means that we cannot include that terms in the model.

lr_model <- glm(Label~Fwd.Pkt.Len.Mean+
                  Init.Fwd.Win.Byts+Init.Bwd.Win.Byts+Fwd.Seg.Size.Min,
                data = dtrain, family = binomial(link = "logit"))
summary(lr_model)

#prediction & test error
lr_prob <- predict(lr_model, dtest, type = "response")
lr_pred <- ifelse(lr_prob > 0.5,"benign","ddos")
table_test = table(Predicted = lr_pred, Actual = dtest$Label)
table_test

#confusion matrix

confusionMatrix(factor(lr_pred),
                factor(dtest$Label),
                positive = "Benign")

#AUC curve
test_roc = roc(dtest$Label~lr_prob,plot = TRUE,
               print.auc = TRUE,percentage=TRUE,
               col="#377eb8", lwd=4)

#Decision Tree 
dt_model <- tree(Label~.,data = dtrain, method="class")
summary(dt_model)

#plot decision tree
binary.model <- rpart(Label~., data = dtrain,cp=0.001)
rpart.plot(binary.model)

#prediction & test error
dt_prob <- predict(dt_model, dtest, method = "class")
dt_pred <- ifelse(dt_prob  > 0.5,"benign","ddos")
table_test_dt = table(Predicted = dt_pred, Actual = dtest$Label)
table_test_dt

#confusion matrix
confusionMatrix(as.factor(dt_pred),
                as.factor(dtest$Label),
                positive = "Benign")

#AUC curve
dt_results <- cbind.data.frame(attacktype1=dtest$Label,dt_pred)
res.roc1 <- roc(as.numeric(dt_results$attacktype1), 
                as.numeric(dt_results$dt_pred))
plot.roc(res.roc1, col=2, print.auc=TRUE)

# Random Forest
rf_model <- randomForest(Label~.,ntree=50, mtry=3, importance=TRUE,
                         data=dtrain, method="class")  
rf_model 

#List of the most important variables used in our Random Forest
varImp(rf_model)
varImpPlot(rf_model)

#prediction & test error
rf_prob <- predict(rf_model, dtest, method = "class")
rf_pred <- ifelse(rf_prob  > 0.5,"benign","ddos")
table_test_dt = table(Predicted = rf_prob, Actual = dtest$Label)
table_test_dt

#confusion matrix
confusionMatrix(as.factor(rf_prob),
                as.factor(dtest$Label),
                positive = "Benign")


#ROC curve 
rf_results <- cbind.data.frame(attacktype=dtest$Label,rf_pred)
res.roc <- roc(as.numeric(rf_results$attacktype), 
               as.numeric(rf_results$rf_pred))
plot.roc(res.roc, col=3, print.auc = TRUE)

#artificial neural networks
#take a sample
data_s <- sample_n(data,5000)
data_s
count(data_s, data_s$Label)

#ddos=0, benign=1
#data$Label<- ifelse(data$Label=='ddos',0,1)
#normalized values
#data has been normalized in order to be adjusted 
#to a common scale so as to compare with more accuracy 
#predicted and actual values. 
#The technique is the max-min normalization one

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(data_s, normalize))
maxmindf

#Split data into Train and Test dataset (70% training)

set.seed(33)  
dt = sort(sample(nrow(maxmindf), nrow(maxmindf)*.7))
dtrain<- maxmindf[dt,]
dtest<- maxmindf[-dt,]

#count(dtest, dtest$Label)
#count(dtrain, dtrain$Label)

prop.table(table(dtrain$Label))
prop.table(table(dtest$Label))

#Implement ANN on data
#The threshold is set to 0.01, implying that if the change 
#in error during an iteration is less than 0.01 then 
#no further optimization is done by the model

nn_model <- neuralnet(Label~.,data=dtrain,
                      hidden=c(3,2), linear.output=FALSE,threshold=0.01)
ann_pred <- predict(nn_model, dtest)

nn_prediction <- ifelse(ann_pred > 0.5, 1, 0)
table_test = table(Predicted = nn_prediction, Actual = dtest$Label)
results <- data.frame(actual = dtest$Label, prediction = nn_prediction)
nn$result.matrix
results

nn <- neuralnet(Gender~.,data=dtrain,hidden=c(9), 
                linear.output=FALSE,threshold=0.01)
pred <- predict(nn, dtest)
nn_prediction <- ifelse(pred > 0.5, 1, 0)
table_test = table(Predicted = nn_prediction, Actual = dtest$Gender)
results <- data.frame(actual = dtest$Gender, prediction = nn_prediction)
nn$result.matrix
results


#confusion matrix
confusionMatrix(ann_pred, positive = "benign")
confusionMatrix(factor(mapvalues(nn_prediction, 
                                 from=c("0", "1"),
                                 to=c("ddos", "benign"))),
                factor(mapvalues(dtest$Label, 
                                 from=c("0", "1"),
                                 to=c("ddos", "benign"))))

#roc curve
test_roc_nn = roc(dtest$Label~ann_pred,plot = TRUE, print.auc = TRUE)
test_roc_nn
as.numeric(test_roc_nn$auc)

# plot the results
plot(nn_model)
