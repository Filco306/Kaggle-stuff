show_table_props = function(Table) {
  for (i in 1:nrow(Table)) {
    Table[i,] = Table[i,]/sum(Table[i,])
  }
  return(Table)
}

data = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
#table(data$SeniorCitizen)
data$SeniorCitizen = as.factor(data$SeniorCitizen)
data = data[!is.na(data$TotalCharges),]
table(data$Churn)/sum(table(data$Churn))
table(StreamTV = data$StreamingTV, StreamMovies = data$StreamingMovies, Churn = data$Churn)

for(i in c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18)) {
  print(colnames(data)[i])
  print(show_table_props(table(data[,i], Churn = data$Churn)))
}
par(mfrow=c(2,1))
hist(data[data$Churn == "Yes",]$TotalCharges, xlim = c(min(data[!is.na(data$TotalCharges),]$TotalCharges), max(data[!is.na(data$TotalCharges),]$TotalCharges)), breaks = 100)
hist(data[data$Churn == "No",]$TotalCharges, xlim = c(min(data[!is.na(data$TotalCharges),]$TotalCharges), max(data[!is.na(data$TotalCharges),]$TotalCharges)), breaks = 100)
hist(data[data$Churn == "Yes",]$MonthlyCharges, xlim = c(min(data[!is.na(data$MonthlyCharges),]$MonthlyCharges), max(data[!is.na(data$MonthlyCharges),]$MonthlyCharges)), breaks = 100)
hist(data[data$Churn == "No",]$MonthlyCharges, xlim = c(min(data[!is.na(data$MonthlyCharges),]$MonthlyCharges), max(data[!is.na(data$MonthlyCharges),]$MonthlyCharges)), breaks = 100)
hist(data[data$Churn == "Yes",]$tenure, xlim = c(min(data[!is.na(data$tenure),]$tenure), max(data[!is.na(data$tenure),]$tenure)), breaks = 100)
hist(data[data$Churn == "No",]$tenure, xlim = c(min(data[!is.na(data$tenure),]$tenure), max(data[!is.na(data$tenure),]$tenure)), breaks = 100)
par(mfrow=c(1,1))


log_reg = glm(Churn ~ PaymentMethod + Contract + PaperlessBilling + OnlineSecurity + SeniorCitizen, data = data[,2:21], family = "binomial")
summary(log_reg)
library(caret)


preds = predict(log_reg, data[,2:21], type = "response")
preds = as.factor(ifelse(preds >= 0.5, "Yes", "No"))

table(data$Churn, preds)
1 - sum(diag(table(data$Churn, preds)))/sum(table(data$Churn, preds))

library(e1071)

nb = naiveBayes(Churn ~., data = data[,2:21])
preds = predict(nb, data)
nb$tables
table(data$Churn, preds)
1 - sum(diag(table(data$Churn, preds)))/sum(table(data$Churn, preds))

prcomps = prcomp(data[,c(6,19,20)], scale = T)

explanationFactor = prcomps$sdev^2/sum(prcomps$sdev^2)

head(prcomps$x[,1:2])

data = cbind(data, prcomps$x)
orig_data = data
data = data[,c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,21,22,23,24)]

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.2))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

log_reg = glm(Churn ~ PaymentMethod + Contract + PaperlessBilling + OnlineSecurity + SeniorCitizen + PC1 + PC2, data = train, family="binomial")
preds_valid = predict(log_reg, valid, type = "response")
#preds_valid = as.factor(ifelse(preds_valid >= 0.5, "Yes", "No"))


Pi = as.matrix(seq(0,1,by=0.05))

classifications_valid = apply(Pi,1,function(pi) {
  return(ifelse(preds_valid > pi, 1, 0))
})

TPRs = apply(classifications_valid, 2, function(col) {
  return(TPR(predictions = as.factor(col), labels = valid$Churn))
})
FPRs = apply(classifications_valid, 2, function(col) {
  return(FPR(predictions = as.factor(col), labels = valid$Churn))
})

plot(FPRs, TPRs, main = "Logistic regression ROC curve", xlim = c(0,1), ylim = c(0,1))

prob_threshold = Pi[which.max(TPRs - FPRs),]
preds_valid = as.factor(ifelse(preds_valid >= prob_threshold, "Yes", "No"))
table(valid$Churn, preds_valid)
1 - sum(diag(table(valid$Churn, preds_valid)))/sum(table(valid$Churn, preds_valid))


preds_test = predict(log_reg, test, type = "response")

classifications_test = apply(Pi,1,function(pi) {
  return(ifelse(preds_test > pi, 1, 0))
})

TPRs = apply(classifications_test, 2, function(col) {
  return(TPR(predictions = as.factor(col), labels = test$Churn))
})
FPRs = apply(classifications_test, 2, function(col) {
  return(FPR(predictions = as.factor(col), labels = test$Churn))
})

preds_test = as.factor(ifelse(preds_test >= prob_threshold, "Yes", "No"))
1 - sum(diag(table(test$Churn, preds_test)))/sum(table(test$Churn, preds_test))



nb = naiveBayes(Churn ~ PaymentMethod + Contract + PaperlessBilling + OnlineSecurity + SeniorCitizen + PC1 + PC2, data = train)

nb_preds = predict(nb, valid, type = "raw")
table(test$Churn, nb_preds)
1 - sum(diag(table(test$Churn, nb_preds)))/sum(table(test$Churn, nb_preds))

Pi = as.matrix(seq(0,1,by=0.05))

classifications_valid = apply(Pi,1,function(pi) {
  return(ifelse(nb_preds[,2] > pi, 1, 0))
})

TPRs = apply(classifications_valid, 2, function(col) {
  return(TPR(predictions = as.factor(col), labels = valid$Churn))
})
FPRs = apply(classifications_valid, 2, function(col) {
  return(FPR(predictions = as.factor(col), labels = valid$Churn))
})

plot(x=FPRs, y = TPRs, main ="Naive Bayes ROC curve")
AUC = function(TPR, FPR) {
  # TPR is y, FPR is x
  # Order after FPR
  xInd = order(FPR)
  x = FPR[xInd]
  y = TPR[xInd]
  area = 0
  for (i in 2:length(TPR)) {
    area = (x[i]-x[i-1])*(y[i] + y[i-1])/2 + area
  }
  return(area)
}
AUC(TPRs, FPRs)
prob_threshold = Pi[which.max(TPRs - FPRs),]


# Perform best subset selection

plot(data[data$Churn == "Yes",]$PC1, data[data$Churn == "Yes",]$PC2, col = "blue", ylim = c(min(data$PC2), max(data$PC2)), xlim = c(min(data$PC1), max(data$PC1)))
points(data[data$Churn == "No",]$PC1, data[data$Churn == "No",]$PC2, col = "red", ylim = c(min(data$PC2), max(data$PC2)), xlim = c(min(data$PC1), max(data$PC1)))

x_train = model.matrix( ~. -1,train[,-c(17)])
y_train = train[,17]
x_test = model.matrix( ~. -1,test[,-c(17)])
y_test = train[,17]
library(glmnet)

lasso_model = cv.glmnet(x=x_train, y = y_train, alpha=1, family = "binomial")
coef(lasso_model, s = "lambda.min")
pred_test = predict(lasso_model, x_test, s="lambda.min", type = "response")

preds_test = as.factor(ifelse(pred_test >= 0.5, "Yes", "No"))
table(test$Churn, preds_test)
1 - sum(diag(table(test$Churn, preds_test)))/sum(table(test$Churn, preds_test))
plot(lasso_model)


svm_data = data[,c(17,18,19)]
n=dim(svm_data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
train=svm_data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.2))
valid=svm_data[id2,]
id3=setdiff(id1,id2)
test=svm_data[id3,]
library(kernlab)
C = c(0.5,1,5,10)
width = 0.5


for (c in C) {
  svm_model = ksvm(Churn~.,data = train, cross=4, kernel="polydot", kpar=list(degree=4, scale = 1, offset = 2), C = c)
  val_preds= predict(svm_model, valid)
  print(table(valid$Churn, val_preds))
}


loss_binary_classification = function(FP_punishment, FN_punishment, probabilities) {
  if (dim(probabilities)[2] == 2) {
    probs_positive = probabilities[,2]
  } else {
    probs_positive = probabilities
  }
  return(apply(as.matrix(probs_positive, 1, function(row) {
    losses = c(FN_punishment*(1 - row[1]), FP_punishment**(1 - row[2]))
  })))
}

testClassifsWLoss = apply(predictionsTest, 1, function(row) {
  losses = c(1 - row[1], 10*(1 - row[2]))
  # c(bad, good)
  return(which.min(losses))
})
