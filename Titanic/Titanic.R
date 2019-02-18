#### Some sketchy initial analyses and attempts of the titanic dataset. 

train = read.csv("train.csv")
test = read.csv("test.csv")
train$train_test = rep("train", nrow(train))
test$train_test = rep("test", nrow(test))

test$Survived = rep(NA, nrow(test))

all_data = rbind(train,test)

create_submission = function(preds, file_name) {
  write.csv(data.frame(PassengerId = seq(892,1309,1), Survived = as.integer(preds)), file_name, row.names = FALSE)
}

table(train$Pclass, train$Survived)


library(caret)

# all_data$Cabin_dep = apply(as.matrix(all_data$Cabin), 1, function(row) {
#   if (nchar(row) != 0) {
#     return(substr(row,1,1))
#   } else {
#     return("NA")
#   }
# })
# all_data$Cabin_dep
# table(all_data$Cabin_dep, all_data$Survived)
all_data$Cabin

all_data$Cabin_dec = apply(as.matrix(all_data$Cabin),1, function(string) {
  i = 1
  if (nchar(string) < 2) {
    return(0)
  }
  
  while(is.na(as.integer(substr(string,i,i)))) {
    i = i + 1
  }
  first = i
  
  while(!is.na(as.integer(substr(string,i,i))) || i == nchar(string)) {
   i = i + 1
  }
  last = i - 1
  
  cabin_no = substr(string,first,last)
  
  
  
  return(as.integer(substr(cabin_no,1,nchar(cabin_no)-1)))
})

all_data$Fare_below = ceiling(all_data$Fare) - (ceiling(all_data$Fare) %% 20) + 20
table(all_data$Fare_below, all_data$Survived)
plot(all_data[all_data$Survived == 1,]$Fare)
points(all_data[all_data$Survived == 0,]$Fare, col = "red")


hist(all_data$Age, freq=FALSE)



## Solve the 
dens_age = density(all_data[!is.na(all_data$Age),]$Age)
lines(dens_age)

# Fix fare

all_data[is.na(all_data$Fare),]$Fare = mean(all_data[!is.na(all_data$Fare),]$Fare)

all_data[is.na(all_data$Age),]$Age = sample(dens_age$x, size = sum(is.na(all_data$Age)), prob = (dens_age$y/sum(dens_age$y)))
all_data[is.na(all_data$Cabin_dec),]$Cabin_dec = 0


all_data$age_group = as.integer(ceiling(all_data$Age) - (ceiling(all_data$Age) %% 5) + 5)
all_data$Sex = as.integer(ifelse(all_data$Sex == "male", 1,-1))
train = all_data[all_data$train_test == "train",-c(1,4,6,9,11,12,13,15)]
test = all_data[all_data$train_test == "test",-c(1,4,6,9,11,12,13,15)]

train$Survived = as.factor(train$Survived)

####### START MODELLING #######


library(e1071)

nb = naiveBayes(Survived ~., train)

table(train$Survived, predict(nb,train))
sum(diag(table(train$Survived, predict(nb,train))))/sum(table(train$Survived, predict(nb,train)))
create_submission(as.integer(predict(nb, test)) - 1)


log_reg = glm(Survived~., train, family="binomial")
table(train$Survived, predict(log_reg,train))

library(MASS)
priors = (c(sum(train$Survived == "0"),sum(train$Survived == "1"))/nrow(train))
lda_model = lda(Survived ~., data = train, prior = priors)
preds_train = predict(lda_model, train)
table(train$Survived, preds_train$class)
test_preds = as.integer(predict(lda_model, test)$class)
test_preds[is.na(test_preds)] = sample(as.integer(c(1,2)),size = sum(is.na(test_preds)),prob = priors, replace = TRUE)
create_submission(as.integer(test_preds) - 1,"submission_lda.csv")

feat_PCA = prcomp(all_data[,-c(1,2,3,4,5,9,11,12,13,15,16)], scale = FALSE)

feat_PCA
explanations = (feat_PCA$sdev^2)/sum(feat_PCA$sdev^2)

PCA_data = data.frame(PassengerID = all_data$PassengerId,Survived = all_data$Survived, feat_PCA$x)

library(caret)

train_indices = 1:891
test_indices = 892:1309
x = as.matrix(data.frame(feat_PCA$x[,1:5],all_data[,c(3,5)]))
y = train$Survived
nb_pca = train(x[train_indices,], y[train_indices], method = "nb", trControl = trainControl(method = "cv", number = 10))

train_preds = predict(nb_pca, x[train_indices,])
table(train$Survived, train_preds)
sum(diag(table(train$Survived, train_preds)))/sum(table(train$Survived, train_preds))

test_preds = predict(nb_pca,x[test_indices])
create_submission(as.integer(test_preds) - 1, "submissions_nb.csv")

log_reg = train(x[train_indices,],y[train_indices], method="glm", family = "binomial", trControl = trainControl(method = "cv", number = 10))

train_preds = predict(log_reg, x[train_indices,])
table(train$Survived, train_preds)
sum(diag(table(train$Survived, train_preds)))/sum(table(train$Survived, train_preds))

test_preds = predict(log_reg,x[test_indices,])
create_submission(as.integer(test_preds) - 1, "submissions_logreg.csv")

plot(feat_PCA$x[all_data$Survived == 1,3], feat_PCA$x[all_data$Survived == 1,4], col = "blue")
points(feat_PCA$x[all_data$Survived == 0,3], feat_PCA$x[all_data$Survived == 0,4], col = "red")
