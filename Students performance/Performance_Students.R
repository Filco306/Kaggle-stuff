
# Calculates the maximum and minimum MSE obtained through the Naive Bayes category classifier. 
calc_max_MSE = function(conf_m) {
  max_SSE = 0
  min_SSE = 0
  score_step_by_class = (100/nrow(conf_m))
  max_possible_err_inside_class = score_step_by_class - 1
  
  n = sum(conf_m)
  for (i in 1:nrow(conf_m)) {
    for (j in 1:ncol(conf_m)) {
      # Take the distance from the diagonal
      if (i != j && conf_m[i,j] > 0) {
        min_SSE = min_SSE + (1 + abs((i - j - 1)*score_step_by_class))^2
        max_SSE = max_SSE + (max_possible_err_inside_class + abs(score_step_by_class*(i-j)))^2
      } else if (i == j) { # They can still be far away. 
        max_SSE = max_SSE + max_possible_err_inside_class^2
      }
      
      
    }
  }
  
  max_mse = (1/n)*max_SSE
  min_mse = (1/n)*min_SSE
  print(paste("The MSE for this model on the test set will lie between", min_mse, "and", max_mse))
  print(paste("This is a deviation of around", sqrt(min_mse), "and", sqrt(max_mse)))
}

get_grades = function(avg_score) {
  grades = as.character()
  for (i in 1:length(avg_score)) {
    if (avg_score[i] < 40) {
      grades[i] = "F"
    } else if (avg_score[i] < 50) {
      grades[i] = "E"
    } else if (avg_score[i] < 60) {
      grades[i] = "D"
    } else if (avg_score[i] < 70) {
      grades[i] = "C"
    } else if (avg_score[i] < 80) {
      grades[i] = "B"
    } else {
      grades[i] = "A"
    }
  }
  
  return(as.factor(grades))
}

calc_avg = function(data) {
  avg_score = as.numeric()
  for (i in 1:nrow(data)) {
    avg_score[i] = (data$math.score[i] + data$reading.score[i] + data$writing.score[i])/3
  }
  return(avg_score)
}

pass_or_fail = function(score) {
  pass = as.character()
  for (i in 1:length(score)) {
    if (score[i] >= 55) {
      pass[i] = "pass"
    } else {
      pass[i] = "fail"
    }
  }
  return(as.factor(pass))
}

data = read.csv("StudentsPerformance.csv")
data$average_score = calc_avg(data)
data$score_grp_5 = cut(data$reading.score, breaks = seq(0,100,by = 5), right = TRUE)
data$score_grp_5 = factor(data$score_grp_5, levels = levels(data$score_grp_5)[table(data$score_grp_5) > 0])
data$score_grp_2 = cut(data$reading.score, breaks = seq(0,100,by = 2), right = TRUE)
data$score_grp_2 = factor(data$score_grp_2, levels = levels(data$score_grp_2)[table(data$score_grp_2) > 0])
data$score_grp_4 = cut(data$reading.score, breaks = seq(0,100,by = 4), right = TRUE)
data$score_grp_4 = factor(data$score_grp_4, levels = levels(data$score_grp_4)[table(data$score_grp_4) > 0])
data$score_grp_10 = cut(data$reading.score, breaks = seq(0,100,by = 10), right = TRUE)
data$grade = get_grades(data$average_score)
data$pass = pass_or_fail(data$average_score)



data = data[,-c(6,7,8,9)]
n=dim(data)[1]
set.seed(123)
id=sample(1:n, floor(n*0.8))
train=data[id,]
test=data[-id,] 

# Upsample the failures in the training set

up_samples = sample(which(train$pass == "fail"), size = (sum(train$pass == "pass") - sum(train$pass == "fail")), replace = T)

up_samples = train[up_samples,]
train = rbind(train,up_samples)


#library(e1071)
library(caret)
train5 = train[,-c(7,8,9,10,11)]
test5 = test[,-c(7,8,9,10,11)]
#nb_model_5 = naiveBayes(score_grp_5~., data = train5)
nb_model_5 = train(x=train5[,-c(6)],y=train5[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_5, test5)
conf_m = table(Actual = test5$score_grp_5, Predicted = test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)

train2 = train[,-c(6,8,9,10,11)]
test2 = test[,-c(6,8,9,10,11)]
#nb_model_2 = naiveBayes(score_grp_2 ~., data = train2)
nb_model_2 = train(x=train2[,-c(6)],y=train2[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_2, test2)
conf_m = table(test2$score_grp_2, test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)

train4 = train[,-c(6,7,9,10,11)]
test4 = train[,-c(6,7,9,10,11)]
#nb_model_4 = naiveBayes(score_grp_4 ~., data = train4)
nb_model_4 = train(x=train4[,-c(6)],y=train4[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_4, test4)
conf_m = table(test4$score_grp_4, test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)

train10 = train[,-c(6,7,8,10,11)]
test10 = test[,-c(6,7,8,10,11)]
#nb_model_10 = naiveBayes(score_grp_10 ~., data = train10)
nb_model_10 = train(x=train10[,-c(6)],y=train10[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_10, test10)
conf_m = table(test10$score_grp_10, test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)

trainG = train[,-c(6,7,8,9,11)]
testG = test[,-c(6,7,8,9,11)]
#nb_model_G = naiveBayes(score_grp_G ~., data = trainG)
nb_model_G = train(x=trainG[,-c(6)],y=trainG[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_G, testG)
conf_m = table(testG$grade, test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)

trainPass = train[,-c(6,7,8,9,10)]
testPass = test[,-c(6,7,8,9,10)]
#nb_model_G = naiveBayes(score_grp_G ~., data = trainG)
nb_model_Pass = train(x=trainPass[,-c(6)],y=trainPass[,6], "nb", trControl = trainControl(method = "cv", number = 10))
test_preds = predict(nb_model_Pass, testPass)
conf_m = table(testPass$pass, test_preds)
print(paste("Misclassification rate is",1 - sum(diag(conf_m))/sum(conf_m)))
calc_max_MSE(conf_m)








######### OLD ANALYSIS ###########

MSE = function(y, y_hat) {
  return(mean((y - y_hat)^2))
}

avg_score_model = glm(average_score ~. - math.score - reading.score - writing.score, data = train)
summary(avg_score_model)
test_preds_avg_score = predict(avg_score_model, test)

# Least possible score is 1.27266e-26. Get close to that if you can!
sqrt(MSE(test$average_score, test_preds_avg_score))
# Get baseline for prediction of avg score



write_score_model = glm(writing.score~. - math.score - reading.score, data=train, family="gaussian")
summary(write_score_model)

math_score_model = glm(math.score~., data=train, family="gaussian")
summary(math_score_model)

pred_test = predict(write_score_model, test)

MSE(y=test$writing.score, y_hat = pred_test)

cov(data$math.score, data$writing.score)

####### Heuristic analysis ######
table(data$gender)

par(mfrow=c(2,1))
hist(data[data$gender == "male",]$math.score, breaks = 50, main = "Males math score")
hist(data[data$gender == "female",]$math.score, breaks = 50, main = "Females math score")
hist(data[data$gender == "male",]$writing.score, breaks = 50, main = "Males writing score")
hist(data[data$gender == "female",]$writing.score, breaks = 50, main = "Females writing score")
hist(data[data$lunch == "free/reduced",]$math.score, breaks = 50, main = "free lunch math score")
hist(data[data$lunch == "standard",]$math.score, breaks = 50, main = "standard lunch math score")

par(mfrow=c(1,1))
median(data[data$gender == "male",]$math.score)
median(data[data$gender == "female",]$math.score)
mean(data[data$gender == "male",]$math.score)
mean(data[data$gender == "female",]$math.score)
var(data[data$gender == "male",]$math.score)
var(data[data$gender == "female",]$math.score)

cov(data[,c(6,7,8)])


library(tree)

tree_model = tree(average_score ~. - writing.score - math.score - reading.score, data = train)
plot(tree_model)
text(tree_model, pretty=0)

tree_test_preds = predict(tree_model, test)
sqrt(MSE(test$average_score,tree_test_preds))

cv_tree = cv.tree(tree_model, K = 5)

plot(x=cv_tree$size, y=cv_tree$dev)

which.min(cv_tree$dev)

new_tree = prune.tree(tree_model, best=6)

test_preds_new = predict(new_tree, test)
sqrt(MSE(test$average_score, test_preds_new))