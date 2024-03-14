# Данный скрипт запускается в облаке https://posit.cloud/ #

#### Пакеты ####
# install.packages(dplyr)
# install.packages(caret)
# install.packages(keras)
# install.packages(nnet)
# install.packages(randomForest)
# install.packages(mlbench)
# install.packages(e1071)
# install.packages(glmnet)
library(dplyr)
library(caret)
library(nnet)
library(randomForest)
library(mlbench)
library(e1071)
library(glmnet)

install.packages("remotes")
remotes::install_github("rstudio/tensorflow")
install.packages("reticulate")
install.packages("keras")
library(keras)
keras::install_keras(tensorflow = "cpu")

#### Выгрузка данных ####
diplom_train_x <- read.csv("/cloud/project/diplom_train_x.csv")
diplom_train_x <- subset(diplom_train_x, select = - X)
View(diplom_train_x) # 1000 публикаций 100 слов
diplom_train_y <- read.csv("/cloud/project/diplom_train_y.csv")
View(diplom_train_y) # вектор классов для трейна
diplom_all_sample <- read.csv("/cloud/project/diplom_all_sample.csv")
diplom_all_sample <- subset(diplom_all_sample, select = - X)
View(diplom_all_sample) # все публикации 100 слов

train_x <- diplom_train_x
train_y <- as.factor(diplom_train_y$x)
test <- diplom_all_sample
train <- data.frame(train_x,train_y)


#### Random Forest ####
set.seed(444)
mod_rf <- randomForest(train_y ~ ., data = train, proximity=TRUE)
mod_rf
n_opt <- which.min(mod_rf$err.rate[,1])
mod_rf_opt <- randomForest(train_y ~ ., data = train, proximity=TRUE, 
                           ntree = n_opt)
mod_rf_opt
rf_pr <- predict(mod_rf_opt,train_x)
confusionMatrix(rf_pr,train_y)
test_pr_rf <- predict(mod_rf_opt, test)

#### SVM ####
mod_svm <- svm(train_y ~ ., data = train, kernel = 'polynomial')
mod_svm
confusionMatrix(predict(mod_svm,train_x),train_y)
set.seed(444)
mod_tune <- tune.svm(train_y ~ ., data = train,kernel = 'polynomial',
                     coef0 = c(1/3,1/2,3/4),
                     cost = seq(1:10),
                     degree = 2:6)
mod_tune
mod_svm_opt <- svm(train_y ~ ., data = train, kernel = 'polynomial', coef0 =0.5, 
                   cost = 3, degree = 3)
mod_svm_opt
confusionMatrix(predict(mod_svm_opt,train_x),train_y)
test_pr_svm <- predict(mod_svm_opt, test)
write.csv(test_pr_svm, 'svm_pr.csv')

#### LASSO ####
cv_lasso = cv.glmnet(x = as.matrix(train_x), y = train_y,
                     family = 'multinomial',
                     alpha = 1,
                     type.measure = "class",
                     nfolds = 4)
best_lambda <- cv_lasso$lambda.min
best_lambda
lasso_opt <- glmnet(x = as.matrix(train_x), y = train_y,
                    family = 'multinomial',
                    alpha = 1, lambda = best_lambda)
pr_lasso <- predict(lasso_opt,as.matrix(train_x))
pr_lasso <- apply(pr_lasso, 1, which.max)
pr_lasso <- as.factor(pr_lasso - 1)
confusionMatrix(pr_lasso,train_y)
test_pr_lasso <- predict(lasso_opt, as.matrix(test))
test_pr_lasso <- apply(test_pr_lasso, 1, which.max)
test_pr_lasso <- as.factor(test_pr_lasso - 1)

#### Мультиномиальная регрессия ####
mod_multinom <- nnet::multinom(train_y ~., data = train)
confusionMatrix(predict(mod_multinom,train_x),train_y)
test_pr_multinom <- predict(mod_multinom, as.matrix(test))


#### Нейросеть ####
train_data <- as.matrix(train_x)
train_targets <- train_y
build_model <- function() {
  model <- keras_model_sequential() %>% layer_dense(units = 25, activation = "relu",
                                                    input_shape = 100) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 25, activation = 'relu')%>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = "softmax")
  model %>% compile(optimizer = "rmsprop", loss = 'categorical_crossentropy', 
                    metrics = 'accuracy')
}

all_acc_histories <- c()
for(i in 1:4) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  model <- build_model()
  history <- model %>% fit(partial_train_data, class.ind(partial_train_targets), epochs = 100,
                           batch_size = 1, verbose = 0, 
                           validation_data = list(val_data, class.ind(val_targets)))
  results <- model %>% evaluate(val_data, class.ind(val_targets), verbose = 0)
  acc_history <- history$metrics$val_accuracy
  all_acc_histories <- rbind(all_acc_histories, acc_history)
}
pr_nnet_2 <- predict(model, as.matrix(train_x))
pr_nnet_2 <- apply(pr_nnet_2, 1, which.max)
pr_nnet_2 <- as.factor(pr_nnet_2 - 1)
confusionMatrix(pr_nnet_2,train_y)

mod_net2 <- keras_model_sequential()%>% 
  layer_dense(input_shape = 100, units = 25, activation = "relu")%>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 25, activation = "relu")%>%
  layer_dense(units = 25, activation = "relu")%>%
  layer_dense(units = 4, activation = "softmax")
mod_net2 <- mod_net2 %>% compile(optimizer = 'rmsprop', loss = 'categorical_crossentropy',
                                 metrics = 'accuracy')
train_data <- as.matrix(train_x)
train_targets <- train_y
Result <- mod_net2 %>% fit(train_data, class.ind(train_targets), batch_size = 1, epochs = 500)
pr <- predict(mod_net2, train_data)
Y_pr <- apply(pr, 1, which.max)
Y_pr <- as.factor(Y_pr - 1)
confusionMatrix(Y_pr,train_targets)
test_pr_nnet <- predict(mod_net2, as.matrix(test))
test_pr_nnet <- apply(test_pr_nnet, 1, which.max)
test_pr_nnet <- as.factor(test_pr_nnet - 1)

#### Предикты ####
preds <- data.frame(test_pr_rf, test_pr_svm, test_pr_lasso, test_pr_multinom)
colnames(preds) <- c('rf','svm','lasso','multinom')
preds$nnet <- test_pr_nnet
preds <- dplyr::select(preds, c(3,4,5,6,8))
colnames(preds) <- c('rf','svm','lasso','multinom','nnet')
preds <- dplyr::select(preds, -c('lasso','multinom'))
View(preds)

true_preds <- c()
for (i in 1:dim(preds)[1]){
  if (preds$rf[i] == preds$svm[i] & preds$rf[i] == preds$nnet[i]){
    true_preds[i] <- preds$rf[i]
  } else if (preds$rf[i] == preds$svm[i] & preds$rf[i] != preds$nnet[i]){
    true_preds[i] <- preds$rf[i]
  } else if (preds$svm[i] == preds$nnet[i] & preds$rf[i] != preds$svm[i]){
    true_preds[i] <- preds$svm[i]
  } else if (preds$rf[i] == preds$nnet[i] & preds$rf[i] != preds$svm[i]){
    true_preds[i] <- preds$rf[i]
  } else if (preds$rf[i] != preds$nnet[i] & preds$rf[i] != preds$svm[i] &
             preds$svm[i] != preds$nnet[i]){
    true_preds[i] <- preds$svm[i]
  }
}

#### Запись файла с разметкой для всех публикаций ####
write.csv(true_preds, 'true_preds.csv')
