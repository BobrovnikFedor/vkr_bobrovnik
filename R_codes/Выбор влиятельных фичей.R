#### Пакеты ####
# install.packages(randomForest)
# install.packages(mlbench)
# install.packages(dplyr)
library(randomForest)
library(mlbench)
library(dplyr)

#### Выгрузка данных ####
train_x <- read.csv("~/Downloads/Diplom/Sorted_Way/train_x.csv")
test_x <- read.csv("~/Downloads/Diplom/Sorted_Way/test_x.csv")
target <- read.csv("~/Downloads/Diplom/Sorted_Way/target.csv")
train_x <- subset(train_x, select = -X)
test_x <- subset(test_x, select = -X)
target <- as.factor(target)
train_df <- data.frame(train_x, target)

#### Создание модели случайного леса ####
set.seed(444)
random_forest <- randomForest(target ~ ., data = train_df, proximity=TRUE)
random_forest

#### Создание оптимальной модели случайного леса ####
n_opt <- which.min(random_forest$err.rate[,1])
set.seed(444)
random_forest_opt <- randomForest(target ~ ., data = train_df, proximity=TRUE, 
                    ntree = n_opt)
random_forest_opt
imp <- importance(random_forest_opt)
varImpPlot(random_forest_opt)
imp <- data.frame(imp)
imp$index <- 1:length(imp$MeanDecreaseGini)
impt <- data.frame(imp[order(imp$MeanDecreaseGini,decreasing = TRUE),])
rowname_max <- rownames(imp[imp$MeanDecreaseGini>0.535,])

train_ <- dplyr::select(train_x, rowname_max)
test_ <- dplyr::select(test_x, rowname_max)

#### Запись файлов для обучения моделей ####
write.csv(train_,'diplom_train_x.csv')
write.csv(target,'diplom_train_y.csv')
write.csv(test_, 'diplom_all_sample.csv')