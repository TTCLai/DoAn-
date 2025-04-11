library(tidyverse) 
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(skimr)
library(rpart)
library(rpart.plot)
library(tidyr)

## Load dữ liệu 
preprocessed_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

##  Loại bỏ các cột không sử dụng khi training
data <- preprocessed_data[, c(2:3, 5:13)] 

## Transform dứ liệu
data$Blood.Pressure <- sub("/", " ", data$Blood.Pressure)
data <- separate(data, Blood.Pressure, into = c("Systolic", "Diastolic"), sep = " ")
data$Systolic <- as.numeric(data$Systolic)
data$Diastolic <- as.numeric(data$Diastolic)

data$BMI.Category<- gsub("Normal Weight", "Normal", data$BMI.Category)

data$BMI.Category<- gsub("Obese", "Obesity", data$BMI.Category)

data$Sleep.Disorder[data$Sleep.Disorder == "Sleep Apnea"] = "Sleep.Apnea"
data <- data %>% transform(Sleep.Disorder = as.factor(Sleep.Disorder))

data$Gender <- as.factor(data$Gender)

data$BMI.Category <- as.factor(data$BMI.Category)

## Chia dữ liệu thành training và testing
set.seed(111)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train_data <- data[ind==1,]
test_data <- data[ind==2,]

## Chạy giải thuật Decision tree -----------------------------------------------------------------------------
decision_tree<- rpart(Sleep.Disorder ~ .,
                      data = train_data,
                      method = 'class',
                      xval = 10, # k-fold với k = 10
)

## In kết quả sau khi chạy giải thuật
printcp(decision_tree)
## Vẽ Cây 
rpart.plot(decision_tree,type=0, yesno = TRUE)
## Liệt kê các rules
rpart.rules(decision_tree)

## Vẽ đồ thị cp (cost complexity)
plotcp(decision_tree)

## Cắt tỉa cây với cp nhỏ nhất
library(caret)
decision_tree_prune <- prune(decision_tree, 
                             cp = decision_tree$cptable[which.min(decision_tree$cptable[, "xerror"]), "CP"])
# in cp
printcp(decision_tree_prune)
# Vẽ cây sau cắt tỉa
rpart.plot(decision_tree_prune, type=0, yesno = TRUE)
## Hiển thị rules sau khi ccatws tỉa
(rpart.rules(decision_tree_prune))

# Lưu model
saveRDS(decision_tree_prune, file = "DecisionTree.rds")













## Random Forest --------------------------------------------------------------------------------------------
library(randomForest)
## thuật toán
rf_model <- randomForest(Sleep.Disorder~., data = train_data, ntree = 1000, important = TRUE)
# Kết quả sau khi chay jmoo hình
rf_model


# Vẽ đồ thị biểu hiện các error của các lớp theo số lượng cây
oob_err_data <- data.frame(
  Trees = rep(1:nrow(rf_model$err.rate), 4), 
  Type = rep(c("OOB", "Insomnia", "None", "Sleep.Apnea"), each = nrow(rf_model$err.rate)),
  Error = c(rf_model$err.rate[,"OOB"], rf_model$err.rate[,"Insomnia"], rf_model$err.rate[,"None"], rf_model$err.rate[, "Sleep.Apnea"]))

ggplot(data = oob_err_data, aes(x = Trees, y= Error)) + geom_line(aes(color = Type))+ theme(text = element_text(size = 40)) + theme_minimal()

# tìm mtry
rf_fit <- train(Sleep.Disorder~.,
                data = train_data,
                method = "ranger",
                tuneLength = 12,
                metric = "ROC",
                trControl = trainControl(
                  method = "repeatedcv",  # k-fold cross validation
                  number = 10,  # 10 folds
                  savePredictions = "final", 
                  classProbs = TRUE,  
                  summaryFunction = defaultSummary  
                )
)
rf_fit
plot(rf_fit)

# xây dựng mô hình với mtry nhỏ nhất vừa tìm được
set.seed(111)
rf_model_2 <- randomForest(Sleep.Disorder~., data = train_data, mtry = rf_fit$bestTune$mtry, splitrule = rf_fit$bestTune$splitrule, importance = TRUE, ntree = 1000)
rf_model_2

# Lưu model
saveRDS(rf_model_2, file = "RFModel.rds")


##  Validation --------------------------------------------------

# Test validation of the Decision Tree
predict_decision_tree <- predict(decision_tree, newdata = test_data, type="class")
confusionMatrix(test_data$Sleep.Disorder, predict_decision_tree)


# Test validation of the Random Forest
predict_rf <- predict(rf_model_2, newdata = test_data, type = "class")
confusionMatrix(test_data$Sleep.Disorder,predict_rf)

