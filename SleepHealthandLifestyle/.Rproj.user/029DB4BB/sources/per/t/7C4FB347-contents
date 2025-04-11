
library(tidyverse) 
library(dplyr)
library(ggplot2)
library(GGally)

data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")


# data <- as.data.frame(data)

# Preprocessing data --------------
# Xóa cột ID
data <- data[, c(2:13)] 
# Xử lý cột dữ liệu Huyết áp
data$Blood.Pressure <- sub("/", " ", data$Blood.Pressure)
data <- separate(data, Blood.Pressure, into = c("Systolic", "Diastolic"), sep = " ")
data$Systolic <- as.numeric(data$Systolic)
data$Diastolic <- as.numeric(data$Diastolic)

# Xử lý cột BMI
data$BMI.Category<- gsub("Normal Weight", "Normal", data$BMI.Category)

data$BMI.Category<- gsub("Obese", "Obesity", data$BMI.Category)

data <- data %>% transform(Sleep.Disorder = as.factor(Sleep.Disorder))

# xu ly factor

# data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)

data$Sleep.Disorder[data$Sleep.Disorder == "None"] <- 0
data$Sleep.Disorder[data$Sleep.Disorder == "Sleep Apnea"] <- 2
data$Sleep.Disorder[data$Sleep.Disorder == "Insomnia"] <- 1

data$Sleep.Disorder <- as.numeric(data$Sleep.Disorder)

data$Gender <- as.factor(data$Gender)

data$Occupation <- as.factor(data$Occupation)

data$BMI.Category <- as.factor(data$BMI.Category)

head(data)


range(data$Age)
range(data$Sleep.Duration)
range(data$Quality.of.Sleep)
range(data$Physical.Activity.Level)
range(data$Stress.Level)
range(data$Systolic)
range(data$Diastolic)
range(data$Heart.Rate)
range(data$Daily.Steps)
range(data$Sleep.Disorder)




ggpairs(data, ggplot2::aes(colour = Sleep.Disorder))
# ggplot(data, aes(x = Sleep.Disorder, y = Quality.of.Sleep)) + geom_point()




# 1.	Mối quan hệ giữa nghề nghiệp và mức độ căng thẳng ảnh hưởng đến chất lượng giấc ngủ như thế nào? 
# (mức độ tương quan, box plot: x nghề nghiệp, y là mức độ căng thẳng)

ggplot(data, aes(colour = Occupation)) +
  geom_line(aes(x = Quality.of.Sleep, y = Stress.Level))+
  theme_minimal()

ggplot(data, aes(x = Quality.of.Sleep, y = Stress.Level, fill = Occupation, colour = Occupation)) +
  geom_bar(stat = "identity", position = "dodge")  +
  # geom_density(aes( y = ..count..), lwd = 1, alpha = 0.5, outline.type = "upper")+
  facet_wrap(~Occupation, scales = "free")  +
  labs(x = "Quality of Sleep", y = "Stress Level") + theme_minimal()



data %>% filter(Occupation == 'Manager') 

# 2.	Mối quan hệ giữa hoạt động thể chất tác động đến chất lượng giấc ngủ như thế nào?

ggplot(data, aes(x = Physical.Activity.Level, y = Quality.of.Sleep, fill = Occupation)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Occupation, scales = "free")  +
  labs(y = "Quality of Sleep", x = "Physical Activity Level") + theme_minimal()


# Chia dataset -------


set.seed(123)
spec = c(train = .7, test = .3)
g = sample(cut(
  seq(nrow(data)), 
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))
res = split(data, g)
train_data <- res$train
test_data <- res$test

addmargins(prop.table(table(g)))


# Decision Tree

# Load the MASS package
library(MASS)
library(tidymodels)
library(tidyr)
# 
# tree_spec <- decision_tree() %>%
#   set_engine("rpart") %>%
#   set_mode("classification")
# 
# tree_fit <- tree_spec %>%
#   fit(factor(Sleep.Disorder) ~ ., data = train_data)
# 
# # Make predictions on the testing data
# predictions <- tree_fit %>% predict(test_data) %>%  pull(.pred_class)
# 
# # Calculate RMSE and R-squared
# metrics <- metric_set(rmse, rsq)
# model_performance <- test_data %>%
#   mutate(predictions = predictions) %>%
#   metrics(truth = Sleep.Disorder, estimate = as.numeric(predictions))

# rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")
# 
# rules <- rpart.rules(tree_fit$fit)
# print(rules)
# 
# 
# # Load the necessary library
# library(vip)
# 
# # Create a variable importance plot
# var_importance <- vip::vip(tree_fit, num_features = 10)
# print(var_importance)


# Load the library
library(rpart)
library(rpart.plot)

# Plot the decision tree
fit <- rpart(Sleep.Disorder ~ ., 
             data = train_data, 
             method = 'class',
             xval = 10)
rpart.plot(fit, yesno = TRUE)

printcp(fit)
plotcp(fit)

predict_unseen <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Sleep.Disorder, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


# tune
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test_data, type = 'class')
  table_mat <- table(test_data$Sleep.Disorder, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(Sleep.Disorder~., data = train_data, method = 'class', control = control)
accuracy_tune(tune_fit)
