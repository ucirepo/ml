install.packages('caret')
install.packages('randomForest')
library(caret)
library(randomForest)
data("iris")
head(iris)
x <- iris[,1:4]
y <- iris[,5]
ctrl<- rfeControl(functions = rfFuncs, method ="cv",number=10)
result <- rfe(x, y, sizes=c(1:4),rfeControl=ctrl)
print(result)
plot(result)
predictors(result)

csv_path <- "C:/.csv"
data <- read.csv(csv_path)
head(data)
x <- data[, -ncol(data)]
y <- data[, ncol(data)]
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(x, y, sizes = c(1:ncol(x)), rfeControl = ctrl)
print(result)
plot(result)
predictors(result)
