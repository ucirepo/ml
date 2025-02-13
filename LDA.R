install.packages('caTools')
install.packages('caret')
install.packages('e1071')
install.packages('MASS')
library(MASS)
library(caret)
library(e1071)
library(caTools)

data = read.csv("C:/.csv")
set.seed(123)
split = sample.split(data$tcl, SplitRatio = 0.8)
split
training_set = subset(data,split == TRUE)
training_set
test_set = subset(data,split == FALSE)
test_set

training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


lda = lda(formula=tcl ~., data=training_set)
training_set = as.data.frame(predict(lda,training_set))
training_set = training_set[c(5,6,1)]
test_set = as.data.frame(predict(lda,test_set))
test_set = test_set[c(5,6,1)]

classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[-3])
y_pred

cm = table(test_set[, 3],y_pred)
cm

accuracy = sum(diag(cm)) / sum(cm)
accuracy






