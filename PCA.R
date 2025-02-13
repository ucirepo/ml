install.packages('caTools')
install.packages('caret')
install.packages('e1071')
library(caret)
library(e1071)
library(caTools)

data = read.csv("C:/.csv")
View(data)
set.seed(123)
split = sample.split(data$tcl, SplitRatio = 0.8)
split
training_set = subset(data,split == TRUE)
training_set
test_set = subset(data,split == FALSE)
test_set

training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


pca = preProcess(x = training_set[-14], method = 'pca',pcaComp=2)
training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]
test_set = predict(pca,test_set)
test_set = test_set[c(2,3,1)]

classifier = svm(formula =tcl ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[-3])
y_pred

cm = table(test_set[, 3],y_pred)
cm

accuracy = sum(diag(cm)) / sum(cm)
accuracy

