data('mtcars')
head(mtcars)
View(mtcars)
model <- lm(mpg~disp+hp+wt+drat,data=mtcars)
summary(model)
library(car)
vif(model)
vif_values <- vif(model)
barplot(vif_values,main='VIF Values',horiz=TRUE,col='steelblue')
abline(v=5,lwd=3,lty=2)
data<-mtcars[,c('disp','hp','wt','drat')]
cor(data)
 

create_vif_plot <- function(csv_file_path, response_var, predictor_vars) {
  data <- read.csv(csv_file_path)
  formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + ")))
  model <- lm(formula, data = data)
  vif_values <- vif(model)
  barplot(vif_values, main = 'VIF Values', horiz = TRUE, col = 'steelblue')
  abline(v = 5, lwd = 3, lty = 2)
  print(vif_values)
}
csv_file_path <- ".csv"  
response_var <- ""  
predictor_vars <- c("x", "y")  

create_vif_plot(csv_file_path, response_var, predictor_vars)
