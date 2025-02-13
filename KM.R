install.packages("survival")

library(survival)

dat <- data.frame(
  time = c(3, 4),
  event = c(1, 1)
)

km <- survfit(Surv(time, event) ~ 1, data = dat)
summary(km)

plot(km, xlab = "Time")

