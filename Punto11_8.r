
clc <- function(){
  cat("\014")
}

clc()

rm(list = ls())

x = c(39.00, 37.50, 22.20, 17.50, 0.64, 0.45, 2.62, 2.36, 32.00, 0.77)
mean(x)
y = c(23.00, 22.30, 9.40, 9.70, 0.15, 0.28, 0.75, 0.51, 28.00, 0.39)
mean(y)

plot(x, y, main = "Scatterplot")

mod = lm(y ~ x)
# summary(mod)

mod$coefficients
abline(mod, col = 3, lwd = 3)
 