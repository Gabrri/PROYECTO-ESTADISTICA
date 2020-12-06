rm(list = ls())

clc <- function(){
  cat("\014")
}

clc()

n_i = c(89, 18, 12, 81)
p_i = c(0.41, 0.1, 0.04, 0.45)
n = sum(n_i)
alpha = 0.05
gl = length(n_i) - 1
X_2 = sum(((n_i - n * p_i) ^ 2) / (n * p_i))

pr = chisq.test(n_i, p = p_i)
q = pr$statistic
pchisq(q, df = gl, lower.tail = FALSE)
qchisq(0.05, df = gl, lower.tail = FALSE)

