rm(list = ls())

clc <- function(){
  cat("\014")
}

clc()

library(ISLR)
data(Auto)
head(Auto)

### a)

# pairs(Auto)


### b)

x = Auto$horsepower
y = Auto$acceleration
plot(x, y, main = "scatterplot")
abline(lm(y ~ x), col = 3, lwd = 3)

# 1)

# pairs(data.frame(x, y))

# 2)

x_bar = mean(x)
y_bar = mean(y)

s_xy = sum((x - x_bar) * (y - y_bar))
s_xx = sum((x - x_bar) ^ 2)
s_yy = sum((y - y_bar) ^ 2)

beta1 = s_xy / s_xx
beta0 = y_bar - beta1 * x_bar

# 3)

# H_0: beta1 = 0
# H_a: beta1 != 0
n = length(x)
df = n - 2
alpha = 0.05
SSE = s_yy - beta1 * s_xy
S = sqrt(SSE / (n - 2))
c_11 = 1 / s_xx
T_est = beta1 / (S * sqrt(c_11)) # Cae en la region de rechazo, luego beta1 != 0
t_alpha_2 = qt(alpha / 2, df, lower.tail = F)

# 4)

izq1 = beta1 - t_alpha_2 * S * sqrt(c_11)
der1 = beta1 + t_alpha_2 * S * sqrt(c_11)

# 5) Luego de la hipotesis, la evidencia sugiere que si hay uns relacion entre
#    entre horsepower y acceleration para los autos

# 6) 

x_ast = x[100]
y_ast = y[100]
izq2 = beta0 + beta1 * x_ast - t_alpha_2 * S * sqrt((1 / n) + ((x_ast - x_bar) ^ 2) / s_xx)
der2 = beta0 + beta1 * x_ast + t_alpha_2 * S * sqrt((1 / n) + ((x_ast - x_bar) ^ 2) / s_xx)

# 7) predict x_p = 213

x_p = 213
y_p = beta0 + beta1 * x_p
mod = lm(y ~ x)
predict(mod, newdata = data.frame(x = c(x_p)))
izq3 = beta0 + beta1 * x_p - t_alpha_2 * S * sqrt(1 + (1 / n) + ((x_p - x_bar) ^ 2) / s_xx)
der3 = beta0 + beta1 * x_p + t_alpha_2 * S * sqrt(1 + (1 / n) + ((x_p - x_bar) ^ 2) / s_xx)

# 8) El intervalo de prediccion es mas grande que el confianza

# 9) 

r = beta1 * sqrt(s_xx / s_yy)
r_cuad1 = r ^ 2
r_cuad2 = 1 - SSE / s_yy

# 10) En un 47%, la variacion de acceleration se explica al ajustar el modelo 
#     lineal simple, usando horsepower como la variable independiente.
#     Como r es negativo concluimos que los autos con mas horsepower tienden a 
#     tener menos acceleration


# c) Ya anteriormente
