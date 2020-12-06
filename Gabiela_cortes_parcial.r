rm(list = ls())

tabla = read.csv(file.path("/Users/GabrielaCortes/Desktop", "data.txt"))
x= data.frame(length(tabla-1),tabla)
y= data.frame(length(tabla-1),tabla)
x_barra = mean(x)
y_barra = mean(y)

s_xy = sum((x - x_barra) * (y - y_barra))
s_xx = sum((x - x_barra) ^ 2)
s_yy = sum((y - y_barra) ^ 2)

beta1 = s_xy / s_xx
beta0 = y_barra - beta1 * x_barra

############################################

r = beta1 * sqrt(s_xx / s_yy)
r_cuad1 = r ^ 2
r_cuad2 = 1 - SSE / s_yy
############################################

x_value = 20
y_value = beta0 + beta1 * x_value
mod = lm(y_value ~ x)
predict(mod, newdata = data.frame(x = c(x_value)))