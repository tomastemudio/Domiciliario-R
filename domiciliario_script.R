#==========================================================================
#                              DOMICILIARIO 2
#               Tomas Temudio, Lautaro Castares, Gonzalo
#==========================================================================

# Librerias
library(glue)
library(ggplot2)
library(Hmisc)

#-------------------------------------------------------------------------
#                             EJERCICIO 1
#-------------------------------------------------------------------------

set.seed(123)

## I)

simulaciones <- 1000

### Con n = 30
n1 <- 30
muestra_30 <- rnorm(n1, mean = 0, sd = 1)
resultado_30 <- c()

for (i in 1:simulaciones) {
  resultado_30[i] = mean(sample(muestra_30, replace = T))
}
media_30 <- mean(resultado_30)
sd_30 <- sd(resultado_30)
glue('n utilizado: {n1}; media: {media_30}; desvio estandar: {sd_30}')
hist(resultado_30, main = 'Histograma de resultado con n = 30', xlab = "Media", col = 'lightblue', prob = T, ylim = c(0,2.5))
lines(density(resultado_30), col = 'blue')

### Con n = 100

n2 <- 100
muestra_100 <- rnorm(n2, mean = 0, sd = 1)
resultado_100 <- c()

for (i in 1:simulaciones) {
  resultado_100[i] = mean(sample(muestra_100, replace = T))
}
media_100 <- mean(resultado_100)
sd_100 <- sd(resultado_100)
glue('n utilizado: {n2}; media: {media_100}; desvio estandar: {sd_100}')
hist(resultado_100, main = 'Histograma de resultado', xlab = "Media", col = 'lightblue', prob =T)
lines(density(resultado_100), col = 'darkgreen')

### Con n = 500

n3 <- 500
muestra_500 <- rnorm(n3, mean = 0, sd = 1)
resultado_500 <- c()

for (i in 1:simulaciones) {
  resultado_500[i] = mean(sample(muestra_500, replace = T))
}
media_500 <- mean(resultado_500)
sd_500 <- sd(resultado_500)
glue('n utilizado: {n3}; media: {media_500}; desvio estandar: {sd_500}')
hist(resultado_500, main = 'Histograma de resultado', xlab = "Media", col = 'lightblue', prob = T)
lines(density(resultado_500), col = 'red')

### Con n = 1000

n4 <- 1000
muestra_1000 <- rnorm(n4, mean = 0, sd = 1)
resultado_1000 <- c()

for (i in 1:simulaciones) {
  resultado_1000[i] = mean(sample(muestra_1000, replace = T))
}
media_1000 <- mean(resultado_1000)
sd_1000 <- sd(resultado_1000)
glue('n utilizado: {n4}; media: {media_1000}; desvio estandar: {sd_1000}')
hist(resultado_1000, main = 'Histograma de resultado', xlab = "Media", col = 'lightblue', prob = T, ylim = c(0,14))
lines(density(resultado_1000), col = 'pink')

## II)

densidad_30 <- density(resultado_30)
plot(densidad_30, col = 'blue', lwd=4, ylim = c(0,13), main = ("Grafico de densidad de Kernel"))

densidad_100 <- density(resultado_100)
lines(densidad_100, col = 'darkgreen', lwd=4)

densidad_500 <- density(resultado_500)
lines(densidad_500, col = 'red', lwd=4)

densidad_1000 <- density(resultado_1000)
lines(densidad_1000, col = 'pink', lwd=4)

legend('topright', legend = c('n=30', 'n=100', 'n=500', 'n=1000'), lwd = 4, col = c('blue', 'darkgreen', 'red', 'pink'))
minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)

## III)

### Con n = 30

n1_exp = 30
muestra_exp_30 = rexp(n1_exp, rate = 1)
resultado_exp_30 = c()

for (i in 1:simulaciones) {
  resultado_exp_30[i] <- mean(sample(muestra_exp_30, replace = T))
}
media_exp_30 <- mean(resultado_exp_30)
sd_exp_30 <- sd(resultado_exp_30)
hist(resultado_exp_30, main = 'Histograma de resultado con n = 30', xlab = "Media", col = 'lightgreen', prob = T)
lines(density(resultado_exp_30), col = 'blue')

### Con n = 100

n2_exp = 100
muestra_exp_100 = rexp(n2_exp, rate = 1)
resultado_exp_100 = c()

for (i in 1:simulaciones) {
  resultado_exp_100[i] <- mean(sample(muestra_exp_100, replace = T))
}
media_exp_100 <- mean(resultado_exp_100)
sd_exp_100 <- sd(resultado_exp_100)
hist(resultado_exp_100, main = 'Histograma de resultado con n = 100', xlab = "Media", col = 'lightgreen', prob = T,ylim = c(0,6))
lines(density(resultado_exp_100), col = 'darkgreen')

### Con n = 500

n3_exp = 500
muestra_exp_500 = rexp(n3_exp, rate = 1)
resultado_exp_500 = c()

for (i in 1:simulaciones) {
  resultado_exp_500[i] <- mean(sample(muestra_exp_500, replace = T))
}
media_exp_500 <- mean(resultado_exp_500)
sd_exp_500 <- sd(resultado_exp_500)
hist(resultado_exp_500, main = 'Histograma de resultado con n = 500', xlab = "Media", col = 'lightgreen', prob = T, ylim = c(0,10))
lines(density(resultado_exp_500), col = 'red')

### Con n = 1000

n4_exp = 1000
muestra_exp_1000 = rexp(n4_exp, rate = 1)
resultado_exp_1000 = c()

for (i in 1:simulaciones) {
  resultado_exp_1000[i] <- mean(sample(muestra_exp_1000, replace = T))
}
media_exp_1000 <- mean(resultado_exp_1000)
sd_exp_1000 <- sd(resultado_exp_1000)
hist(resultado_exp_1000, main = 'Histograma de resultado con n = 1000', xlab = "Media", col = 'lightgreen', prob = T, ylim = c(0,14))
lines(density(resultado_exp_1000), col = 'pink')

### Grafico de densidades de Kernel

densidad_exp_30 <- density(resultado_exp_30)
plot(densidad_exp_30, col = 'blue', lwd=4, ylim = c(0,13), main = ("Grafico de densidad de Kernel"))

densidad_exp_100 <- density(resultado_exp_100)
lines(densidad_exp_100, col = 'darkgreen', lwd=4)

densidad_exp_500 <- density(resultado_exp_500)
lines(densidad_exp_500, col = 'red', lwd=4)

densidad_exp_1000 <- density(resultado_exp_1000)
lines(densidad_exp_1000, col = 'pink', lwd=4)

legend('topright', legend = c('n=30', 'n=100', 'n=500', 'n=1000'), lwd = 4, col = c('blue', 'darkgreen', 'red', 'pink'))
minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)

#-------------------------------------------------------------------------
#                             EJERCICIO 2
#-------------------------------------------------------------------------
