
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
x_ <- mean(x)
x_

# a) conocemos sigma^2 (varianza)
# INTERVALO DE CONFIANZA AL 90%

n <- length(x)
sigma <- sqrt(25)
z005 <- qnorm(0.95)
c(x_ - z005*sigma/sqrt(n),x_ + z005*sigma/sqrt(n))

# instalar
install.packages("BSDA")
library(BSDA)

z.test(x, sigma.x =sigma, conf.level=0.90)


#-------------------------------------------------------------------------------
#PRUEVA DE HIPOTESIS (estoy llenando la caja de macarrones con muchos/pocos macarrones)
confianza <- 0.95
alfa <- 1-confianza <- 0.05 # nivel se significación estadistica

#aprovamos hipotesis alternativa

n <- length(x)
sigma <- sqrt(25)
z005 <- qnorm(0.95)
zc <- qnorm(0.975)
zc

zobs <- (x_ -500)/(sigma/sqrt(n))
zobs

library(BSDA)
z.test(x, sigma.x =sigma, mu=500)

#-------------------------------------------------------------------------------


# b) confianza=0.95 => alfa= 0.05

n <- (qnorm(0.975)* sigma)^2
n
# n= 96.03 => n= 97(nº entero) 

# c) no conocemos sigma^2 (varianza) / confianza=0.99 ; alfa = 0.005

x_ <- mean(x)
s <- sd(x)
n <- length(x)
t0005 <- qt(0.995, n-1)
c(x_ - t0005*s/sqrt(n),x_ + t0005*s/sqrt(n))

library(BSDA)
t.test(x, conf.level=0.099)

#-------------------------------------------------------------------------------
#provemos que la media del peso de las cajas es diferente a 500g

library(BSDA)
t.test(x, mu=500)

#p_valor = 0.02 
#alfa =0.01

#p_valor > alfa ==> acepto hipotesis nula (H0)

#-------------------------------------------------------------------------------

# prueva de dos colas 

#COLA SUPERIOR (greater) => #H1: mu > 500g 

#H0: mu < 500g
