### Práctica 1
### 
### Contrastes no paramétricos clásicos

# El contraste más sencillo y aplicable en las condiciones más generales posibles
# es el contraste de signos. Al estar basado en la distribución binomial,
# se puede aplicar utilizando simplemente el comando binom.test
# Una versión más elaborada se encuentra en la función SignTest del paquete DescTools

# Ejemplo: Los tiempos de supervivencia, en semanas, de n = 10 pacientes con un 
# tipo de linfoma fueron 49, 58, 75, 110, 112, 132, 151, 276, 281, 362+. 
# Contrastar si puede suponerse que la mediana del tiempo de supervivencia es 
# de m_0 = 200 semanas.

library(DescTools)
tiempos <- c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362)
SignTest( x=tiempos, mu=200, alternative="two.sided" )

# La función SingTest también se puede utilizar para el caso de muestras apareadas
# Ejemplo: Se somete a 7 pacientes con temor al vómito a cierto tratamiento. 
# El efecto que se desea obtener con esta terapia es la reducción de la ansiedad 
# causada por el vómito. Cada paciente pasó un test, antes y después del tratamiento,
# que evaluaba su sensación de temor (valores altos indican más temor). 
# ¿Puede afirmarse que el tratamiento tuvo los efectos deseados?

antes <- c(10.6, 7.9, 12.4, 16.8, 13.2, 14.7, 18.34)
despues <- c(7.15, 9.36, 6.27, 7.19, 5.45, 6.21, 8)
SignTest( x=antes, y=despues, mu=0, alternative="greater" )

# Un contraste más potente, pero aplicable cuando la distribución es simétrica,
# es el contraste de rangos signados de Wilcoxon.
# Está implementado en la función wilcox.test del paquete stats

# Ejemplo: Aplicar el contraste de rangos signados a los datos del vómito.

wilcox.test(x=antes, y=despues, mu=0, paired=TRUE, alternative="greater")

# El contraste de desplazamiento de medianas se basa en el estadístico de suma 
# de rangos de Wilcoxon, o equivalentemente en el estadístico U de Mann-Whitney
# Se obtiene también mediante la función wilcox.test, para muestras independientes

# Ejemplo: Supongamos que tenemos muestras X = (1.69, 1.67, 1.74, 1.63, 1.65) e 
# Y = (1.73, 1.92, 1.85, 1.8), independientes, que provienen de distribuciones que sólo
# difieren en un desplazamiento de medianas. Contrastar H_0: m_X = m_Y frente a la
# alternativa H_1: m_X < m_Y.

muestrax <- c(1.69, 1.67, 1.74, 1.63, 1.65)
muestray <- c(1.73, 1.92, 1.85, 1.8)
wilcox.test(x=muestrax, y=muestray, mu=0, paired=FALSE, alternative="less")

# El estadístico de suma de rangos de Kruskal-Wallis para contrastar la igualdad
# de más de dos medianas se encuentra en la función kruskal.test del paquete stats

# Ejemplo: Disponemos de 4 métodos de cultivo del maíz ensayados en n = 74 parcelas
# de cultivo experimental. Tenemos una variable respuesta de tipo continuo que mide
# el rendimiento del cultivo, de la cual se han observado los siguientes datos.
# Contrastar la hipótesis H_0: m_1 = m_2 = m_3 = m_4.

rendimientos <- c(83, 91, 94, 89, 89, 96, 91, 92, 90, 93, 91, 95,
  86, 91, 89, 92, 90, 91, 90, 81, 83, 84, 83, 88, 91, 89, 84,
  82, 86, 84, 84, 91, 89, 90, 91, 83,101,100, 91, 93, 96, 95,
  94, 98, 95, 92, 80, 79, 82, 78, 82, 81, 77, 79, 81, 80, 81,
  81, 81, 94, 96, 101,81, 78)
metodos <- as.factor(c(rep(1,17), rep(2,19), rep(3,13), rep(4,15)))
datos <- data.frame(rendimientos, metodos)
tapply(rendimientos, metodos, median)
kruskal.test(rendimientos~metodos, data=datos)

# El contraste de Kruskal-Wallis detecta claramente diferencias significativas entre los métodos.
# Para realizar las comparaciones múltiples usaremos el contraste de Conover-Iman, disponible
# mediante la función kwAllPairsConoverTest del paquete PMCMRplus

library(PMCMRplus)
kwAllPairsConoverTest(rendimientos~metodos, datos)

# Se puede observar que hay diferencias significativas entre el método 4 y el 1 y el 3, y también 
# entre el 2 y el 3, y ninguna más

# El contraste de Friedman para más de dos medianas de muestras relacionadas en un diseño
# completamente aleatorizado está disponible en la función friedman.test del paquete stats

# Ejemplo: Se quiere comparar la velocidad de 3 impresoras midiendo el tiempo de impresión
# de 5 documentos. Contrastar la hipótesis de que las 3 impresoras son igualmente rápidas.

datosimp <- matrix(
  c(4.7, 3.8,    8, 13.6, 18.1,
    6.8, 8.5,   13, 16.8, 26.7,
      9, 9.5, 12.9, 17.7, 23.6
  ),
  ncol=3
)
grupos <- paste("Impr", 1:3, sep="")
bloques <- paste("Doc", 1:5, sep="")
colnames(datosimp) <- grupos
rownames(datosimp) <- bloques
apply(datosimp, 2, median)
datosimp2 <- data.frame(
  tiempo=c(4.7, 3.8, 8, 13.6, 18.1, 6.8, 8.5, 13, 16.8, 26.7, 9, 9.5, 12.9, 17.7, 23.6),
  impresora=rep(grupos, c(5,5,5)),
  documento=rep(bloques, 3))
friedman.test(tiempo~impresora|documento, data=datosimp2)
friedman.test(datosimp)

# Se observan diferencias significativas entre las impresoras. Realizamos a continuación
# las comparaciones múltiples mediante el contraste exacto de Elsinga y otros (EHPG),
# disponible en la función frdAllPairsExactTest del paquete PMCMRplus

frdAllPairsExactTest(datosimp)

# sólo hay diferencias significativas entre la impresora 1 y la impresora 3

# La función de distribución empírica de una muestra x puede obtenerse en R mediante
# el comando ecdf(x)

muestra <- rnorm(100)
Fn <- ecdf(muestra)
windows()
plot(Fn)
rug(muestra)
curve(pnorm, from=-4, to=4, lwd=2, add=TRUE, col="blue")

# El contraste de Kolmogorov-Smirnov está implementado en la función ks.test
# del paquete stat

# Ejemplo: contrastemos si la muestra anterior proviene de una distribución N(0,1).
# Contrastemos también si proviene de una t-Student con 3 grados de libertad y de 
# una distribución de Cauchy(0,1)

ks.test(x=muestra, pnorm)
ks.test(x=muestra, pt, df=3)
ks.test(x=muestra, pcauchy, location=0, scale=1)

# El contraste de Cramér-von Mises se puede encontrar en la función cvm.test
# del paquete goftest

# Ejemplo: repetir los contrastes anteriores utilizando el contraste de Cramér-von Mises

library(goftest)
cvm.test(x=muestra, pnorm)
cvm.test(x=muestra, pt, df=3)
cvm.test(x=muestra, pcauchy, location=0, scale=1)

# El contraste de Anderson-Darling se puede encontrar en la función ad.test
# del paquete goftest

# Ejemplo: repetir los contrastes anteriores utilizando el contraste de Anderson-Darling

ad.test(x=muestra, pnorm)
ad.test(x=muestra, pt, df=3)
ad.test(x=muestra, pcauchy, location=0, scale=1)


# En el caso de que queramos contrastar si la distribución de los datos proviene de
# la familia normal (sin especificar media y varianza), podemos utilizar la función
# LillieTest del paquete DescTools, que realiza el test de Lilliefors

library(DescTools)
LillieTest(x=muestra)
muestra.unif <- runif(100, min=-3, max=3)
LillieTest(x=muestra.unif)

# El Q-Q plot para una distribución normal lo podemos pintar con la función qqnorm

n <- 100
mu <- 10
sigma <- 2
x <- rnorm(n, mean = mu, sd = sigma)
windows()
qqnorm(x)
abline(a = mu, b = sigma, col = "red")

# El contraste de normalidad de Shapiro-Wilk está en la función shapiro.test
# El contraste de normalidad de Shapiro-Francia lo podemos encontrar en la
# función sf.test del paquete nortest (que contiene varios contrastes más)

shapiro.test(muestra)
shapiro.test(muestra.unif)

library(nortest)
sf.test(muestra)
sf.test(muestra.unif)


# El contraste de la chi cuadrado está implementado en la función chisq.test del
# paquete stats. Para contrastar si los datos vienen de una distribución uniforme
# basta especificar el vector de frecuencias observadas de cada clase

o <- c(105, 107, 89, 103, 111, 85)
chisq.test(o)

# Si queremos realizar el contraste chi cuadrado para otra distribución, podemos
# utilizar chisq.test para calcular el estadístico de contraste, pero luego
# tenemos que calcular el p-valor a mano para utilizar el número correcto de
# grados de libertad.

# Ejemplo: Contrastar si los datos de goles de un equipo de fútbol provienen de
# una distribución de Poisson

o <- c(8, 6, 11, 5, 8)
media <- (1*6 + 2*11 + 3*5 + 4*5 + 5*1 + 6*2)/38
p <- c(dpois(0:3, lambda=media), 1 - ppois(3, media))
estadistico <- chisq.test(x=o, p=p)$statistic
pvalor <- 1 - pchisq(estadistico, df=5-1-1)

# Para el contraste de homogeneidad se usa la misma función, pero proporcionando
# la tabla de contingencia.

# Ejemplo: comparar las frecuencias de las palabras a, an, this, that, with, 
# without en varios escritos de Austen y en uno de un imitador.

austen <- matrix(
  c(83, 434, 29, 62, 15, 86, 22, 236, 43, 161, 4, 38),
  nrow=2
)
chisq.test(austen)

# Del mismo modo exactamente se hace el contraste de independencia

tabla<-matrix(
  c(68, 52, 56, 72, 32, 20),
  nrow=2
)
chisq.test(tabla)
