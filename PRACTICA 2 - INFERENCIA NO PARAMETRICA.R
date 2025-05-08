### Pr?ctica 2
### Estimaci?n no param?trica de la densidad

# Los datos de tiempo de erupci?n del geyser Old Faithful son la primera variable
# de la tabla de datos faithful

f1<-faithful$eruptions

### Histogramas

# La funci?n que calcula y dibuja los histogramas en R es hist

# La opci?n breaks sirve para especificar los valores donde empiezan y acaban
# las cajas, si es que le proporcionamos un vector de valores ordenados

# Tambi?n se le puede decir el n?mero de cajas

# Tambi?n admite que le especifiquemos el nombre de alguna de las reglas cl?sicas:
# "Sturges" (valor por defecto), o "Scott"

# En el caso de que le especifiquemos un n?mero de cajas, puede que no utilice
# exactamente ese n?mero de cajas, porque realiza cierto "redondeo" mediante
# la funci?n pretty()

# Por defecto, la altura de las cajas representa frecuencias absolutas. Si queremos
# que el histograma sea un estimador de la densidad hay que especificar
# probability=TRUE

windows()
hist(x = f1, probability = TRUE, xlab = "Tiempo de erupci?n", ylab = "Densidad",
     main = "Regla de Sturges", xlim = c(1, 6), ylim = c(0, 0.7) )
rug(f1)

windows()
h <- hist(x = f1, breaks = "Scott", probability = TRUE, xlab = "Tiempo de erupci?n",
     ylab = "Densidad", main = "Regla de Scott", xlim = c(1, 6), ylim = c(0, 0.7) )
rug(f1)

# La regla de Wand para elegir el ancho de caja podemos encontrarla en el 
# paquete KernSmooth

library(KernSmooth)
b <- dpih(x = f1)
cajas <- seq(min(f1), max(f1)+b, by=b)

windows()
h <- hist(x = f1, breaks = cajas, probability = TRUE, xlab = "Tiempo de erupci?n",
     ylab = "Densidad", main = "Regla de Wand", xlim = c(1, 6), ylim = c(0, 0.7) )
rug(f1)


# Ejercicio: construir los histogramas con las 3 reglas de selecci?n de ancho de
# caja para la tabla de datos incomeUK del paquete densEstBayes

### Estimaci?n n?cleo de la densidad

# La funci?n b?sica para estimaci?n de densidades en R es density
# La sintaxis de density es
# density(x, bw, kernel="gaussian", n=512, from, to, cut=3)

# x es el vector de datos
# bw es el ancho de banda (por defecto, la referencia normal de Silverman)
# kernel es el n?cleo usado (por defecto, el gaussiano o normal)
# density calcula el estimador de la densidad en una rejilla de n=512 puntos
# que van desde from - cut*bw hasta to + cut*bw ( por defecto, from=min(x) y 
# to=max(x) )

d <- density(x = f1, bw = 0.2)
windows()
plot(d, t="l", xlab="Tiempo de erupci?n", ylab="Densidad", main="h=0.2",
     xlim = c(1, 6), ylim = c(0, 0.7))
rug(f1)

# Los selectores de ancho de banda por referencia normal, plug-in y por 
# validaci?n cruzada pueden encontrarse en el paquete ks

library(ks)
h1 <- hns(f1)
h2 <- hlscv(f1)
h3 <- hpi(f1)

windows()
plot(density(f1, bw=h2), t="l", lwd=2, col="red", xlab="Tiempo de erupci?n", 
     ylab="Densidad", main="Selectores", xlim = c(1, 6), ylim = c(0, 0.7))

lines(density(f1, bw=h1), lwd=2, col="orange")
lines(density(f1, bw=h3), lwd=2, col="blue")
rug(f1)

# Ejemplo por simulaci?n: generamos una muestra de tama?o 1000 de la densidad
# (3/4) N(0, 1) + (1/4) N(1.5, 1/3^2) y pintamos el estimador n?cleo con
# el selector plug-in de ancho de banda, y a?adimos la verdadera densidad

set.seed(1)
muestra <- c(rnorm(n=75, mean=0, sd=1), rnorm(n=25, mean=1.5, sd=1/3))
h1 <- hns(muestra)
h2 <- hlscv(muestra)
h3 <- hpi(muestra)
d <- density(muestra, bw=h3)
windows()
plot(d$x, 0.75*dnorm(d$x, mean=0, sd=1) + 0.25*dnorm(d$x, mean=1.5, sd=1/3), 
     t="l", lwd=3, xlab="x", ylab="f(x)", main="")
lines(d, lwd=2, col="blue")
lines(density(muestra, bw=h2), lwd=2, col="red")

# Las bandas de variabilidad est?n implementadas en el paquete sm
# Con la opci?n panel=TRUE en sm.density se controla el suavizado de manera
# manual e interactiva

library(sm)
windows()
sm.density(f1, h=hpi(f1), se=TRUE, xlim=c(1, 6), ylim=c(0, 0.7))

sm.density(f1, panel=TRUE)

# El estimador n?cleo tambi?n est? implementado en la funci?n kde del paquete ks
# Con la opci?n positive=TRUE podemos especificar que se aplique una transformaci?n
# logar?tmica para estimar la densidad de datos positivos

library(densEstBayes)
y <- log(incomeUK)
hy <- hpi(y)
fhat <- kde(x = incomeUK, h=hy, positive=TRUE)

windows()
plot(fhat, lwd=2, xlab="Sueldos relativos", ylab="Densidad", xlim=c(0,4))
rug(incomeUK)

# El estimador n?cleo multivariante tambi?n se puede obtener con la funci?n kde
# del paquete ks

datos <- iris[,1:2]
windows()
plot(datos, xlim=c(4,8), ylim=c(1,5))

H <- Hpi(datos, pilot="unconstr")
fhat <- kde(x=datos, H=H)
windows()
plot(fhat, lwd=2, drawlabels=FALSE, col=1, xlim=c(4,8), ylim=c(1,5), cont=10*(1:9))
points(datos, pch=21, bg="blue")

# El an?lisis discriminante no param?trico basado en estimadores n?cleo de la 
# densidad est? implementado en la funci?n kda del paquete ks

etiquetas<-iris[,5]
clas<-kda(datos, etiquetas)
windows()
plot(clas, drawlabels=FALSE, xlim=c(4,8), ylim=c(1,5), cont=0)
points(datos, bg="blue", pch=as.numeric(etiquetas))

# El algoritmo mean shift para an?lisis cluster no param?trico basado en la
# densidad tambi?n est? inclu?do en el paquete ks, en la funci?n kms

windows()
plot(faithful)
H<-Hpi(faithful)
fhat<-kde(faithful, H=H)
kms.faith<-kms(faithful, H=H, verbose=TRUE)
plot(kms.faith, pch=19, col=c("blue","red"))
plot(fhat, add=TRUE, drawlabels=FALSE, col="black", cont=10*(1:9))


