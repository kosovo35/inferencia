# PRÁCTICA 1 : CONTRASTES NO PARAMÉTRICOS CLÁSICOS ----
# P1 - CONTRASTE DE SIGNOS PARA LA MEDIANA ----

# Ejemplo: Los tiempos de supervivencia, en semanas, de n = 10 pacientes con un tipo de linfoma
# fueron 49, 58, 75, 110, 112, 132, 151, 276, 281, 362. Contrastar si puede suponerse que la
# mediana del tiempo de supervivencia es de m_0 = 200 semanas.

library(DescTools)
tiempos <- c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362) ; tiempos
SignTest(x=tiempos, mu=200, alternative="two.sided")

### LA MEDIANA DE LAS DIFERENCIAS SE PUEDE CALCULAR A MANO; ES LA MEDIANA DE LOS TIEMPOS

### FORMA ALTERNATIVA:
binom.test(x=3, n=10, p=0.5, alternative="two.sided")

# Ejemplo: Se somete a 7 pacientes con temor al vómito a cierto tratamiento. El efecto que se
# desea obtener con esta terapia es la reducción de la ansiedad causada por el vómito. Cada
# paciente pasó un test, antes y después del tratamiento, que evaluaba su sensación de temor
# (valores altos indican más temor). ¿Puede afirmarse que el tratamiento tuvo los efectos
# deseados?

## COMO SE TOMAN 2 MEDICIONES SOBRE LOS MISMOS INDIVIDUOS EN DIFERENTES ESPACIOS DE TIEMPO,
## TENEMOS MUESTRAS APAREADAS Y PODEMOS USAR EL CONTRASTE DE SIGNOS

antes <- c(10.6, 7.9, 12.4, 16.8, 13.2, 14.7, 18.34) ; antes
despues <- c(7.15, 9.36, 6.27, 7.19, 5.45, 6.21, 8) ; despues
SignTest(x=antes, y=despues, mu=0, alternative="greater")

# P1 - CONTRASTE DE RANGOS ASIGNADOS PARA LA MEDIANA DE WILCOXON ----

# Cuando la distribución es simétrica
# Ejemplo: Aplicar el contraste de rangos signados a los datos del vómito.

wilcox.test(x=antes, y=despues, mu=0, paired=TRUE, alternative="greater")

## V ES LA SUMA DE RANGOS POSITIVOS ; POR DEFECTO LA FUNCIÓN UTILIZA LA CORRECCIÓN Y NO DA EL
## P-VALOR EXACTO, SI QUEREMOS EXACTO USAMOS exact=true, PERO PARA DATOS GRANDES NO CONVIENE

# P1 - CONTRASTE DE DESPLAZAMIENTO DE MEDIANAS (MANN-WHITNEY) ----

# Ejemplo: Supongamos que tenemos muestras X=(1.69, 1.67, 1.74, 1.63, 1.65) e
# Y=(1.73, 1.92, 1.85, 1.8) independientes, que provienen de distribuciones que sólo difieren
# en un desplazamiento de medianas. Contrastar H_0: m_X = m_Y frente a H_1: m_X < m_Y.

m_x <- c(1.69, 1.67, 1.74, 1.63, 1.65) ; m_x
m_y <- c(1.73, 1.92, 1.85, 1.8) ; m_y
wilcox.test(x=m_x, y=m_y, mu=0, paired=FALSE, alternative="less")

## COMO ESTAMOS COMPARANDO SI LAS DOS MEDIANAS SON IGUALES, LO QUE INDICAMOS EN LA VARIABLE mu
## ES LA DIFERENCIA DE DICHAS MEDIANAS

# P1 - CONTRASTE PARA MÁS DE DOS MEDIANAS DE MUESTRAS INDEPENDIENTES (KRUSKAL-WALLIS) ----

# Ejemplo: Disponemos de 4 métodos de cultivo del maíz ensayados en n = 74 parcelas de cultivo
# experimental. Tenemos una variable respuesta de tipo continuo que mide el rendimiento del
# cultivo, de la cual se han observado los siguientes datos. Contrastar la hipótesis
# H_0: m_1 = m_2 = m_3 = m_4.

rendimientos <- c(
  83, 91, 94, 89, 89, 96, 91, 92, 90, 93, 91, 95, 86, 91, 89, 92, 90,
  91, 90, 81, 83, 84, 83, 88, 91, 89, 84, 82, 86, 84, 84, 91, 89, 90, 91, 83,
  101,100,91, 93, 96, 95, 94, 98, 95, 92, 80, 79, 82,
  78, 82, 81, 77, 79, 81, 80, 81, 81, 81, 94, 96, 101,81, 78
)

## Pasamos los rendimientos a 4 vectores, cada vector corresponde al rendimiento de uno de los
## cuatro métodos
r1 <- c(83, 91, 94, 89, 89, 96, 91, 92, 90, 93, 91, 95, 86, 91, 89, 92, 90)
r2 <- c(91, 90, 81, 83, 84, 83, 88, 91, 89, 84, 82, 86, 84, 84, 91, 89, 90, 91, 83)
r3 <- c(101,100,91, 93, 96, 95, 94, 98, 95, 92, 80, 79, 82)
r4 <- c(78, 82, 81, 77, 79, 81, 80, 81, 81, 81, 94, 96, 101,81, 78)

## Lamamos a la función que aplica el test de Kruskal-Wallis para más de dos muestras indep.
kruskal.test(x = list(r1, r2, r3, r4))

## El contraste resulta significativo y por tanto realizamos las comparaciones múltiples; vamos
## a usar el contraste de Conover-Iman

library(PMCMRplus)
kwAllPairsConoverTest(x = list(r1, r2, r3, r4))

# Encontramos diferencias significativas entre los métodos: 1-4 ; 2-3 ; 3-4 ; y ninguna más

# P1 - CONTRASTE PARA MÁS DE DOS MEDIANAS DE MUESTRAS RELACIONADAS (FRIEDMAN) ----

# Ejemplo: Se quiere comparar la velocidad de 3 impresoras midiendo el tiempo de impresión de 5
# documentos. Contrastar la hipótesis de que las 3 impresoras son igualmente rápidas.

datos_impresora <- matrix(
  data = c(4.7, 6.8, 9.0, 3.8, 8.5, 9.5, 8.0, 13.0, 12.9, 13.6, 16.8, 17.7, 18.1, 26.7, 23.6),
  nrow = 5,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c("Doc1", "Doc2", "Doc3", "Doc4", "Doc5"),
    c("Impr1", "Impr2", "Impr3")
  )
)

# Mostramos la matriz para comprobar que la hemos escrito de forma adecuada
datos_impresora

# Aplicamos el test de Friedman a los datos
friedman.test(datos_impresora)

# Como se observan diferencias significativas entre las impresoras, realizamos las comparaciones
# múltiples usando el contraste exacto de Elsinga-Heskes-Pelzer-Grotenhuis (EHPG)

frdAllPairsExactTest(datos_impresora)

# Vemos que sólo hay diferencias significativas entre la impresora 1 y la impresora 3

# P1 - FUNCIÓN DE DISTRIBUCIÓN EMPÍRICA ----

# Simulamos una muestra de una distribución normal de tamaño 100
set.seed(123)
muestra <- rnorm(100)

# Calulamos la función de distribución empírica
Fn <- ecdf(muestra)
Fn

# Representamos dicha función
{
  windows()                                  # para mostrar en una ventana nueva
  plot(Fn)                                   # dibujamos la función
  rug(muestra)                               # añadimos las marcas de valores (alfombra)
  curve(pnorm, lwd=2, add=TRUE, col="blue")  # dibujamos la curva de la función
}

# P1 - CONTRASTE DE BONDAD DE AJUSTE DE KOLMOGOROV-SMIRNOV (H0 SIMPLE) ----

# Ejemplo: contrastemos si la muestra de tamaño 100 proviene de una distribución N(0,1), de una
# t-Student con 3 grados de libertad o de una Cauchy(0,1).

# 1 - ¿proviene de una N(0,1)?
ks.test(x=muestra, y=pnorm)

# 2 -¿proviene de una t-Student con 3 grados de libertad?
ks.test(x=muestra, y=pt, df=3)

# 3 - ¿proviene de una Cauchy(0,1)?
ks.test(x=muestra, pcauchy, location=0, scale=1)

# P1 - CONTRASTE DE BONDAD DE AJUSTE DE CRAMÉR-VON MISES (H0 SIMPLE) ----
 
# Ejemplo: repetir los contrastes anteriores utilizando el contraste de Cramér-von Mises
library(goftest)

# 1 - ¿proviene de una N(0,1)?
cvm.test(x=muestra, pnorm)
# 2 -¿proviene de una t-Student con 3 grados de libertad? 
cvm.test(x=muestra, pt, df=3)
# 3 - ¿proviene de una Cauchy(0,1)?
cvm.test(x=muestra, pcauchy, location=0, scale=1)

# P1 - CONTRASTE DE BONDAD DE AJUSTE DE ANDERSON-DARLING (H0 SIMPLE) ----

# Ejemplo: repetir los contrastes anteriores utilizando el contraste de Anderson-Darling

# 1 - ¿proviene de una N(0,1)?
ad.test(x=muestra, pnorm)
# 2 -¿proviene de una t-Student con 3 grados de libertad? 
ad.test(x=muestra, pt, df=3)
# 3 - ¿proviene de una Cauchy(0,1)?
ad.test(x=muestra, pcauchy, location=0, scale=1)

# P1 - CONTRASTE DE BONDAD DE AJUSTE DE NORMALIDAD DE LILLIEFORS (H0 COMPUESTA) ----

library(DescTools)
LillieTest(x=muestra)

# Realizamos el QQ-plot
{
  windows()
  qqnorm(muestra)
  abline(a=mean(muestra), b=sd(muestra), col="red")
}

# P1 - CONTRASTE DE NORMALIDAD DE SHAPIRO-FRANCIA ----

library(nortest)
sf.test(muestra)

# P1 - CONTRASTE DE NORMALIDAD DE SHAPIRO-WILK ----

shapiro.test(muestra)

# P1 - CONTRASTE DE LA CHI-CUADRADO DE PEARSON ----

# Compara por defecto si los datos provienen de una distribución uniforme
freq_obs <- c(105, 107, 89, 103, 111, 85)
chisq.test(x = freq_obs)

# Para otra distribución, tenemos que calcular el estadístico de contraste y luego el p-valor
# manualmente para utilizar el número correcto de grados de libertad

# Ejemplo: Contrastar si los datos de goles de un equipo de fútbol provienen de una distribución
# de Poisson

freq_obs <- c(8, 6, 11, 5, 8)
media <- (1*6 + 2*11 + 3*5 + 4*5 + 5*1 + 6*2)/38
probabilidades <- c(dpois(0:3, lambda=media), 1 - ppois(3, lambda=media))
estadistico <- chisq.test(x=freq_obs, p=probabilidades)$statistic ; estadistico
valor_chi <- qchisq(p=1-0.05, df=5-1-1) ; valor_chi

# Como el valor de la chi cuadrado es mayor que el estadístico de contraste, los datos no
# descartan la distribución de Poisson.

# Calculemos el p-valor
pvalor <- 1 - pchisq(estadistico, df=5-1-1) ; pvalor

# P1 - CONTRASTE DE HOMOGENEIDAD ----

# Ejemplo: comparar las frecuencias de las palabras a, an, this, that, with, without en varios
# escritos de Austen y en uno de un imitador.

freq_imitador <- c(83, 29, 15, 22, 43, 4)
freq_austen <- c(434, 62, 86, 236, 161, 38)
tabla_contingencia <- matrix(
  c(freq_imitador, freq_austen),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Imitador", "Austen"),                         # nombre filas
    c("a", "an", "this", "that", "with", "without")  # nombre columnas
  )
)
chisq.test(tabla_contingencia)

# P1 - CONTRASTE DE INDEPENDENCIA ----

# Igual que antes

tabla_contingencia <- matrix(
  c(68, 56, 32, 52, 72, 20),
  nrow=2, 
  byrow=TRUE,
  dimnames = list(
    c("Mujer", "Hombre"),
    c("Demócrata", "Republicano", "Independiente")
  )
) ; tabla_contingencia
chisq.test(tabla_contingencia)

# -----------------------------------------------------------------------
# PRÁCTICA 2 : ESTIMACIÓN NO PARAMÉTRICA DE LA DENSIDAD ----

f1 <- faithful$eruptions

# P2 - REGLA DE STURGES (POR DEFECTO) ----
nclass.Sturges(f1)
{
  windows()
  hist(
    x=f1,
    probability=TRUE,
    xlab="Tiempo de erupción",
    ylab="Densidad",
    main="Regla de Sturges",
    xlim=c(1, 6),
    ylim=c(0, 0.7)
  )
  rug(f1)
}

# P2 - REGLA DE SCOTT ----
nclass.scott(f1)
{
  windows()
  h <- hist(
    x=f1,
    breaks="Scott",
    probability=TRUE,
    xlab="Tiempo de erupción",
    ylab="Densidad",
    main="Regla de Scott",
    xlim=c(1, 6),
    ylim=c(0, 0.7)
  )
  rug(f1)
}

# P2 - REGLA DE WAND ----
library(KernSmooth)

# Ancho de caja
b <- dpih(x=f1)

# Definimos las cajas
cajas <- seq(min(f1), max(f1)+b, by=b)
{
  windows()
  h <- hist(
    x=f1,
    breaks=cajas,
    probability=TRUE,
    xlab="Tiempo de erupción",
    ylab="Densidad",
    main="Regla de Wand",
    xlim=c(1, 6),
    ylim=c(0, 0.7)
  )
  rug(f1)
}

## Cogemos el triple de cajas que antes (veremos que son demasiadas cajas y no es correcto)
cajas <- seq(min(f1), max(f1)+b, by=b/3)
{
  windows()
  h <- hist(
    x=f1,
    breaks=cajas,
    probability=TRUE,
    xlab="Tiempo de erupción",
    ylab="Densidad",
    main="Regla de Wand",
    xlim=c(1, 6),
    ylim=c(0, 0.7)
  )
  rug(f1)
}

# P2 - ESTIMACIÓN NÚCLEO DE LA DENSIDAD ----

# bw por defecto es la referencia normal de Silverman
# kernel por defecto es el gaussiano o normal

d <- density(x=f1, bw=0.2)
{
  windows()
  plot(
    d,
    t="l",
    xlab="Tiempo de erupción",
    ylab="Densidad",
    main="h=0.2",
    xlim = c(1, 6),
    ylim = c(0, 0.7)
  )
  rug(f1)
}

# P2 - SELECTORES DE ANCHOS DE BANDA PARA EL ESTIMADOR NÚCLEO DE LA DENSIDAD ----

library(ks)

# Ancho de banda por referencia normal
h1 <- hns(f1)

# Ancho de banda por validación cruzada
h2 <- hlscv(f1)

# Ancho de banda por plug-in
h3 <- hpi(f1)

{
  windows()
  plot(
    density(f1, bw=h2),
    t="l", lwd=2, col="red",
    xlab="Tiempo de erupción", ylab="Densidad", main="Selectores",
    xlim = c(1, 6),
    ylim = c(0, 0.7)
  )
  lines(density(f1, bw=h1), lwd=2, col="orange")
  lines(density(f1, bw=h3), lwd=2, col="blue")
  rug(f1)
}

#### COMO OBSERVAMOS, EN EL VALLE DE LA DENSIDAD POR VALIDACIÓN CRUZADA, HAY UN AUMENTO EN EL
#### SUAVIZADO DEL VALLE, LO CUAL ES UN INDICIO DE QUE EL ANCHO DE BANDA ES PEQUEÑO, POR LO TANTO,
#### VISTO ESTO, DEBERÍAMOS AUMENTAR UN POCO EL ANCHO DE BANDA, AL MENOS PARA ELIMINAR ESA SUBIDA
#### QUE PARECE DEMASIADO ARTIFICIAL

# Ejemplo por simulación: generamos una muestra de tamaño 100 de la densidad
# (3/4) N(0,1) + (1/4) N(1.5,1/3^2) y pintamos el estimador núcleo con el selector plug-in de
# ancho de banda, y añadimos la verdadera densidad

set.seed(1)
muestra <- c(rnorm(n=75, mean=0, sd=1), rnorm(n=25, mean=1.5, sd=1/3))
h1 <- hns(muestra)
h2 <- hlscv(muestra)
h3 <- hpi(muestra)
d <- density(muestra, bw=h3)
{
  windows()
  plot(d$x, 0.75*dnorm(d$x, mean=0, sd=1) + 0.25*dnorm(d$x, mean=1.5, sd=1/3), 
    t="l", lwd=3,
    xlab="x", ylab="f(x)", main="")
  lines(d, lwd=2, col="blue")
  lines(density(muestra, bw=h2), lwd=2, col="red")
}

#### EN ESTE CASO NINGUNO DE LOS DOS ANCHOS DE BANDA LO ESTÁN HACIENDO MUY BIEN. AUMENTEMOS LOS
#### DATOS A 1000

muestra <- c(rnorm(n=750, mean=0, sd=1), rnorm(n=250, mean=1.5, sd=1/3))
h1 <- hns(muestra)
h2 <- hlscv(muestra)
h3 <- hpi(muestra)
d <- density(muestra, bw=h3)
{
  windows()
  plot(d$x, 0.75*dnorm(d$x, mean=0, sd=1) + 0.25*dnorm(d$x, mean=1.5, sd=1/3), 
    t="l", lwd=3, xlab="x", ylab="f(x)", main="")
  lines(d, lwd=2, col="blue")
  lines(density(muestra, bw=h2), lwd=2, col="red")
}

#### AHORA OBSERVAMOS QUE HAY CIERTAS OSCILACIONES QUE SON UN POCO ARTIFICIALES, POR LO TANTO,
#### PODRÍA MEJORARSE EL ANCHO DE BANDA

# P2 - BANDAS DE VARIABILIDAD ----

# Con la opción panel=TRUE en sm.density se controla el suavizado de manera manual e interactiva

library(sm)
{
  windows()
  sm.density(
    f1,
    h=hpi(f1),
    se=TRUE,
    xlim=c(1, 6), ylim=c(0, 0.7)
  )
}

library(tkrplot)
{
  windows()
  sm.density(f1, panel=TRUE)
}

#### CON ESTO PODEMOS MOVER EL ANCHO DE BANDA Y ASÍ VER CUÁL ES EL QUE MEJOR NOS CONVIENE Y
#### PODER SELECCIONARLO A OJO DE LA MEJOR MANERA POSIBLE

#### MOVIENDO LOS VALORES DE LA H, SI TUVIÉSEMOS QUE SELECCIONAR A OJO EL ANCHO DE BANDA, LA
#### MEJOR OPCIÓN SERÍA 0.16 O UN POCO MÁS

# El estimador núcleo También está implementado en la función kde del paquete ks
# Con la opción positive=TRUE podemos especificar que se aplique una transformación logarítmica
# para estimar la densidad de datos positivos

library(densEstBayes)
y <- log(incomeUK)
hy <- hpi(y)
fhat <- kde(x = incomeUK, h=hy, positive=TRUE) #### positive=TRUE INDICA QUE LOS VALORES SON > 0

{
  windows()
  plot(fhat, lwd=2, xlab="Sueldos relativos", ylab="Densidad", xlim=c(0,4))
  rug(incomeUK)
}

# P2 - ESTIMADOR NÚCLEO MULTIVARIANTE ----

datos <- iris[,1:2]
{
  windows()
  plot(datos, xlim=c(4,8), ylim=c(1,5))
}

H <- Hpi(datos, pilot="unconstr") #### MATRIZ DE ANCHO DE BANDA DE TIPO PLUG-IN. HPI ES PARADATOS
#### UNIVARIANTES Y HPI ES PARA DATOS MULTIVARIANTES. pilot="unconstr" ES SIN RESTRICCIONES
fhat <- kde(x=datos, H=H)
{
  windows()
  plot(fhat, lwd=2, drawlabels=FALSE, col=1, xlim=c(4,8), ylim=c(1,5), cont=10*(1:9))
  points(datos, pch=21, bg="blue")
}

# P2 - ANÁLISIS DISCRIMINANTE NO PARAMÉTRICO BASADO EN ESTIMADORES NÚCLEO ----

# El análisis discriminante no paramétrico basado en estimadores núcleo de la  densidad está
# implementado en la función kda del paquete ks

etiquetas <- iris[,5]
clas <- kda(datos, etiquetas)
{
  windows()
  plot(clas, drawlabels=FALSE, xlim=c(4,8), ylim=c(1,5), cont=0)
  points(datos, bg="blue", pch=as.numeric(etiquetas))
}

# El algoritmo mean shift para análisis cluster no paramétrico basado en la densidad también
# está incluido en el paquete ks, en la función kms

{
  windows()
  plot(faithful)
  H <- Hpi(faithful)
  fhat <- kde(faithful, H=H)
  kms.faith<-kms(faithful, H=H, verbose=TRUE)
  plot(kms.faith, pch=19, col=c("blue","red"))
  plot(fhat, add=TRUE, drawlabels=FALSE, col="black", cont=10*(1:9))
}

# -----------------------------------------------------------------------
# PRACTICA 3 - ESTIMACIÓN DE LA CURVA DE REGRESIÓN ----

# P3 - ESTIMADOR NÚCLEO DE LA REGRESIÓN ----

# La función básica para obtener el estimador núcleo en R es ksmooth, del paquete stats
# Hay que especificar x, y, kernel="normal" para el núcleo normal, bandwidth y x.points (la rejilla
# de puntos donde calcula el estimador). Si no se especifica la rejilla, usa una de 100 puntos
# entre min(x) y max(x)

# Vamos a probarla con un conjunto de datos simulados
# La función de regresión va a ser m(x)=sin(x)
# El error va a ser N(0,0.25^2), la distribución de X es uniforme en [0, 2*pi]

set.seed(1)
m <- function(x){ sin(x) }
x <- runif(100, min=0, max=2*pi)
e <- rnorm(100, mean=0, sd=0.25)
y <- m(x) + e
windows()
plot(x,y)

# Calculamos el estimador núcleo con h=1 y lo pintamos
mhat <- ksmooth(x, y, bandwidth=1, kernel="normal")
lines(mhat, col="blue", lwd=2)

# Añadimos la verdadera función de regresión
rejilla <- mhat$x
lines(rejilla, m(rejilla), lwd=2)

# Para calcular el ancho de banda por validación cruzada existen dos funciones
# np::npregbw (npregbw en la librería np) y locpol::regCVBwSelC 

library(locpol)
h1 <- regCVBwSelC(x=x, y=y, deg=0)
s
library(np)
h2 <- npregbw(xdat=x, ydat=y, regtype="lc", bwmethod="cv.ls")

mhat2 <- ksmooth(x, y, bandwidth=h1, kernel="normal")
lines(mhat2, lwd=2, col="red")

# La librería sm tiene la función sm.regression que, con la opción panel=TRUE, permite ir variando
# el ancho de banda de manera interactiva

library(sm)
sm.regression(x=x, y=y, panel=TRUE)

# Otra opción para construir un panel interactivo (con RStudio) es usar la función manipulate,
# del paquete con el mismo nombre

library(manipulate)
manipulate({
  plot(x, y)
  mhat <- ksmooth(x, y, bandwidth=h, kernel="normal")
  lines(mhat, col="blue", lwd=2)
  rejilla <- mhat$x
  lines(rejilla, m(rejilla), lwd=2)
}, h = slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))

# Ver más ejemplos en https://bookdown.org/egarpor/NP-UC3M/kre-i-kre.html#fig:kreg

# Podemos utilizar el estimador núcleo de la regresión con los datos de Boston
# donde x=lstat e y=rm

library(MASS)
data(Boston)

windows()
plot(Boston$lstat, Boston$rm)
h <-regCVBwSelC(x=Boston$lstat, y=Boston$rm, deg=0)
mhat <- ksmooth(x=Boston$lstat, y=Boston$rm, bandwidth=h, kernel="normal")
lines(mhat, col="red", lwd=2)
mhat2 <- ksmooth(x=Boston$lstat, y=Boston$rm, bandwidth=3, kernel="normal")
lines(mhat2, col="blue", lwd=2)

# P3 - ESTIMADOR LINEAL LOCAL ----

# El estimador lineal local podemos encontrarlo implementado en KernSmooth::locpoly,
# locpol::locLinSmootherC o en np::npreg

# La selección de ancho de banda puede hacerse mediante validación cruzada, con las funciones
# que ya vimos antes, np::npregbw (indicando regtype="ll") o mediante locpol::regCVBwSelC
# (indicando deg=1)

# Aunque no lo hemos visto en teoría, existe un selector de tipo plug-in para el estimador lineal
# local, que se puede calcular mediante KernSmooth::dpill o mediante locpol::pluginBw

# Selector por validación cruzada para regresión lineal local con los datos simulados

hcv1 <- npregbw(xdat=x, ydat=y, regtype="ll", bwmethod="cv.ls")
hcv2 <- regCVBwSelC(x=x, y=y, deg=1, kernel=gaussK)

# Selector plug-in para regresión lineal local con los datos simulados

library(KernSmooth)
hpi1 <- dpill(x=x, y=y)
hpi2 <- pluginBw(x=x, y=y, deg=1, kernel=gaussK) 
#Salen resultados distintos, más fiable dpill

# Pintamos los estimadores lineales locales
# KernSmooth::locpoly no permite escoger los puntos de evaluación, debe ser una rejilla
# locpol::locLinSmootherC sí permite escoger los puntos de evaluación, pero el que mejor
# funciona es np::npreg

llcv <- npreg(bws = hcv2, txdat = x, tydat = y, exdat=rejilla)
llpi <- npreg(bws = hpi1, txdat = x, tydat = y, exdat=rejilla)
windows()
plot(x, y)
lines(llcv$eval[,1], llcv$mean, lwd=2, col="red")
lines(llpi$eval[,1], llpi$mean, lwd=2, col="blue")

# Pintemos ahora los estimadores lineales locales para los datos de Boston

hc <- regCVBwSelC(x=Boston$lstat, y=Boston$rm, deg=1, kernel=gaussK)
hp <- dpill(x=Boston$lstat, y=Boston$rm)

boston.llcv <- npreg(bws = hc, txdat = Boston$lstat, tydat = Boston$rm, 
  exdat=mhat$x)
boston.llpi <- npreg(bws = hp, txdat = Boston$lstat, tydat = Boston$rm, 
  exdat=mhat$x)

windows()
plot(Boston$lstat, Boston$rm)
lines(boston.llcv$eval[,1], boston.llcv$mean, lwd=2, col="red")
lines(boston.llpi$eval[,1], boston.llpi$mean, lwd=2, col="blue")
lines(mhat2, col="orange", lwd=2)

# P3 - REGRESOGRAMA ----

# El regresograma está implementado en la función HoRM::regressogram. Hay que especificar, x, y,
# y el número de cajas nbins

library(HoRM)
r <- regressogram(x=x, y=y, nbins=8)
windows()
plot(r)

# El paquete HoRM no tiene funciones para elegir el número de cajas a partir de los datos. Un
# enfoque más actual se puede encontrar en la función binsreg::binsreg

library(binsreg)
windows()
b <- binsreg(y, x)

windows()
bb<-binsreg(rm, lstat, data=Boston)

# Selectores automáticos del número de cajas pueden obtenerse mediante binsreg::binsregselect

binsregselect(y, x)


# P3 - REGRESIÓN LOGÍSTICA LOCAL ----

# Vamos a analizar unos datos reales en los que registra si unos bebés recién nacidos presentaban
# displasia broncopulmonar o no (variable BPD) en función de su peso al nacer, en gramos
# (birthweight). Los datos los podemos conseguir con:

bpd<-read.table(file="http://www.stat.cmu.edu/~larry/all-of-nonpar/=data/bpd.dat",header=TRUE)
bpd<-as.data.frame(bpd)

# Vamos a comparar el modelo de regresión logística global con el local
# Podemos obtener el modelo global usando la función glm

rlg<-glm(BPD~birthweight, data=bpd, family = binomial)

windows()
plot(bpd,pch="|")
rej <- seq(min(bpd$birthweight),max(bpd$birthweight),length=100)
y.rlg <- predict(rlg, newdata=data.frame(birthweight=rej), type="resp")
lines(rej, y.rlg, col="blue", lwd=2)

# La regresión logística local podemos obtenerla con el paquete locfit

library(locfit)
rll <- locfit(BPD~birthweight, data=bpd)
lines(rll, col="red", lwd=2)

# Para evaluar el estimador en nuevos puntos hay que usar predict

# -----------------------------------------------------------------------
# PRÁCTICA 4 - ESTIMACIÓN POR SPLINES ----

# P4 - REGRESIÓN POR SPLINES ----

# El estimador por splines está en el paquete básico de R, en la función smooth.spline. Hay que
# especificarle x, y, y el valor spar del parámetro de suavizado (que no es exactamente igual
# que el lambda de teoría). Si no se especifica spar, lo coge por validación cruzada generalizada
# (GCV). Alternativamente, se puede especificar lambda

# Generamos los datos del modelo de regresión y=sen(x)+e de la práctica anterior Y pintamos el
# estimador núcleo inicial calculado, junto con la verdadera regresión
set.seed(1)
m <- function(x){ sin(x) }
x <- runif(100, min=0, max=2*pi)
e <- rnorm(100, mean=0, sd=0.25)
y <- m(x) + e
windows()
plot(x,y)

mhat <- ksmooth(x, y, bandwidth=1, kernel="normal")
lines(mhat, col="red", lwd=2)
rejilla <- mhat$x
lines(rejilla, m(rejilla), lwd=2)

# Calculamos el spline de suavizado

ss <- smooth.spline(x=x, y=y)

lines(ss, lwd=2, col="blue")

# El valor de lambda utilizado puede recuperarse del objeto creado
ss$lambda #0.001075831

# Podemos explorar el resultado para distintos valores de lambda
lines(smooth.spline(x,y,lambda=1e-7), lwd=2, col="pink")
lines(smooth.spline(x,y,lambda=1), lwd=2, col="orange")


# Para calcular el estimador por splines en una rejilla, o en vector de valores x utilizamos
# predict

predict(ss, x = c(0,0.2,0.8))

# Veamos el resultado para los datos de Boston

library(MASS)
data(Boston)
ss2 <- smooth.spline(Boston$lstat, Boston$rm)
windows()
plot(Boston$lstat, Boston$rm)
lines(ss2, lwd=2, col="blue")

# P4 - ESTIMACIÓN DE LA DENSIDAD MEDIANTE SPLINES ----

# El paquete principal para hacer estimación de la densidad mediante splines es logspline, a
# través de la función del mismo nombre. También está incluido en el paquete gss, mediante la
# función ssden()

library(logspline)

# Recuperemos la estimación de la densidad de eruptions mediante un histograma

f1<-faithful$eruptions

library(KernSmooth)
b <- dpih(x = f1)
cajas <- seq(min(f1), max(f1)+b, by=b)

windows()
h <- hist(x = f1, breaks = cajas, probability = TRUE, xlab = "Tiempo de erupción",
  ylab = "Densidad", main="", xlim = c(1, 6), ylim = c(0, 1) )
rug(f1)

f1.spline<-logspline(f1)
plot(f1.spline,add=TRUE,n=1000,lwd=3)

# Para calcular el valor del estimador en un punto concreto se utiliza dlogspline

dlogspline(2.1, f1.spline)

# Con gss necesitamos especificar un modelo de predicción sin respuesta
library(gss)
f1.gss<-ssden(~f1)

xx<-seq(1.6,5.1,length=1000)
fhat<-dssden(f1.gss,xx)
lines(xx,fhat,lwd=3, col="blue")

# Comparamos con un estimador núcleo con ancho de banda pequeño

h<-dpik(f1) # 0.165

lines(density(f1,bw=0.05),col="red",lwd=3)
# -----------------------------------------------------------------------
# PRÁCTICA 5 - MODELOS ADITIVOS GENERALIZADOS ----

# P5 - REGRESIÓN MÚLTIPLE ----

# El paquete np permite obtener los estimadores por regresión tipo núcleo o tipo lineal local
# con varios predictores. Basta especificar las variables predictoras en el modelo de regresión.
# El estimador núcleo se obtiene con regtype="lc" y el estimador lineal local con regtype="ll"

library(np)
library(MASS)
data(Boston)

bw <- npregbw(rm ~ lstat + age, data=Boston, regtype = "ll")
boston.ll <- npreg(bw)

# Al pintar el objeto anterior se obtiene un gráfico dinámico
windows()
plot(boston.ll)

# Para pintar el estimador también podemos utilizar una rejilla de 20 x 20 puntos respecto a las
# variables predictoras lstat y age, donde evaluamos el estimador

xs <- seq(0, 40, by = 2)
ys <- seq(0, 100, by = 5)
xys <- expand.grid(xs, ys)

boston.ll2 <- npreg(bws=bw$bw, txdat=cbind(Boston$lstat, Boston$age),
  tydat=Boston$rm, regtype="ll", exdat=xys)

zs <- matrix(boston.ll2$mean, nrow=length(xs), ncol=length(ys))

windows()
persp(xs, ys, zs, theta=40, d=4, xlab="lstat", ylab="age", zlab="rm",
  ticktype="detailed", lwd=2, main="regresión lineal local")

# P5 - MODELOS ADITIVOS GENERALIZADOS ----

# Existen dos paquetes muy elaborados para trabajar con modelos aditivos generalizados: gam
# (de Trevor Hastie, uno de sus inventores) y mgcv (de Simon Wood, uno de los investigadores
# recientes)

# En el paquete gam, la función principal también se llama gam. Se utiliza la fórmula habitual
# de la forma respuesta~predictores, pero hay que indicar qué tipo de método de suavizado se
# quiere utilizar con cada predictor: lo (lineal local) o s (spline). Se puede especificar el
# parámetro de suavizado con lo(x, h) o s(x, lambda) o utilizar los valores por defecto (no por
# GCV)

# Para modelos aditivos (no generalizados) se especifica family=gaussian (o nada)

library(gam)
boston.gam <- gam(rm~lo(lstat)+lo(age), data=Boston)
windows()
plot(boston.gam, ask=TRUE)

zs2 <- predict(boston.gam, newdata=data.frame(lstat=xys[,1], age=xys[,2]))
zs2 <- matrix(zs2, nrow=length(xs), ncol=length(ys))

windows()
persp(xs, ys, zs2, theta=40, d=4, xlab="lstat", ylab="age", zlab="rm",
  ticktype="detailed", lwd=2, main="Modelo aditivo lineal local")

# En el paquete mgcv la función también se llama gam, pero tiene distintas posibilidades: sólo
# admite ajuste de las componentes por splines y utiliza  por defecto GCV para escoger los
# parámetros de suavizado

library(mgcv)
g <- gam(rm~s(lstat)+s(age), data=Boston)

windows()
plot(g, se=FALSE, lwd=2)

# podríamos pintar la superficie de regresión en 3D como en el caso anterior, utilizando predict

# Para ajustar un modelo aditivo logístico tenemos que especificar family=binomial. Veamos cómo
# realizar el análisis de los datos de pacientes diabéticos


library(gss)
data(wesdr)

lg <- gam(ret~s(dur)+s(gly)+s(bmi), data=wesdr, family=binomial)

windows();
plot(lg, pages=3, se=FALSE, lwd=2)

# -----------------------------------------------------------------------
