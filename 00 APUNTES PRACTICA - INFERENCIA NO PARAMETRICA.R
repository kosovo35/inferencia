# PRÁCTICA 1 : CONTRASTES NO PARAMÉTRICOS CLÁSICOS ----
# P1 - CONTRASTE DE SIGNOS PARA LA MEDIANA ----

# Jesus la mama

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

frdAllPairsExactTest(datosimp)

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
probabilidades <- c(dpois(0:3, lambda=media), 1 - ppois(3, media))
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

# Los datos de tiempo de erupción del geyser Old Faithful son la primera variable de la tabla
# de datos faithful

f1 <- faithful$eruptions

# Histogramas

# La función que calcula y dibuja los histogramas en R es hist

# La opción breaks sirve para especificar los valores donde empiezan y acaban las cajas, si es
# que le proporcionamos un vector de valores ordenados

# También se le puede decir el número de cajas

# También admite que le especifiquemos el nombre de alguna de las reglas clásicas: 
# "Sturges" (valor por defecto), o "Scott"

# En el caso de que le especifiquemos un número de cajas, puede que no utilice exactamente ese
# número de cajas, porque realiza cierto "redondeo" mediante la función pretty()

# Por defecto, la altura de las cajas representa frecuencias absolutas. Si queremos que el
# histograma sea un estimador de la densidad hay que especificar probability=TRUE

{
  windows()
  hist(x = f1, probability = TRUE, xlab = "Tiempo de erupción", ylab = "Densidad",
    main = "Regla de Sturges", xlim = c(1, 6), ylim = c(0, 0.7) )
  rug(f1)
}

#### SI NO SE LE DICE NADA, EL HISTOGRAMA COGE LAS PROBABILIDADES ABSOLUTAS

{
  windows()
  h <- hist(x = f1, breaks = "Scott", probability = TRUE, xlab = "Tiempo de erupción",
    ylab = "Densidad", main = "Regla de Scott", xlim = c(1, 6), ylim = c(0, 0.7) )
  rug(f1)
}

#### CAJAS QUE NECESITAS CON STURGES
nclass.Sturges(f1)

#### CAJAS QUE NECESITAS CON SCOTT
nclass.scott(f1)

# La regla de Wand para elegir el ancho de caja podemos encontrarla en el paquete KernSmooth

library(KernSmooth)
b <- dpih(x = f1)
cajas <- seq(min(f1), max(f1)+b, by=b)

#### LA VARIABLE cajas SON LOS PUNTOS DONDE VAMOS A CALCULAR LOS INTERVALOS PARA EL HISTOGRAMA

{
  windows()
  h <- hist(x = f1, breaks = cajas, probability = TRUE, xlab = "Tiempo de erupción",
    ylab = "Densidad", main = "Regla de Wand", xlim = c(1, 6), ylim = c(0, 0.7) )
  rug(f1)
}

#### ESTIMACIÓN CON EL ANCHO DE CAJA CON LA REGLA DE WAND
cajas <- seq(min(f1), max(f1)+b, by=b/3)

{
  windows()
  h <- hist(x = f1, breaks = cajas, probability = TRUE, xlab = "Tiempo de erupci?n",
    ylab = "Densidad", main = "Regla de Wand", xlim = c(1, 6), ylim = c(0, 0.7) )
  rug(f1)
}

#### EN ESTE CASO HEMOS COGIDO DEMASIADAS CAJAS, Y POR LO TANTO NO ESTÁ BIEN HECHO EL HISTOGRAMA


# Ejercicio: construir los histogramas con las 3 reglas de selección de ancho de caja para la
# tabla de datos incomeUK del paquete densEstBayes

### Estimación núcleo de la densidad

# La función básica para Estimación de densidades en R es density
# La sintaxis de density es density(x, bw, kernel="gaussian", n=512, from, to, cut=3)

# x es el vector de datos
# bw es el ancho de banda (por defecto, la referencia normal de Silverman)
# kernel es el núcleo usado (por defecto, el gaussiano o normal)
# density calcula el estimador de la densidad en una rejilla de n=512 puntos que van desde
# from - cut*bw hasta to + cut*bw ( por defecto, from=min(x) y  to=max(x) )

d <- density(x = f1, bw = 0.2)
{
  windows()
  plot(d, t="l", xlab="Tiempo de erupción", ylab="Densidad", main="h=0.2",
    xlim = c(1, 6), ylim = c(0, 0.7))
  rug(f1)
}

# Los selectores de ancho de banda por referencia normal, plug-in y por  validación cruzada
# pueden encontrarse en el paquete ks

library(ks)
h1 <- hns(f1)
#### ANCHO DE BANDA POR REFERENCIA NORMAL (COGE LA CURVATURA COMO SI FUERA UNA NORMAL, PERO LA
#### NORMAL ES MÁS SUAVE QUE LA DENSIDAD QUE NOSOTROS ESTAMOS ESTIMANDO. COMO LA CURVATURA ES
#### MÁS PEQUEÑA, OBTENEOMS UN h MÁS GRANDE, LO CUAL LLAMAMOS h SOBRESUAVIZADA)

h2 <- hlscv(f1)
#### ANCHO DE BANDA POR VALIDACIÓN CRUZADA (ESCRIBIMOS bw.ucv=FALSE PARA QUE NO COJA POR DEFECTO
#### EL ANCHO DE BANDA DE bw.ucv) 

h3 <- hpi(f1)
#### ANCHO DE BANDA plug-in

{
  windows()
  plot(density(f1, bw=h2), t="l", lwd=2, col="red", xlab="Tiempo de erupción", 
    ylab="Densidad", main="Selectores", xlim = c(1, 6), ylim = c(0, 0.7))
  
  lines(density(f1, bw=h1), lwd=2, col="orange")
  lines(density(f1, bw=h3), lwd=2, col="blue")
  rug(f1)
}

#### COMO OBSERVAMOS, EN EL VALLE DE LA DENSIDAD POR VALIDACIÓN CRUZADA, HAY UN AUMENTO EN EL
#### SUAVIZADO DEL VALLE, LO CUAL ES UN INDICIO DE QUE EL ANCHO DE BANDA ES PEQUEÑO, POR LO TANTO,
#### VISTO ESTO, DEBERÍAMOS AUMENTAR UN POCO EL ANCHO DE BANDA, AL MENOS PARA ELIMINAR ESA SUBIDA
#### QUE PARECE DEMASIADO ARTIFICIAL

# Ejemplo por simulación: generamos una muestra de tamaño 1000 de la densidad
# (3/4) N(0, 1) + (1/4) N(1.5, 1/3^2) y pintamos el estimador núcleo con el selector plug-in de
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
    t="l", lwd=3, xlab="x", ylab="f(x)", main="")
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

# Las bandas de variabilidad están implementadas en el paquete sm
# Con la opción panel=TRUE en sm.density se controla el suavizado de manera manual e interactiva

library(sm)
{
  windows()
  sm.density(f1, h=hpi(f1), se=TRUE, xlim=c(1, 6), ylim=c(0, 0.7))
}

library(tkrplot)
{
  windows()
  sm.density(f1, panel=TRUE)
}

#### CON ESTO PODEMOS MOVER EL ANCHO DE BANDA Y ASÍ VER CUÁL ES EL QUE MEJOR NOS CONVIENE Y
#### PODER SELECCIONARLO A OJO DE LA MEJOR MANERA POSIBLE

#### TAL Y COMO PODEMOS VER MOVIENDO LOS VALORES DE LA H, SI TUVIÉSEMOS QUE SELECCIONAR A OJO
#### EL ANCHO DE BANDA, LA MEJOR OPCIÓN SERÍA 0.16 O UN POCO MÁS

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

# El estimador núcleo multivariante también se puede obtener con la función kde del paquete ks

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
