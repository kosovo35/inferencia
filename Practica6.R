######################################
# Análisis de subgrupos
######################################


# AGRUPAMIENTO JERÁRQUICO


# La función hclust realiza el agrupamiento jerárquico en R. Su sintaxis es hclust(d, method = "complete") donde d es la matriz de disparidades (o distancias), que se puede obtener con dist method es el linkage, que puede ser "complete", "average" o "single"

# Consideremos un ejemplo con datos simulados con 25 observaciones en cada grupo

set.seed(2)
x<-matrix(rnorm(50*2), ncol=2)
x[1:25,1]<-x[1:25,1]+3
x[1:25,2]<-x[1:25,2]-4
plot(x)

# Construimos el dendrograma con los 3 tipos de enlace

aj.completo=hclust(dist(x), method="complete")    # ENLACE COMPLETO
aj.medio=hclust(dist(x), method="average")        # ENLACE MEDIO
aj.unico=hclust(dist(x), method="single")         # ENLACE ÚNICO

# Podemos pintar los dendrogramas con plot: hacemos una matriz de gráficas 1x3

op<-par(mfrow=c(1,3))
plot(aj.completo,main="Enlace completo", xlab="", sub="", cex=.9)
plot(aj.medio, main="Enlace medio", xlab="", sub="", cex=.9)
plot(aj.unico, main="Enlace único", xlab="", sub="", cex=.9)

# Vemos cómo el enlace único produce un resultado más extraño. Si separamos en 2 grupos hay un grupo con una única observación y otro grupo con el resto

# Podemos ver las etiquetas de cada grupo con cutree, indicando el número de grupos deseado

grupos.completo<-cutree(aj.completo, 2)
'
[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

Nos indica que de la observación 1 a la 25 las clasificamos en el grupo 1 y de la 26 a la 50 en el grupo 2
'
grupos.medio<-cutree(aj.medio, 2)
'
[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 2

Las primeras 25 observaciones en el grupo 1. Del 26 al 50, mayoría en grupo 2, pero hay mezcla: obs 33, 44 y 46 en grupo 1
'
grupos.unico<-cutree(aj.unico, 2)
'
 [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

Todas las observaciones en el grupo 1 excepto la 16 que la clasifica en el grupo 2
'


# El resultado lo podemos representar gráficamente

plot(x,col=5-grupos.completo,main="Enlace completo",pch=19,cex=1.5)
plot(x,col=5-grupos.medio,main="Enlace medio",pch=19,cex=1.5)
plot(x,col=5-grupos.unico,main="Enlace ?nico",pch=19,cex=1.5)

par(mfrow=c(1,1))

# Algo más coherente se obtiene con enlace único si pedimos 4 grupos, aunque sigue habiendo dos observaciones que constituyen su propio grupo

grupos.unico<-cutree(aj.unico, 4)
'
[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3 3 3 3 3 3 3
'
plot(x,col=5-grupos.unico,main="Enlace ?nico",pch=19,cex=1.5)

# Una función que nos puede ayudar a elegir el número más adecuado de grupos es prediction.strength, de la librería fpc (también necesitamos class)

library(fpc);library(class)
set.seed(1)
ngrupos<-prediction.strength(x,Gmin=2,Gmax=10,M=100,clustermethod=hclustCBI,method="complete") # = 2

# Como el agrupamiento jerárquico se basa en distancias, a veces es recomendable escalar los datos para que todas las coordenadas tengan varianza 1

xsc<-scale(x)
plot(hclust(dist(xsc), method="complete"), main="Agrupamiento jerárquico
     con variables escaladas")


# AGRUPAMIENTO POR K-MEDIAS


# El algoritmo de k-medias está implementado en la función kmeans. Necesita la matriz de datos, el número de grupos (centers) y el número de inicializaciones aleatorias nstart

set.seed(4)
res.km<-kmeans(x,centers=2,nstart=20)

# Los grupos asignados los podemos recuperar con

res.km$cluster
'
[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
'
plot(x, col=5-res.km$cluster, main="Agrupamiento por K-medias con K=2", xlab="", ylab="", pch=19, cex=1.5)

# Veamos qué pasa si forzamos K=3 grupos

set.seed(4)
res.km<-kmeans(x,3,nstart=20)
'
K-means clustering with 3 clusters of sizes 17, 23, 10

Cluster means:
        [,1]        [,2]
1  3.7789567 -4.56200798
2 -0.3820397 -0.08740753
3  2.3001545 -2.69622023

Clustering vector:
 [1] 1 3 1 3 1 1 1 3 1 3 1 3 1 3 1 3 1 1 1 1 1 3 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2

Within cluster sum of squares by cluster:
[1] 25.74089 52.67700 19.56137
 (between_SS / total_SS =  79.3 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"        
[9] "ifault"
'
plot(x, col=5-res.km$cluster, main="Agrupamiento por K-medias con K=3", xlab="", ylab="", pch=19, cex=1.5)

# La componente res.km$tot.withinss es la W_tot que queremos minimizar. Veamos la diferencia entre usar una única inicialización y 20

set.seed(3)
res.km<-kmeans(x,3,nstart=1) # = 97.97927
res.km<-kmeans(x,3,nstart=20) # = 97.97927

# Para pintar W_tot en función de K podemos utilizar un bucle for

Ks<-1:10
Wtot<-numeric(10)
set.seed(3)
for(k in Ks) Wtot[k]<-kmeans(x,centers=k,nstart=20)$tot.withinss
plot(Ks,Wtot,t="b")
# Observamos que el codo de la gráfica está en K = 2 ó K = 3, por lo tanto, estos serían la cantidad de grupos óptima


# Un ejemplo con datos reales: el conjunto de datos planets de la librería HSAUR2 contiene p=3 variables medidas sobre    n=101 exoplanetas. ¿Cuántas categorías distintas de exoplanetas hay? Podemos ver detalles de estas variables con ?planets

library(HSAUR2)
?planets

# Pintamos la matriz de diagramas de dispersión con
pairs(planets)

# Todos las variables son positivas y con largas colas, luego conviene hacer una transformación logarítmica antes de analizarlas

lplan<-log(1+planets)
pairs(lplan)

# Comprobemos si las 3 variables tienen la misma escala
apply(lplan,2,sd)
'
     mass    period     eccen 
0.7120591 2.0683765 0.1616199 
'

# Salen poco parecidas, luego es mejor estandarizar

slplan<-scale(lplan) # variables estandarizadas
pairs(slplan)

# Podemos pintar los datos en 3D con rgl

library(rgl)
open3d()
plot3d(x=slplan[,1], y=slplan[,2], z=slplan[,3], type="s", col="yellow", size=1,
       xlab="masa",ylab="periodo",zlab="excentricidad")

# Investiguemos primero pintando W_tot el número de grupos

Ks<-1:10
Wtot<-numeric(10)
set.seed(3)
for(k in Ks) Wtot[k]<-kmeans(slplan,centers=k,nstart=20)$tot.withinss
plot(Ks,Wtot,t="b")

# Parece haber 3 ó 4 categorías. Siendo conservadores utilizaríamos 3. Observemos el resultado utilizando kmeans

set.seed(1)
slplan.km<-kmeans(slplan,centers=3,nstart=20)
'K-means clustering with 3 clusters of sizes 31, 42, 28

Cluster means:
        mass     period      eccen
1  1.1094462  0.5019882  0.9471295
2 -0.2664146  0.5391742 -0.1177906
3 -0.8286936 -1.3645339 -0.8719217

Clustering vector:
  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31 
  3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   3   2   3   2   2   2   3   2   3   3   3   2   2   2   2   2 
 32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62 
  2   3   2   2   2   2   2   2   2   3   2   1   2   2   2   2   2   1   2   2   3   3   1   2   2   2   2   2   2   2   2 
 63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93 
  2   2   1   2   2   1   1   3   1   1   3   2   1   3   1   1   1   1   1   2   2   2   1   1   1   1   1   1   1   1   1 
 94  95  96  97  98  99 100 101 
  1   1   1   1   1   1   1   1 

Within cluster sum of squares by cluster:
[1] 44.45944 45.04624 28.29321
 (between_SS / total_SS =  60.7 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"        
[9] "ifault"'

# Hay un grupo más numeroso (tamaño 42) y otros dos de tamaños parecidos. Las etiquetas de grupo las recuperamos con

grupos.km<-slplan.km$cluster

# Para interpretar estos grupos podemos volver a los datos originales y calcular las medias de cada grupo

rbind(grupo1=colMeans(planets[grupos.km==1,]),grupo2=colMeans(planets[grupos.km==2,]),
      grupo3=colMeans(planets[grupos.km==3,]))
'           mass    period     eccen
grupo1 7.251935 962.83885 0.4830645
grupo2 1.968381 880.61279 0.2502214
grupo3 1.020500  17.35412 0.1054286'

# El grupo de tamaño 28 lo forman explanetas con masa similar a la Júpiter, con periodos y excentricidades bastante   pequeños. El grupo mayor (de tamaño 42) esta formado por planetas con el doble de masa que Júpiter, periodo más del doble de la Tierra y excentricidad moderada. El tercer grupo, de tamaño 31, estaría formado por planetas enormes, que tienen 3 veces el periodo de la Tierra y mayor excentricidad

# Pintamos la solución respecto a las variables originales

pairs(planets,col=grupos.km+2)

open3d()
plot3d(x=planets[,1], y=planets[,2], z=planets[,3], type="s", col=grupos.km+2, size=1,
       xlab="masa",ylab="periodo",zlab="excentricidad")

# Comparamos con el agrupamiento jerárquico

slplan.aj<-hclust(dist(slplan),method="complete")
plot(slplan.aj,main="Enlace completo", xlab="", sub="", cex=.9)

# El dendrograma más bien parece sugerir 2 ó 4 grupos. De todos modos, vamos a partir en 3 para comparar con k-medias

grupos.aj<-cutree(slplan.aj, 3)

# Para comparar las particiones observamos la tabla cruzada

table(kmedias=grupos.km,jerarquico=grupos.aj)
'       jerarquico
kmedias  1  2  3
      1  0  6 25
      2 15 22  5
      3 27  1  0'

# Hay ciertas diferencias. Veamos las nuevas medias

rbind(grupo1=colMeans(planets[grupos.aj==1,]),grupo2=colMeans(planets[grupos.aj==2,]),
      grupo3=colMeans(planets[grupos.aj==3,]))
'           mass    period     eccen
grupo1 1.244905  393.5076 0.1073405
grupo2 1.817241  451.5949 0.3980000
grupo3 7.702333 1256.5361 0.4128667'

# Comparamos las dos asignaciones gráficamente

pairs(planets,col=grupos.km+2)
pairs(planets,col=grupos.aj+2)

open3d()
plot3d(x=planets[,1], y=planets[,2], z=planets[,3], type="s", col=grupos.aj+2, size=1,
       xlab="masa",ylab="periodo",zlab="excentricidad")

set.seed(1)
ngrupos<-prediction.strength(slplan,Gmin=2,Gmax=10,M=100,clustermethod=hclustCBI,method="complete")
ngrupos$optimalk # = 1


###########################################
# Análisis de subgrupos utilizando modelos de mezcla de normales
###########################################


# La librería principal que vamos a usar es mclust

library(mclust)

# Una guía de referencia para mclust se puede encontrar en la página de los autores http://www.stat.washington.edu/mclust/ y también en la ayuda en pdf de la librería de R

# Vamos a analizar con la función Mclust los datos faithful

plot(faithful)
faithful.mclust<-Mclust(faithful)  # análisis del modelo de mezclas (este algoritmo usa el EM)

# Un resumen de los resultados lo tenemos en

summary(faithful.mclust)
'---------------------------------------------------- 
Gaussian finite mixture model fitted by EM algorithm 
---------------------------------------------------- 

Mclust EEE (ellipsoidal, equal volume, shape and orientation) model with 3 components: 

 log-likelihood   n df       BIC       ICL    (criterios de calidad de ajuste)
      -1126.326 272 11 -2314.316 -2357.824

Clustering table (indica cuántos puntos hay en cada grupo):
  1   2   3 
 40  97 135 '

# Sugiere un modelo con 3 componentes, los 3 con igual volumen, forma y orientación (modelo EEE)

# Podemos obtener más detalles con

summary(faithful.mclust,parameters=TRUE)
'---------------------------------------------------- 
Gaussian finite mixture model fitted by EM algorithm 
---------------------------------------------------- 

Mclust EEE (ellipsoidal, equal volume, shape and orientation) model with 3 components: 

 log-likelihood   n df       BIC       ICL
      -1126.326 272 11 -2314.316 -2357.824

Clustering table:
  1   2   3 
 40  97 135 

Mixing probabilities (probabilidad de mezcla [peso relativo de cada componente]):
        1         2         3 
0.1656784 0.3563696 0.4779520 

Means:    (medias de cada grupo)
               [,1]      [,2]      [,3]
eruptions  3.793066  2.037596  4.463245
waiting   77.521051 54.491158 80.833439

Variances:     (La matriz de varianzas y covarianzas de cada componente (idéntica para los 3 porque es modelo EEE))
[,,1]
           eruptions    waiting
eruptions 0.07825448  0.4801979
waiting   0.48019785 33.7671464
[,,2]
           eruptions    waiting
eruptions 0.07825448  0.4801979
waiting   0.48019785 33.7671464
[,,3]
           eruptions    waiting
eruptions 0.07825448  0.4801979
waiting   0.48019785 33.7671464'

# La representación gráfica se obtiene con

plot(faithful.mclust)
'
1. Valores de BIC por modelo y número de componentes.
2. Clustering asignado a los datos.
3. Incertidumbre (cuán seguros estamos de la asignación).
4.Log-densidad estimada.
'

# Sucesivamente vamos viendo las distintas funciones BIC para todos los modelos con número de componentes de 1 a 9, los grupos, un gráfico con la medida de  la incertidumbre, y la gráfica de la log-densidad

# Si queremos centrarnos sólo en el análisis de los distintos modelos de ajuste podemos utilizar mclustBIC (calcula y resume los BIC de todos los modelos posibles.)

faithful.BIC<-mclustBIC(faithful)

# Los 3 mejores modelos los obtenemos con

summary(faithful.BIC,data=faithful)
'Best BIC values:
             EEE,3        VVE,2        VEE,3
BIC      -2314.316 -2320.432980 -2322.103490
BIC diff     0.000    -6.116684    -7.787194

Classification table for model (EEE,3): 

  1   2   3 
 40  97 135 '

# El mejor modelo sigue siendo EEE, estando cerca VVE con 2 componentes y VEE con 3

# La información completa se obtiene con

faithful.BIC
'Bayesian Information Criterion (BIC): 
        EII       VII       EEI       VEI       EVI       VVI       EEE       VEE       EVE       VVE       EEV       VEV
1 -4024.721 -4024.721 -3055.835 -3055.835 -3055.835 -3055.835 -2607.623 -2607.623 -2607.623 -2607.623 -2607.623 -2607.623
2 -3452.998 -3458.305 -2354.601 -2350.607 -2352.618 -2346.065 -2325.220 -2322.972 -2324.273 -2320.433 -2329.115 -2325.416
3 -3377.701 -3336.598 -2323.014 -2332.687 -2332.205 -2342.366 -2314.316 -2322.103 -2342.319 -2336.271 -2325.322 -2329.648
4 -3230.264 -3242.826 -2323.673 -2331.284 -2334.749 -2343.486 -2331.223 -2340.173 -2361.821 -2362.487 -2351.523 -2361.084
5 -3149.394 -3129.080 -2327.059 -2350.230 -2347.564 -2351.017 -2360.659 -2347.337 -2351.828 -2368.937 -2356.856 -2368.101
6 -3081.414 -3038.171 -2338.205 -2360.578 -2357.660 -2373.469 -2347.352 -2372.287 -2366.482 -2386.537 -2366.087 -2386.323
7 -2990.367 -2973.374 -2356.454 -2368.513 -2372.851 -2394.696 -2369.330 -2371.175 -2379.810 -2402.220 -2379.071 -2401.270
8 -2978.100 -2935.082 -2364.140 -2384.740 -2389.064 -2413.705 -2376.104 -2390.391 -2403.934 -2425.956 -2392.988 -2425.426
9 -2953.359 -2919.415 -2372.790 -2398.223 -2407.224 -2432.708 -2389.609 -2406.732 -2414.089 -2448.208 -2407.500 -2446.726
        EVV       VVV
1 -2607.623 -2607.623
2 -2327.598 -2322.192
3 -2339.983 -2349.696
4 -2344.686 -2351.493
5 -2364.900 -2379.388
6 -2384.117 -2387.016
7 -2398.703 -2412.440
8 -2414.962 -2442.018
9 -2438.876 -2460.398

Top 3 models based on the BIC criterion: 
    EEE,3     VVE,2     VEE,3 
-2314.316 -2320.433 -2322.103'

# Hay modelos que no se muestran, porque el algoritmo EM no ha sido capaz de ajustar los parámetros del modelo

# Pintando el resultado de mclustBIC podemos especificar qué modelos y cuántas componentes queremos comparar

plot(faithful.BIC, G = 1:7, ylim = c(-2400,-2300), 
     legendArgs = list(x = "bottomright", ncol = 7))

# Para investigar el resultado del ajuste, podemos pintar la densidad estimada

faithful.dens<-densityMclust(faithful)
plot(faithful.dens,faithful,drawlabels=FALSE)
plot(faithful.dens, type = "persp", col = grey(0.8))

# Otra manera de ver si dos componentes se pueden combinar es utilizar

faithful.comb<-clustCombi(data=faithful,object=faithful.mclust) # intenta fusionar componentes si tienen mucha superposición
faithful.comb # se usa la entropía para determinar el número óptimo de grupos combinados (buscar el “codo” en la curva).

# Podemos pintar las distintas combinaciones y elegir el número de grupos localizando el codo de la entropía como función del número de grupos

plot(faithful.comb, what = "entropy") 

# Utilizando clustering modal

faithful.gmmhd<-gmmhd(faithful.mclust)
faithful.gmmhd
'"gmmhd" model object:
 Mclust initial model = (EEE,3)
 GMMHD final number of clusters = 2'
plot(faithful.gmmhd)


# Un ejemplo más complicado es el de los datos wreath. Son 14 pequeños subgrupos, con matrices de covarianza con igual forma y tamaño, pero distinta orientación

data(wreath)
plot(wreath, pch = 20, cex = 0.5)

# Tal y como podemos observar, a simple vista se distinguen 14 grupos y por defecto Mclust() solo considera hasta 9 grupos, de forma que se quedaría corto

wreath.mclust<-Mclust(wreath)

# Ampliemos el número máximo de grupos hasta 20

wreath.mclust<-Mclust(wreath,G=1:20)
summary(wreath.mclust)
'
---------------------------------------------------- 
Gaussian finite mixture model fitted by EM algorithm 
---------------------------------------------------- 

Mclust EEV (ellipsoidal, equal volume and shape) model with 14 components: 

 log-likelihood    n df       BIC       ICL
      -5254.513 1000 57 -10902.77 -10902.91

Clustering table:
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 
74 69 63 74 68 70 71 66 83 77 66 77 61 81 
'
# Tal y como podemos observar, el modelo toma ahora 14 componentes que era lo que más o menos se observaba a simple vista

wreath.dens<-densityMclust(wreath,G=14) # ajusta un modelo de mezcla gaussiana no supervisado (basado en EM y BIC), enfocado en modelar la densidad subyacente de los datos. Le estamos diciendo que tome exactamente 14 componentes, que son las que nos ha dicho Mclust que tomemos. Internamente usa el mismo modelo que Mclust pero centrado en la densidad.
plot(wreath.dens,drawlabels=FALSE)
plot(wreath.dens, type = "persp", col = grey(0.8))


