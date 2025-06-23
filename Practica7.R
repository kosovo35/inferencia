#######################################
## PCA
#######################################

# Vamos a utilizar los datos USArrests para hacer el PCA

data(USArrests)

# Vamos a guardar los nombres de los estados

estados<-row.names(USArrests)

# Podemos pintar la matriz de diagramas de dispersión, incluyendo sólo la parte triangular inferior

windows()
pairs(USArrests, upper.panel=NULL)

# Veamos la escala de cada una de las variables

apply(USArrests,2,sd)
'   Murder   Assault  UrbanPop      Rape 
 4.355510 83.337661 14.474763  9.366385 '

# Son muy diferentes (miden cosas muy diferentes)
# Por tanto, es muy recomendable reescalar las variables
# Esto es una opción de prcomp(), tomando scale = TRUE

pca<-prcomp(USArrests, scale=TRUE)

# La matriz de cargas puede obtenerse con pca$rotation

pca$rotation
'                PC1        PC2        PC3         PC4
Murder   -0.5358995 -0.4181809  0.3412327  0.64922780
Assault  -0.5831836 -0.1879856  0.2681484 -0.74340748
UrbanPop -0.2781909  0.8728062  0.3780158  0.13387773
Rape     -0.5434321  0.1673186 -0.8177779  0.08902432'

# Tenemos las 4 componentes principales


# Para cada punto, sus coordenadas con respecto a las nuevas variables quedan almacenadas en pca$x

pca$x
'                       PC1         PC2         PC3          PC4
Alabama        -0.97566045 -1.12200121  0.43980366  0.154696581
Alaska         -1.93053788 -1.06242692 -2.01950027 -0.434175454
Arizona        -1.74544285  0.73845954 -0.05423025 -0.826264240
Arkansas        0.13999894 -1.10854226 -0.11342217 -0.180973554
California     -2.49861285  1.52742672 -0.59254100 -0.338559240
Colorado       -1.49934074  0.97762966 -1.08400162  0.001450164
Connecticut     1.34499236  1.07798362  0.63679250 -0.117278736
Delaware       -0.04722981  0.32208890  0.71141032 -0.873113315
Florida        -2.98275967 -0.03883425  0.57103206 -0.095317042
Georgia        -1.62280742 -1.26608838  0.33901818  1.065974459
Hawaii          0.90348448  1.55467609 -0.05027151  0.893733198
Idaho           1.62331903 -0.20885253 -0.25719021 -0.494087852
Illinois       -1.36505197  0.67498834  0.67068647 -0.120794916
Indiana         0.50038122  0.15003926 -0.22576277  0.420397595
Iowa            2.23099579  0.10300828 -0.16291036  0.017379470
Kansas          0.78887206  0.26744941 -0.02529648  0.204421034
Kentucky        0.74331256 -0.94880748  0.02808429  0.663817237
Louisiana      -1.54909076 -0.86230011  0.77560598  0.450157791
Maine           2.37274014 -0.37260865  0.06502225 -0.327138529
Maryland       -1.74564663 -0.42335704  0.15566968 -0.553450589
Massachusetts   0.48128007  1.45967706  0.60337172 -0.177793902
Michigan       -2.08725025  0.15383500 -0.38100046  0.101343128
Minnesota       1.67566951  0.62590670 -0.15153200  0.066640316
Mississippi    -0.98647919 -2.36973712  0.73336290  0.213342049
Missouri       -0.68978426  0.26070794 -0.37365033  0.223554811
Montana         1.17353751 -0.53147851 -0.24440796  0.122498555
Nebraska        1.25291625  0.19200440 -0.17380930  0.015733156
Nevada         -2.84550542  0.76780502 -1.15168793  0.311354436
New Hampshire   2.35995585  0.01790055 -0.03648498 -0.032804291
New Jersey     -0.17974128  1.43493745  0.75677041  0.240936580
New Mexico     -1.96012351 -0.14141308 -0.18184598 -0.336121113
New York       -1.66566662  0.81491072  0.63661186 -0.013348844
North Carolina -1.11208808 -2.20561081  0.85489245 -0.944789648
North Dakota    2.96215223 -0.59309738 -0.29824930 -0.251434626
Ohio            0.22369436  0.73477837  0.03082616  0.469152817
Oklahoma        0.30864928  0.28496113  0.01515592  0.010228476
Oregon         -0.05852787  0.53596999 -0.93038718 -0.235390872
Pennsylvania    0.87948680  0.56536050  0.39660218  0.355452378
Rhode Island    0.85509072  1.47698328  1.35617705 -0.607402746
South Carolina -1.30744986 -1.91397297  0.29751723 -0.130145378
South Dakota    1.96779669 -0.81506822 -0.38538073 -0.108470512
Tennessee      -0.98969377 -0.85160534 -0.18619262  0.646302674
Texas          -1.34151838  0.40833518  0.48712332  0.636731051
Utah            0.54503180  1.45671524 -0.29077592 -0.081486749
Vermont         2.77325613 -1.38819435 -0.83280797 -0.143433697
Virginia        0.09536670 -0.19772785 -0.01159482  0.209246429
Washington      0.21472339  0.96037394 -0.61859067 -0.218628161
West Virginia   2.08739306 -1.41052627 -0.10372163  0.130583080
Wisconsin       2.05881199  0.60512507  0.13746933  0.182253407
Wyoming         0.62310061 -0.31778662  0.23824049 -0.164976866'

# Para pintar el biplot utilizamos la función del mismo nombre. Con scale=0 las flechas están escaladas para representar las cargas

windows()
biplot(pca, scale=0)

# Salen en la dirección opuesta al dibujo de clase. Esto es porque los autovectores están determinados  salvo el signo, pero podemos cambiarlos

pca$rotation=-pca$rotation
pca$x=-pca$x
biplot(pca, scale=0)

# La función prcomp también te devuelve la desviación típica explicada por cada componente

pca$sdev # = (1.5748783; 0.9948694; 0.5971291; 0.4164494)

# Para calcular la proporción de varianza explicada 

pve<-pca$sdev^2/sum(pca$sdev^2) # = (0.62006039; 0.24744129; 0.08914080; 0.04335752)

# la primera componente explica el 62% de la varianza, la segunda el 24.75%, la tercera el 8.91% y la cuarta el 4.33%

# Para obtener la varianza explicada por cada componente

pca$sdev^2 # = (2.4802416; 0.9897652; 0.3565632; 0.1734301)

# Los scree plot los conseguimos poniendo

windows()
plot(pve , xlab="Componente principal", ylab="PVE", ylim=c(0,1) ,type="b")

windows()
plot(cumsum (pve ), xlab="Componente principal", ylab ="
PVE acumulada", ylim=c(0,1), type="b")


###########################################
### Escalado multidimensional
###########################################

# Existen varias funciones para hacer MDS en R, pero vamos a utilizar principalmente cmdscale para el MDS clásico (el visto en clase)

# El conjunto de datos eurodist contiene las distancias entre 21 ciudades europeas (en kilómetros)

data(eurodist)
as.matrix(eurodist)[1:5,1:5]
'          Athens Barcelona Brussels Calais Cherbourg
Athens         0      3313     2963   3175      3339
Barcelona   3313         0     1318   1326      1294
Brussels    2963      1318        0    204       583
Calais      3175      1326      204      0       460
Cherbourg   3339      1294      583    460         0'

# El objetivo sería intentar recuperar una representación bidimensional conociendo sólo dichas distancias

mds <- cmdscale(eurodist)
'                        [,1]        [,2]
Athens           2290.274680  1798.80293
Barcelona        -825.382790   546.81148
Brussels           59.183341  -367.08135
Calais            -82.845973  -429.91466
Cherbourg        -352.499435  -290.90843
Cologne           293.689633  -405.31194
Copenhagen        681.931545 -1108.64478
Geneva             -9.423364   240.40600
Gibraltar       -2048.449113   642.45854
Hamburg           561.108970  -773.36929
Hook of Holland   164.921799  -549.36704
Lisbon          -1935.040811    49.12514
Lyons            -226.423236   187.08779
Madrid          -1423.353697   305.87513
Marseilles       -299.498710   388.80726
Milan             260.878046   416.67381
Munich            587.675679    81.18224
Paris            -156.836257  -211.13911
Rome              709.413282  1109.36665
Stockholm         839.445911 -1836.79055
Vienna            911.230500   205.93020'

# La salida son las coordenadas en R^2 representa la posición de las ciudades en el plano.

# Podemos ver el grado de fidelidad que tienen algunas de las distancias de los puntos obtenidos por MDS con respecto a las distancias originales; por ejemplo, entre Atenas y Barcelona

sqrt(sum((mds[1,]-mds[2,])^2)) # = 3357.798  (distancia entre las coordenadas de Atenas y Barcelona obtenidas por MDS)
as.matrix(eurodist)[1,2] # = 3313 (distancia real que se encuentra en el elemento (2,1) de la matriz de proximidad)

# Mientras que los datos de la matriz 21 x 21 inicial eran difíciles de representar, ahora podemos pintar las coordenadas bidimensionales obtenidas por MDS

windows()
plot(mds)

# En vez de puntos, pongamos los nombres de las ciudades

plot(mds, type = 'n')
text(mds[, 1], mds[, 2], labels(eurodist))

# Recordemos que la solución dada por MDS no es única, sino que es única salvo rotaciones
# Si multiplicamos la coordenada y por -1 (rotación de 180º) tenemos

plot(mds, type = 'n')
text(mds[, 1], -mds[, 2], labels(eurodist))

# El paquete ggplot2 permite refinar un poco el gráfico anterior, evitando el solapamiento de los nombres y eliminando los ejes, que no son muy informativos en cualquier caso

library(ggplot2)
ggplot(as.data.frame(mds), aes(V1, -V2, label = rownames(mds))) +
       geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('') +
       scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

# Otro ejemplo, no para plasmar distancias entre ciudades sino para representar datos de mayor dimensión en R2, es el del conjunto mtcars

data(mtcars)

as.matrix(mtcars)[1:5,1:5]

dist_mtcars <- dist(mtcars)

mds <- cmdscale(dist_mtcars)

windows()
ggplot(as.data.frame(mds), aes(V1, V2, label = rownames(mds))) +
  geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

