# Cargamos el paquete ISLR

library(ISLR)
attach(Caravan)

# La tabla de datos Caravan del paquete ISLR contiene p=85 predictores que registran características demográficas de      n=5822 personas. La variable respuesta es Purchase, que indica si un individuo contrata una póliza de seguros para una caravana. Sólo hay 348 individuos que la contratan (6%)

# Datos Caravan (la variable 86 es la que queremos predecir)
# Hay que tipificar para usar knn

Xsc<-scale(Caravan[,-86]) # centra las variables cuantitativas, es decir, todas menos la última (Purchase) 
apply(Xsc,2,sd)  # Tipifica


#############################################
# K-VECINOS MÁS PRÓXIMOS
#############################################


# Con knn.cv predice la clase en los datos de entrenamiento por VC

library(class)
caravan.knn.cv<-knn.cv(train=Xsc,
                       cl=Caravan[,86],
                       k=13)
mean(caravan.knn.cv!=Caravan[,86]) # = 0.06028856

# Con una muestra de prueba

test<-1:1000 # muestra de entrenamiento
Xtest<-Xsc[test,] # valores de X de la muestra de prueba
Xentrena<-Xsc[-test,] # valores de X de la muestra de entrenamiento

Ytest<-Caravan[test,86] # valores de Y de la muestra de prueba
Yentrena<-Caravan[-test,86] # valores de Y de la muestra de entrenamiento

mean(as.numeric(Yentrena)-1) # = 0.05993364     Se ha mantenido la proporción de clases
mean(as.numeric(Ytest)-1) # = 0.059      (Error de prueba)

caravan.knn<-knn(train=Xentrena,
                 test=Xtest,
                 cl=Yentrena,
                 k=13)

# Matriz de confusión
table(predichos=caravan.knn,verdaderos=Ytest)
'
         verdaderos
predichos  No Yes
      No  941  59
      Yes   0   0
'
# Error de prueba
mean(caravan.knn!=Ytest) # = 0.059

# Ajustamos el número de vecinos
library(caret)

datos.entrena<-Caravan[-test,]

set.seed(1)
knnTune<-train(Purchase~., data=datos.entrena,
               preProcess=c("center","scale"),
               method="knn",
               tuneGrid=expand.grid(k=c(7,9,11,13,15,17)),
               trControl=trainControl(method="cv",number = 10)
               )
windows()
plot(knnTune)  # Obtenemos que el número de vecinos que más precisión obtiene es K = 11

knnPredict <- predict(knnTune,newdata = Caravan[test,])

confusionMatrix(knnPredict,Caravan[test,86])
'
Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  941  59
       Yes   0   0
                                          
               Accuracy : 0.941           
                 95% CI : (0.9246, 0.9548)
    No Information Rate : 0.941           
    P-Value [Acc > NIR] : 0.5346          
                                          
                  Kappa : 0               
                                          
 Mcnemar`s Test P-Value : 4.321e-14       

Sensitivity : 1.000           
Specificity : 0.000           
Pos Pred Value : 0.941           
Neg Pred Value :   NaN           
Prevalence : 0.941           
Detection Rate : 0.941           
Detection Prevalence : 1.000           
Balanced Accuracy : 0.500           

"Positive" Class : No     
'

prob.knn<-predict(knnTune,newdata = Caravan[-test,], type="prob" )  # valores predichos para la muestra de entrenamiento


# Curva ROC

library(pROC)

knn.roc<-roc(response=datos.entrena$Purchase, 
              predictor=prob.knn[,2])

windows()
plot(knn.roc, legacy.axes=TRUE, print.thres=0.5, 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad",
     print.auc=TRUE)

auc.log<-knn.roc$auc # = 0.8667

# Pintamos la curva ROC con el mejor valor de lambda
windows()
plot(knn.roc, legacy.axes=TRUE, print.thres="best", 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad")

# Matriz de confusión
table(predichos=prob.knn[,2]>0.074,verdaderos=Yentrena)
'
         verdaderos
predichos   No  Yes
    FALSE 2697    0
    TRUE  1836  289
'


####################################
# Datos de spam
####################################

# Son n=4601 observaciones, tenemos p=57 predictores. Una última variable de tipo 0/1 (No Spam/Sí Spam) 
# Hay 1813 correos spam = 39.4%

# Los predictores miden la proporción de veces que aparece cierta palabra en el texto, o ciertos caracteres en el texto, o ciertos indicadores sobre el uso de mayúsculas, etc.

library(tree)

spam<-read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data",header=FALSE)
spam$V58<-as.factor(spam$V58)
#levels(spam$V58)<-c("No","Sí")

# Árbol de clasificación
spam.tree<-tree(V58~.,data=spam)

windows();
plot(spam.tree); text(spam.tree)

summary(spam.tree)
'
Classification tree:
tree(formula = V58 ~ ., data = spam)
Variables actually used in tree construction:
 [1] "V53" "V7"  "V52" "V25" "V56" "V5"  "V55" "V16" "V27" "V46"
Number of terminal nodes:  13 
Residual mean deviance:  0.4879 = 2238 / 4588 
Misclassification error rate: 0.08259 = 380 / 4601 
'

# Construimos el conjunto de prueba

test<-read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.traintest",header=FALSE)
test<-which(test$V1==1)

# Calculamos el árbol sin los datos de prueba y el error de predicción

spam.tree<-tree(V58~.,data=spam[-test,])
# Predecimos los valores del árbol para los datos de prueba
spam.tree.pred<-predict(spam.tree,newdata=spam[test,],type="class")
# Error de predicción del árbol de clasificación (spam.tree) sobre los datos de prueba
mean(spam.tree.pred!=spam$V58[test]) # = 0.1002604
# Matriz de confusión
table(predichos=spam.tree.pred,
      verdaderos=spam$V58[test])
'
         verdaderos
predichos   0   1
        0 881  94
        1  60 501
'

# Podamos con VC

set.seed(1)
spam.cv<-cv.tree(spam.tree,FUN=prune.misclass) # usa el número de errores de clasificación como criterio de evaluación
spam.cv
'
$size
[1] 14  9  8  7  6  5  3  2  1   ---> tamaño del árbol tras cada poda (p.ej. size[1] = 14 significa que en la primera poda el árbol se queda con 14 hojas)

Como length(size) = 9, significa que se realizan 9 podas

$dev
[1]  303  303  306  316  326  379  444  617 1218   ---> Número de errores de clasificación (El mínimo está en dev = 303, y se alcanza tanto para size = 14 como para size = 9)

$k
[1] -Inf    0   10   14   21   34   51  176  601   ---> penalización del tamaño del árbol

$method
[1] "misclass"

attr(,"class")
[1] "prune"         "tree.sequence"
'

# La conclusión que podemos extraer tras analizar la poda del árbol es que el número mínimo de errores (303) se obtiene tanto para árboles de tamaño 14 como de tamaño 9. Por complejidad, podar a size = 9 puede ser una buena opción, ya que reduce la complejidad sin aumentar el error 

spam.podado<-prune.misclass(spam.tree,best=9) # podamos el árbol con 9 hojas
plot(spam.podado);text(spam.podado)

# Calculamos el error de prueba para el árbol podado

spam.podado.pred<-predict(spam.podado,newdata=spam[test,],type="class")
mean(spam.podado.pred!=spam[test,58]) # = 0.1002604
table(spam.podado.pred,spam[test,58])
'
spam.podado.pred   0   1
               0 881  94
               1  60 501
'

# Los nombres de las variables los podemos leer de un archivo
nombres<-as.character(read.table("spamnames.txt",header=FALSE)$V1)


# BAGGING


# El error de clasificación queda aproximadamente en un 10%. Vamos a ver como podemos mejorar dicha tasa de error usando bagging. Utilizaremos la librería randomForest, fijando la semilla para que todos obtengamos los mismos resultados

library(randomForest)
set.seed(1)

# En la función randomForest podemos especificar cuáles son las observaciones de entrenamiento (opción subset), cuántos árboles aleatorios queremos construir (opción ntree) y el número de variables que vamos a probar en cada división (mtry)

# Para hacer bagging incluimos todas las variables predictoras: mtry=57

ntrain<-(1:nrow(spam))[-test]
spam.bag<-randomForest(V58~.,data=spam,subset=ntrain,
                       ntree=500,mtry=57, importance =TRUE)
spam.bag
'
Call:
 randomForest(formula = V58 ~ ., data = spam, ntree = 500, mtry = 57,      importance = TRUE, subset = ntrain) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 57

        OOB estimate of  error rate: 5.87%
Confusion matrix:
     0    1 class.error
0 1775   72  0.03898213
1  108 1110  0.08866995
'

# La estimación out-of-bag del error de clasificación es 5.87%. Comete menor error cuando clasifica como no spam. Si pintamos la salida devuelta obtenemos cómo desciende el error a medida que tomamos más árboles

plot(spam.bag)

# Recordemos que la estimación OOB está calculada utilizando la muestra de entrenamiento. Veamos cómo lo hace en la muestra de prueba.

spam.bag.pred<-predict(spam.bag,newdata=spam[test,])
mean(spam.bag.pred!=spam[test,58]) # = 0.05403646 (5.34%)
table(spam.bag.pred,spam[test,58])
'
spam.bag.pred   0   1
            0 904  46
            1  37 549
'

# Se confirma que hemos mejorado el error de clasificación, reduciéndolo al 5.34%

# Podemos ver cuáles son las variables más importantes a la hora de clasificar un correo como spam o no spam

varImpPlot(spam.bag) # Esta gráfica representa las importancias de las variables utilizadas en el modelo de bosque aleatorio. Para ver concrétamente cuáles son estas variables más importantes podemos hacer

importance(spam.bag)
'
              0          1 MeanDecreaseAccuracy MeanDecreaseGini
V1   4.59894657  2.9643174             5.515818        3.8650018
V2   5.72833791  2.3442511             5.929268        3.2911670
V3  -0.99965717  4.6479792             2.544356        4.8184575
V4  12.04202883 -3.7137482             9.595757        2.1247338
V5  21.89593568 18.0402878            27.025064       18.6490710
V6  13.53449112 13.9008471            17.817121        6.5959996
V7  97.84820906 51.9572451           107.611871      194.1542293
V8  16.75063981 10.0694144            18.406317        9.6824038
V9  12.17422312 10.6925723            15.335100        4.8531062
V10  9.59135854  8.4491209            12.202683        8.2365326
V11 17.25967634  3.7915623            17.998739        8.7956630
V12  5.07553791 15.5258752            15.592866       13.2779261
V13  3.04337254  8.4542167             8.439329        4.3295083
V14  4.94511470 12.3747733            12.473057        3.0868077
V15  4.90412430  1.7300436             4.800888        0.5747034
V16 41.06706193 31.0225132            48.867625       60.3065298
V17 32.36035739 17.8461646            35.091236       19.3495505
V18 15.83349719  7.8481256            17.002819       10.1425547
V19 14.39401118 13.2765500            19.764462       25.8661692
V20 10.63531172 -4.0796153            10.407929        1.7490227
V21 16.61112773 13.8175601            20.954728       23.0585802
V22 20.64382917 13.5319194            21.840305        4.4971592
V23 16.82993029  1.1753408            17.047150        8.2188411
V24 22.04906623  6.8987520            23.255455       14.5948129
V25 27.17409938 85.7028780            68.610293       73.4928110
V26  1.73100845 13.8528397            14.515048        2.9726618
V27 15.77278838 42.8356721            46.958693       25.1539318
V28  8.35157946  9.1747885            12.046846        4.0080593
V29 -8.35225718 20.8578737            20.790774        2.1621331
V30  6.15131689  4.7663665             7.989161        1.8382447
V31  2.27155363  8.8746544             9.676553        0.9609224
V32 -0.30926617  7.4451985             7.643269        0.8302875
V33 -1.95218626 10.7370019             9.537110        5.7458003
V34  3.57763075  5.2352313             5.761358        0.5747138
V35  4.80503891  7.2249978             8.198459        1.3938299
V36 10.32952442  3.0091331            10.868121        2.8287860
V37  7.78263091 23.2633562            23.983903        6.9329332
V38 -0.03867955  7.5085284             4.936914        0.9203211
V39  5.93658347 15.1321089            15.261668        6.7965743
V40  3.28834649 -0.9399261             3.154692        0.4193381
V41 -4.45008088 12.6752499            12.004367        1.3250610
V42  5.69705076 36.9420567            36.142998        8.4512562
V43 -5.82422229 20.6815046            20.494810        3.1038654
V44  4.48293893  4.1061072             5.898511        0.8958091
V45 13.84344828 18.0562565            22.200834       11.4450422
V46 15.93609475 72.6753810            70.139377       24.7000067
V47  1.60736080  1.9324426             2.829708        0.3910813
V48  7.81506023 22.9710623            23.550978        3.3408034
V49 11.23865809 12.9058813            16.392533        5.7101127
V50  6.37179254 16.3884920            17.202420       12.4798590
V51  4.05822087  4.1882703             5.576887        1.8867295
V52 51.08827059 45.6997842            71.859136      236.4773324
V53 36.82838552 42.5344252            59.526048      408.5418666
V54  3.01606822  3.4472749             4.688521        1.8940500
V55 35.07187539 18.6709962            38.430291       69.2548588
V56 25.99093363 32.9707745            42.758375       40.2054082
V57 24.01860850 19.0730831            30.326317       45.2936973
'
# Pero tal y como vemos, estas variables están ordenadas por nombre, no por importancia, por lo tanto:

# Ordenar por importancia en precisión
importance(spam.bag)[order(importance(spam.bag)[, "MeanDecreaseAccuracy"], decreasing = TRUE), ]
'
              0          1 MeanDecreaseAccuracy MeanDecreaseGini
V7  97.84820906 51.9572451           107.611871      194.1542293
V52 51.08827059 45.6997842            71.859136      236.4773324
V46 15.93609475 72.6753810            70.139377       24.7000067
V25 27.17409938 85.7028780            68.610293       73.4928110
V53 36.82838552 42.5344252            59.526048      408.5418666
V16 41.06706193 31.0225132            48.867625       60.3065298
V27 15.77278838 42.8356721            46.958693       25.1539318
V56 25.99093363 32.9707745            42.758375       40.2054082
V55 35.07187539 18.6709962            38.430291       69.2548588
V42  5.69705076 36.9420567            36.142998        8.4512562
V17 32.36035739 17.8461646            35.091236       19.3495505
V57 24.01860850 19.0730831            30.326317       45.2936973
V5  21.89593568 18.0402878            27.025064       18.6490710
V37  7.78263091 23.2633562            23.983903        6.9329332
V48  7.81506023 22.9710623            23.550978        3.3408034
V24 22.04906623  6.8987520            23.255455       14.5948129
V45 13.84344828 18.0562565            22.200834       11.4450422
V22 20.64382917 13.5319194            21.840305        4.4971592
V21 16.61112773 13.8175601            20.954728       23.0585802
V29 -8.35225718 20.8578737            20.790774        2.1621331
V43 -5.82422229 20.6815046            20.494810        3.1038654
V19 14.39401118 13.2765500            19.764462       25.8661692
V8  16.75063981 10.0694144            18.406317        9.6824038
V11 17.25967634  3.7915623            17.998739        8.7956630
V6  13.53449112 13.9008471            17.817121        6.5959996
V50  6.37179254 16.3884920            17.202420       12.4798590
V23 16.82993029  1.1753408            17.047150        8.2188411
V18 15.83349719  7.8481256            17.002819       10.1425547
V49 11.23865809 12.9058813            16.392533        5.7101127
V12  5.07553791 15.5258752            15.592866       13.2779261
V9  12.17422312 10.6925723            15.335100        4.8531062
V39  5.93658347 15.1321089            15.261668        6.7965743
V26  1.73100845 13.8528397            14.515048        2.9726618
V14  4.94511470 12.3747733            12.473057        3.0868077
V10  9.59135854  8.4491209            12.202683        8.2365326
V28  8.35157946  9.1747885            12.046846        4.0080593
V41 -4.45008088 12.6752499            12.004367        1.3250610
V36 10.32952442  3.0091331            10.868121        2.8287860
V20 10.63531172 -4.0796153            10.407929        1.7490227
V31  2.27155363  8.8746544             9.676553        0.9609224
V4  12.04202883 -3.7137482             9.595757        2.1247338
V33 -1.95218626 10.7370019             9.537110        5.7458003
V13  3.04337254  8.4542167             8.439329        4.3295083
V35  4.80503891  7.2249978             8.198459        1.3938299
V30  6.15131689  4.7663665             7.989161        1.8382447
V32 -0.30926617  7.4451985             7.643269        0.8302875
V2   5.72833791  2.3442511             5.929268        3.2911670
V44  4.48293893  4.1061072             5.898511        0.8958091
V34  3.57763075  5.2352313             5.761358        0.5747138
V51  4.05822087  4.1882703             5.576887        1.8867295
V1   4.59894657  2.9643174             5.515818        3.8650018
V38 -0.03867955  7.5085284             4.936914        0.9203211
V15  4.90412430  1.7300436             4.800888        0.5747034
V54  3.01606822  3.4472749             4.688521        1.8940500
V40  3.28834649 -0.9399261             3.154692        0.4193381
V47  1.60736080  1.9324426             2.829708        0.3910813
V3  -0.99965717  4.6479792             2.544356        4.8184575
'
# Obtendríamos que según la precisión, las tres variables más importantes serían la V7, V52, V46, cuyos nombres son:

nombres[c(7,52,46)] # = "word_freq_remove" "char_freq_!" "word_freq_edu"   

# O por importancia en Gini
importance(spam.bag)[order(importance(spam.bag)[, "MeanDecreaseGini"], decreasing = TRUE), ]
'
              0          1 MeanDecreaseAccuracy MeanDecreaseGini
V53 36.82838552 42.5344252            59.526048      408.5418666
V52 51.08827059 45.6997842            71.859136      236.4773324
V7  97.84820906 51.9572451           107.611871      194.1542293
V25 27.17409938 85.7028780            68.610293       73.4928110
V55 35.07187539 18.6709962            38.430291       69.2548588
V16 41.06706193 31.0225132            48.867625       60.3065298
V57 24.01860850 19.0730831            30.326317       45.2936973
V56 25.99093363 32.9707745            42.758375       40.2054082
V19 14.39401118 13.2765500            19.764462       25.8661692
V27 15.77278838 42.8356721            46.958693       25.1539318
V46 15.93609475 72.6753810            70.139377       24.7000067
V21 16.61112773 13.8175601            20.954728       23.0585802
V17 32.36035739 17.8461646            35.091236       19.3495505
V5  21.89593568 18.0402878            27.025064       18.6490710
V24 22.04906623  6.8987520            23.255455       14.5948129
V12  5.07553791 15.5258752            15.592866       13.2779261
V50  6.37179254 16.3884920            17.202420       12.4798590
V45 13.84344828 18.0562565            22.200834       11.4450422
V18 15.83349719  7.8481256            17.002819       10.1425547
V8  16.75063981 10.0694144            18.406317        9.6824038
V11 17.25967634  3.7915623            17.998739        8.7956630
V42  5.69705076 36.9420567            36.142998        8.4512562
V10  9.59135854  8.4491209            12.202683        8.2365326
V23 16.82993029  1.1753408            17.047150        8.2188411
V37  7.78263091 23.2633562            23.983903        6.9329332
V39  5.93658347 15.1321089            15.261668        6.7965743
V6  13.53449112 13.9008471            17.817121        6.5959996
V33 -1.95218626 10.7370019             9.537110        5.7458003
V49 11.23865809 12.9058813            16.392533        5.7101127
V9  12.17422312 10.6925723            15.335100        4.8531062
V3  -0.99965717  4.6479792             2.544356        4.8184575
V22 20.64382917 13.5319194            21.840305        4.4971592
V13  3.04337254  8.4542167             8.439329        4.3295083
V28  8.35157946  9.1747885            12.046846        4.0080593
V1   4.59894657  2.9643174             5.515818        3.8650018
V48  7.81506023 22.9710623            23.550978        3.3408034
V2   5.72833791  2.3442511             5.929268        3.2911670
V43 -5.82422229 20.6815046            20.494810        3.1038654
V14  4.94511470 12.3747733            12.473057        3.0868077
V26  1.73100845 13.8528397            14.515048        2.9726618
V36 10.32952442  3.0091331            10.868121        2.8287860
V29 -8.35225718 20.8578737            20.790774        2.1621331
V4  12.04202883 -3.7137482             9.595757        2.1247338
V54  3.01606822  3.4472749             4.688521        1.8940500
V51  4.05822087  4.1882703             5.576887        1.8867295
V30  6.15131689  4.7663665             7.989161        1.8382447
V20 10.63531172 -4.0796153            10.407929        1.7490227
V35  4.80503891  7.2249978             8.198459        1.3938299
V41 -4.45008088 12.6752499            12.004367        1.3250610
V31  2.27155363  8.8746544             9.676553        0.9609224
V38 -0.03867955  7.5085284             4.936914        0.9203211
V44  4.48293893  4.1061072             5.898511        0.8958091
V32 -0.30926617  7.4451985             7.643269        0.8302875
V34  3.57763075  5.2352313             5.761358        0.5747138
V15  4.90412430  1.7300436             4.800888        0.5747034
V40  3.28834649 -0.9399261             3.154692        0.4193381
V47  1.60736080  1.9324426             2.829708        0.3910813
'
# Obtenemos que las variables más importantes por importancia en Gini son la V53, V52 y V7, cuyos nombres son:

nombres[c(53,52,7)] # = "char_freq_$"      "char_freq_!"      "word_freq_remove"


# BOSQUES ALEATORIOS


# En lugar de poder utilizar todas las p variables predictoras en cada división, para los bosques aleatorios se recomienda utilizar sólo sqrt(p)

set.seed(1)
spam.rf<-randomForest(V58~.,data=spam,subset=ntrain,
                      ntree=500, mtry=7, importance =TRUE) # Utilizamos mtry = 7 porque sqrt(57) = 7.549834
spam.rf
'
Call:
 randomForest(formula = V58 ~ ., data = spam, ntree = 500, mtry = 7,      importance = TRUE, subset = ntrain) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 7

        OOB estimate of  error rate: 4.99%
Confusion matrix:
     0    1 class.error
0 1787   60  0.03248511
1   93 1125  0.07635468
'

# La estimación OOB del error se ha reducido un poco más: 5.09%
# Veamos qué ocurre en los datos de prueba

spam.rf.pred<-predict(spam.rf,newdata=spam[test,])
mean(spam.rf.pred!=spam[test,58]) # = 0.04817708
table(spam.rf.pred,spam[test,58])
'
spam.rf.pred   0   1
           0 908  41
           1  33 554
'

# El error de clasificación sobre el conjunto de prueba cae hasta el 4.81%

# Las variables más determinantes siguen siendo V53, V52 y V7, pero también es destacable el papel de V16, V21 y V55

varImpPlot(spam.rf)
# Se calcula una medida relativa de importancia para cada variable. importance(spam.rf) devuelve una matriz con varias columnas, en la que concrétamente, la columna 4 se corresponde con MeanDecreaseGini (generalmente). Se divide por el máximo de esa columna para normalizar entre 0 y 1
relimp<-importance(spam.rf)[,4]/max(importance(spam.rf))
sort(relimp) # ordena de menor a mayor
nombres[c(53,52,7,16,21,55)] # = "char_freq_$"  "char_freq_!"   "word_freq_remove"  "word_freq_free"  "word_freq_your"    "capital_run_length_average"


# Elegimos el número de variables mtry con caret. Primero echamos un vistazo con tuneRF


set.seed(1)
bestmtry <- tuneRF(x=spam[ntrain,-58], y=spam[ntrain,58],
              stepFactor=1.5, improve=1e-5, ntree=500)

# Busca automáticamente el valor de mtry que minimiza el error OOB

'
mtry = 7  OOB error = 4.99% 
Searching left ...
mtry = 5 	OOB error = 4.93% 
0.0130719 1e-05 
mtry = 4 	OOB error = 5.15% 
-0.04635762 1e-05 
Searching right ...
mtry = 10 	OOB error = 5.15% 
-0.04635762 1e-05 
'
windows()
print(bestmtry)
'
       mtry   OOBError
4.OOB     4 0.05154976
5.OOB     5 0.04926591
7.OOB     7 0.04991843
10.OOB   10 0.05154976
'
# El mejor valor de mtry es 5, ya que tiene un OOBerror de 0.04926591

set.seed(1)
rfTune<-train(V58~.,data=spam,subset=ntrain,
              method="rf",
              trControl=trainControl(method="cv",number = 10),
              tuneGrid=expand.grid(mtry=c(5,6,7)))
windows()
plot(rfTune)

# Parece que lo mejor es tomar mtry=5
set.seed(1)
spam.rf5<-randomForest(V58~.,data=spam,subset=ntrain,
                      ntree=500, mtry=5, importance =TRUE)
spam.rf5.pred<-predict(spam.rf5,newdata=spam[test,])
mean(spam.rf5.pred!=spam[test,58]) # = 0.04752604
table(spam.rf5.pred,spam[test,58])
'
spam.rf5.pred   0   1
            0 911  43
            1  30 552
'

# El error de prueba queda en 4.75%


# BOOSTING


# Para hacer boosting podemos utilizar las librerías gbm, adabag o C50 gbm: gradient boosting adabag está programada por un equipo de la UCLM, siver para bagging y adaboost C5.0 es la versión más reciente del algoritmo C4.5

library(gbm)

set.seed(1)
gbmTune<-train(V58~.,data=spam[ntrain,],
               method="gbm",distribution="bernoulli",
               trControl=trainControl(method="cv",number = 10),
               tuneLength=5
               )
# Entrena un modelo de Gradient Boosting (GBM) usando caret con validación cruzada
windows()
plot(gbmTune)
# A la vista del gráfico, se recomienda usar un árbol de decisión con al menos 150 árboles y una profundidad de 3 ó 4.

gbmGrid<-expand.grid(n.trees=c(400,600,800),
                     interaction.depth=c(4,5),
                     n.minobsinnode=10,
                     shrinkage=0.1)
# se crean 6 combinaciones: (3 valores de n.trees) × (2 valores de depth) = 6. Estas combinaciones serán evaluadas durante el proceso de validación cruzada con train(...), y se seleccionará aquella que maximice el rendimiento del modelo

set.seed(1)
gbmTune<-train(V58~.,data=spam[ntrain,],
               method="gbm",distribution="bernoulli",
               trControl=trainControl(method="cv",number = 10),
               tuneGrid=gbmGrid
      )

set.seed(1)
spam.gbm.pred<-predict(gbmTune,spam[test,-58])
table(spam.gbm.pred,spam[test,58])
'
spam.gbm.pred   0   1
            0 908  36
            1  33 559
'
mean(spam.gbm.pred!=spam[test,58]) # = 0.04492188
# gbm logra un error de prueba del 4.49%


library(adabag)
set.seed(1)
spam.boost<-boosting(V58~.,data=spam[ntrain,],mfinal=50)
spam.boost.pred<-predict.boosting(spam.boost,newdata=spam[test,])
spam.boost.pred$error # = 0.04752604
spam.boost.pred$confusion
'
               Observed Class
Predicted Class   0   1
              0 903  35
              1  38 560
'

# Se obtiene un error de prueba del 4.75%

library(C50)
set.seed(1)
spam.C50<-C5.0(x = spam[ntrain, -58], y = spam[ntrain,58], trials=100)
spam.C50.pred<-predict(spam.C50,spam[test,-58])
table(spam.C50.pred,spam[test,58])
'
spam.C50.pred   0   1
            0 903  34
            1  38 561
'
mean(spam.C50.pred!=spam[test,58]) # = 0.046875

# Con C5.0 el error de clasificación es el 4.69%


###################################################
## Máquinas de vector soporte
###################################################


# Para utilizar el clasificador de vectores soporte y las máquinas de vectores soporte necesitamos la función svm de la librería e1071

library(e1071)
set.seed(1)

# Vamos a generar un conjunto de datos artificial

x<-matrix(rnorm(20*2),ncol=2)
y<-c(rep(-1,10),rep(1,10))
x[y==1,]<-x[y==1,]+ 1
windows()
plot(x,col=(3-y),pch=19)

# Vemos que las dos clases no son linealmente separables
# Construyamos el clasificador de vectores soporte

datos<-data.frame(x=x, y=as.factor(y))
ajuste.svm<-svm(y~., data=datos, kernel="linear",cost=10,scale=FALSE)
windows()
plot(ajuste.svm,datos)

# Con ajuste.svm$index obtenemos los vectores soporte y con summary algunos datos más

ajuste.svm$index # = 1  2  5  7  14  16  17 (índices del vector soporte del modelo ajustado. El SVM usa solo estos puntos para construir el hiperplano de decisión.)
summary(ajuste.svm)
'
Call:
svm(formula = y ~ ., data = datos, kernel = "linear", cost = 10, scale = FALSE)


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  10 

Number of Support Vectors:  7

 ( 4 3 )


Number of Classes:  2 

Levels: 
 -1 1
'

# ¿Cómo elegir el coste? La función tune utiliza VC

set.seed(1)
params<-tune(svm,y~.,data=datos,kernel="linear",ranges=list(cost=10^(-3:2)))
summary(params)
'
Parameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 cost
  0.1

- best performance: 0.05 

- Detailed performance results:
   cost error dispersion
1 1e-03  0.55  0.4377975
2 1e-02  0.55  0.4377975
3 1e-01  0.05  0.1581139
4 1e+00  0.15  0.2415229
5 1e+01  0.15  0.2415229
6 1e+02  0.15  0.2415229
'
# El menor error se obtiene para cost=0.1 (con un error de 0.05). Podemos almacenar este ajuste.

mejor.modelo<-params$best.model
summary(mejor.modelo)
windows()
plot(mejor.modelo,datos)

# Para predecir generamos un nuevo conjunto de prueba con la misma distribución

xtest<-matrix(rnorm(20*2),ncol =2)
ytest<-sample(c(-1,1),20,replace=TRUE)
xtest[ytest==1,]<-xtest[ytest==1,] + 1
datos.prueba<-data.frame(x=xtest,y=as.factor(ytest))

# Como siempre, utilizamos predict para predecir

ypred<-predict(mejor.modelo,datos.prueba)
mean(ypred!=datos.prueba$y) # = 0.15
table(predichos=ypred,verdaderos=datos.prueba$y)
'
         verdaderos
predichos -1 1
       -1  9 1
       1   2 8
'

# Para las máquinas de vectores soporte, simplemente especificamos un kernel distinto
# Con kernel="polynomial" tenemos que elegir el grado (degree) del polinomio
# Con kernel="radial" tenemos que elegir el parámetro gamma
# Todo lo podemos calibrar mediante validación cruzada

# Generamos datos con una frontera entre clases que sea claramente no lineal

set.seed (1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,150),rep(-1,50))
datos<-data.frame(x=x,y=as.factor(y))
windows()
plot(x,col=3-y,pch=19)

# Dividimos los datos en entrenamiento y prueba y utilizamos kernel="radial" sobre los datos de entrenamiento

ntrain<-sample(200,100)
ajuste.svm<-svm(y~., data=datos[ntrain,], kernel="radial", gamma=1, cost =1)
windows()
plot(ajuste.svm, datos[ntrain,])
summary(ajuste.svm)
'
Call:
svm(formula = y ~ ., data = datos[ntrain, ], kernel = "radial", gamma = 1, cost = 1)


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  radial 
       cost:  1 

Number of Support Vectors:  31

 ( 16 15 )


Number of Classes:  2 

Levels: 
 -1 1
'

# Se ve que la frontera es no lineal, y que tenemos 31 vectores soporte. Elegimos cost y gamma por validación cruzada

set.seed(1)
params<-tune(svm, y~., data=datos[ntrain,],kernel="radial",
             ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                          gamma=c(0.5,1,2,3,4)))
summary(params)
'
Parameter tuning of ‘svm’:

- sampling method: 10-fold cross validation 

- best parameters:
 cost gamma
    1   0.5

- best performance: 0.07 

- Detailed performance results:
    cost gamma error dispersion
1  1e-01   0.5  0.26 0.15776213
2  1e+00   0.5  0.07 0.08232726
3  1e+01   0.5  0.07 0.08232726
4  1e+02   0.5  0.14 0.15055453
5  1e+03   0.5  0.11 0.07378648
6  1e-01   1.0  0.22 0.16193277
7  1e+00   1.0  0.07 0.08232726
8  1e+01   1.0  0.09 0.07378648
9  1e+02   1.0  0.12 0.12292726
10 1e+03   1.0  0.11 0.11005049
11 1e-01   2.0  0.27 0.15670212
12 1e+00   2.0  0.07 0.08232726
13 1e+01   2.0  0.11 0.07378648
14 1e+02   2.0  0.12 0.13165612
15 1e+03   2.0  0.16 0.13498971
16 1e-01   3.0  0.27 0.15670212
17 1e+00   3.0  0.07 0.08232726
18 1e+01   3.0  0.08 0.07888106
19 1e+02   3.0  0.13 0.14181365
20 1e+03   3.0  0.15 0.13540064
21 1e-01   4.0  0.27 0.15670212
22 1e+00   4.0  0.07 0.08232726
23 1e+01   4.0  0.09 0.07378648
24 1e+02   4.0  0.13 0.14181365
25 1e+03   4.0  0.15 0.13540064
'

# Lo mejor es coger cost=1 y gamma=0.5

mejor.modelo<-params$best.model

# Veamos cuál es el error en los datos de prueba

ypred<-predict(mejor.modelo,datos[-ntrain,])
mean(ypred!=datos[-ntrain,]$y) # = 0.12
table(predichos=ypred,verdaderos=datos[-ntrain,]$y)
'
         verdaderos
predichos -1  1
       -1 21 10
       1   2 67
'

##############################################################
