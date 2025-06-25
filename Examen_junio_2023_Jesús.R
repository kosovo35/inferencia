################ EJERCICIO 1 ##################

library(ISLR)
data(College)
set.seed(1)

# Dividamos los datos en muestra de entrenamiento (80%) y de prueba (20%):

n <- length(College$Apps)
m_train <- sample(n,0.8*n,replace=F)
m_test <- setdiff(1:n, m_train)

# APARTADO A)

library(glmnet)

# MODELO REGRESIÓN LINEAL MÚLTIPLE

# Ajustamos el modelo lineal múltiple utilizando los datos de entrenamiento:
x<-model.matrix(Apps~.,data=College)[,-1]
y<-College$Apps
modelo_lineal_multiple<-glmnet(x[m_train,],y[m_train],alpha=0,lambda=0)

# Calculamos el error de prueba para regresión lineal múltiple
ypred.lineal_multiple<-predict(modelo_lineal_multiple,s=0,newx=x[m_test,],exact=T)
mean((ypred.lineal_multiple-y[m_test])^2) # = 869503.2

# MODELO LASSO

# Elegimos el mejor lambda por vc:

mejor.lambda<-cv.glmnet(x[m_train,],y[m_train],nfolds=n-length(m_test),alpha=1,)$lambda.min # = 2.199639

# Ajustamos el modelo con este lambda:

modelo.lasso<-glmnet(x[m_train,],y[m_train],alpha=1,lambda=mejor.lambda)

# Calculamos el error de prueba para el modelo de regresión Lasso
ypred.lasso<-predict(modelo.lasso,s=mejor.lambda,newx=x[m_test,])
mean((ypred.lasso-y[m_test])^2) # = 866488.9

# Los 17 predictores tienen coeficientes no nulos.


# APARTADO B)


library(nnet)
library(caret)
train_data <- College[m_train, ]
# Definimos el control de entrenamiento (validación cruzada en 3 partes):
ctrl <- trainControl(method = "cv", number = 3)
# Definimos la rejilla con 10 combinaciones:
grid <- expand.grid(
  size = c(1, 3, 5, 7, 9),           # tamaño de la capa oculta
  decay = c(0.01, 0.1)               # penalización
)
# Entrenamos la red neuronal con búsqueda en la rejilla y CV
modelo_redes_neuronales <- train(
  Apps ~ .,                    # fórmula de regresión
  data = train_data,           # datos de entrenamiento
  method = "nnet",             # red neuronal
  trControl = ctrl,            # 3-fold CV
  tuneGrid = grid,             # rejilla de parámetros
  linout = TRUE,               # salida lineal para regresión
  trace = FALSE                # no mostrar entrenamiento detallado
)
'
Neural Network 

621 samples
 17 predictor

No pre-processing
Resampling: Cross-Validated (3 fold) 
Summary of sample sizes: 414, 414, 414 
Resampling results across tuning parameters:

  size  decay  RMSE      Rsquared    MAE     
  1     0.01   3984.888         NaN  2576.662
  1     0.10   3598.711  0.52688632  2237.134
  3     0.01   3847.897  0.08285681  2401.199
  3     0.10   2929.627  0.58495419  1885.135
  5     0.01   3312.214  0.27759179  2124.412
  5     0.10   2878.126  0.47674555  1694.260
  7     0.01   3031.834  0.43573632  1893.453
  7     0.10   3023.929  0.42209783  1753.131
  9     0.01   3842.590  0.16657219  2239.971
  9     0.10   2718.341  0.53383163  1586.972

RMSE was used to select the optimal model using the smallest value.
The final values used for the model were size = 9 and decay = 0.1.'
# Concluimos por lo tanto que seleccionamos el modelo con size = 9 y decay = 0.01, por lo tanto, vamos a ajustarlo:

fit.nnet<-nnet(x[m_train,], y[m_train], size=9, decay=0.01, maxit=500, linout=TRUE)
# Calculamos el error de prueba para el modelo con validación cruzada en 3 partes
ypred.cv<-predict(fit.nnet,s=0,newx=x[m_test,])
mean((ypred.cv-y[m_test])^2) # = 866488.9




################ EJERCICIO 2 ##################


data("Carseats")
attach(Carseats); High<-factor(ifelse(Sales <= 8,"No","yes"))
datos <- data.frame(Carseats,High);datos<-datos[,-1]
n<-length(datos$CompPrice)

m_train <- sample(n,0.8*n)
m_test <- setdiff(c(1:n),m_train)

set.seed(2)

# APARTADO A) 

library(tree)
datos$High <- as.factor(datos$High)
arbol_clasificacion<-tree(High~.,data=datos[m_train,])
plot(arbol_clasificacion);text(arbol_clasificacion)

# valores predichos:
pred_train<-predict(arbol_clasificacion,newdata=datos[m_train,],type="class")
mean(pred_train!=datos$High[m_train]) # = 0.075

summary(arbol_clasificacion)
'Classification tree:
tree(formula = High ~ ., data = datos[m_train, ])
Variables actually used in tree construction:
[1] "ShelveLoc"   "Price"       "CompPrice"   "Income"      "Age"         "Advertising" "US"         
Number of terminal nodes:  29 
Residual mean deviance:  0.3757 = 109.3 / 291 
Misclassification error rate: 0.075 = 24 / 320 '
# Tenemos 29 nodos terminales

# Calculemos ahora la proporción de error sobre el conjunto de prueba:
pred_test<-predict(arbol_clasificacion,newdata=datos[m_test,],type="class")
mean(pred_test!=datos$High[m_test]) # = 0.2

# Podamos por cv

arbol.cv<-cv.tree(arbol_clasificacion,FUN=prune.misclass) # usa el número de errores de clasificación como criterio de evaluación
arbol.cv
'$size
 [1] 29 25 23 21 19 15  9  5  4  2  1

$dev
 [1]  90  90  89  98  98 102 103  94  95 100 134

$k
 [1]      -Inf  0.000000  0.500000  1.000000  1.500000  2.500000  2.833333  4.250000  5.000000  8.000000 39.000000

$method
[1] "misclass"

attr(,"class")
[1] "prune"         "tree.sequence"'
# Se realizan 11 podas. El mínimo de errores de clasificación (que es 89) se alcanza para size = 23, lo cual significa que la poda que menor error de clasificación tiene es aquel que tiene 23 ramas, por lo tanto, podemos el árbol con 23 hojas:

arbol_podado <- prune.misclass(arbol_clasificacion,best=9) # podamos el árbol con 9 hojas
plot(arbol_podado);text(arbol_podado)
summary(arbol_podado) # sugiere coger 9 nodos terminales

# Calculemos ahora el error sobre el conjunto de prueba:
pred_test_podado<-predict(arbol_podado,newdata=datos[m_test,],type="class")
mean(pred_test_podado!=datos$High[m_test]) # = 0.2 (se comete el mismo error)

# APARTADO B)

library(randomForest)

spam.rf<-randomForest(High~.,data=datos,subset=m_train,
  ntree=500, mtry=3, importance =TRUE) # Utilizamos mtry = 3 porque sqrt(11) = 3.316625
spam.rf
'Call:
 randomForest(formula = High ~ ., data = datos, ntree = 500, mtry = 3,      importance = TRUE, subset = m_train) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 3

        OOB estimate of  error rate: 18.75%
Confusion matrix:
     No yes class.error
No  165  21   0.1129032
yes  39  95   0.2910448'
# Con este método obtenemos un error OOB de 18.75%

bestmtry <- tuneRF(x=datos[m_train,-11], y=datos[m_train,11],
  stepFactor=1.5, improve=1e-5, ntree=500)

rfTune<-train(High~.,data=datos,subset=m_train,
  method="rf",
  trControl=trainControl(method="cv",number = 10),
  tuneGrid=expand.grid(mtry=c(2,3,4)))
plot(rfTune)
# Lo mejor es tomar mtry=3

# Calculemos ahora el error del bosque aleatorio sobre el conjunto de prueba:
spam.rf<-predict(spam.rf,newdata=datos[m_test,],type="class")
mean(spam.rf!=datos$High[m_test]) # = 0.175 


# APARTADO C)

# Con el árbol de decision cometemos un error de 0.2, mientras que con bosque aleatorio cometo 0.175

# Tabla de confusión del árbol podado
table(pred_test_podado,datos$High[m_test])
'pred_test_podado No yes
             No  45  11
             yes  5  19'
# Precisión = 1 - 0.2 = 0.8

# Tabla de confusión del bosque aleatorio
table(spam.rf,datos$High[m_test])
'spam.rf No yes
    No  45   9
    yes  5  21'
# Precisión = 1 - 0.175 = 0.825
