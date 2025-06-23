#########################################
### Regresión knn (por k vecinos más próximos)
#########################################

library(FNN)

# Ejemplo de simulación
# Modelo Y=sin(X)+E

# Generamos una muestra aleatoria
set.seed(1)
x<-runif(500,min=0,max=2*pi) # creamos la variable aleatoria x ~ Uniforme
x<-sort(x) # ordena de menor a mayor
e<-rnorm(500,sd=0.2);
y<-sin(x)+e;
windows();plot(x,y)

# Vamos a estimar m en una rejilla de 100 puntos en [0, 2*pi]

xnew<-seq(0,2*pi,length=100)

# Para cada xnew, calculamos sus 5 vecinos más próximos
Kind<-get.knnx(x,xnew,k=5)$nn.index # el $nn.index nos selecciona simplemente los índices de estos vecinos, no los valores de la variable que sería $nn.dist

# Predecimos el valor de y en los nuevos puntos haciendo la media de los valores y correspondientes a los 5-NN
ypred.5nn<-apply(matrix(y[Kind],nrow=100,ncol=5),1,mean)
lines(xnew,sin(xnew),lwd=3)
lines(xnew,ypred.5nn,lwd=3,col=4)

# Comparamos con regresión lineal
abline(lm(y~x),col=2,lwd=2)

# La función knn.reg del paquete FNN lo hace automáticamente

ypred2<-knn.reg(train=matrix(x,ncol=1),test=matrix(xnew,ncol=1),y=y,k=5)$pred

# También está implementado en la función knnreg del paquete caret

library(caret)
datos<-data.frame(x=x,y=y)
fit.5nn<-knnreg(y~x, data=datos, k=5) # creamos el modelo para posteriormente predecir el valor de los nuevos x con k = 5
datos.nuevos<-data.frame(x=xnew)
ypred3<-predict(fit.5nn,datos.nuevos)

# Las 3 formas nos dan los mismos resultados

head(ypred.5nn)
head(ypred2)
head(ypred3)

# El paquete caret, además, contiene herramientas para entrenar varios métodos: caret = Classification And REgression Training

set.seed(1)
knnTune.5cv <- train(y~x, data=datos,
                 method = "knn",
                 preProc = c("center", "scale"), #Pre-escalado
                 metric = "RMSE",
                 tuneGrid = data.frame(k = 1:40),
                 trControl = trainControl(method = "cv", number=5)
                 # 5-fold CV
                 )

### ENTRENA Y AJUSTA UN MODELO k-NN CON VALIDACIÓN CRUZADA DE 5 PLIEGUES Y BUSCA EL MEJOR VALOR DE k

# set.seed(1)
# knnTune.cv <- train(y~x, data=datos, #Tarda mucho, ineficiente
#                      method = "knn",
#                      preProc = c("center", "scale"), #Pre-escalado
#                      metric = "RMSE",
#                      tuneGrid = data.frame(k = 1:40),
#                      trControl = trainControl(method = "loocv")
#                     )

# 5-fold cv sugería usar k=23 vecinos

knnTune.5cv$bestTune # k=23

fit.23nn<-knnreg(y~x, data=datos, k=23)
ypred.23nn<-predict(fit.23nn,datos.nuevos)
lines(xnew,ypred.23nn,col="orange",lwd=3)


# EJEMPLO CON LOS DATOS DE CASAS


library(MASS)
set.seed(1)
knnTune.5cv <- train(medv~lstat, data=Boston,
                     method = "knn",
                     preProc = c("center", "scale"), 
                     metric = "RMSE",
                     tuneGrid = data.frame(k = 1:50),
                     trControl = trainControl(method = "cv", number=5)
                    )
windows()
plot(knnTune.5cv) #El mejor: k=39
knnTune.5cv$bestTune # = 39
medv.fit.knn<-knnreg(medv~lstat, data=Boston, k=39)
lsnew<-data.frame(lstat=1:40)
medv.pred.knn<-predict(medv.fit.knn,lsnew)

windows()
plot(Boston$lstat,Boston$medv)
lines(lsnew$lstat,medv.pred.knn,col=4,lwd=3)



############################################
### Árboles de regresión
############################################

# Se pueden construir con los paquetes rpart y party

library(rpart)

fit.rpart<-rpart(y~x, data=datos)
windows()
plot(fit.rpart)
text(fit.rpart)

# Las reglas del árbol se pueden ver con
fit.rpart

# Las opciones pueden controlarse mediante rpart.control

fit.rpart<-rpart(y~x, data=datos,
                 control = rpart.control(
                   minbucket = 10,  # ninguna hoja del árbol puede tener menos de 10 obs.
                   cp = 0.05,   # cada división del árbol debe reducir el error en al menos un 5%
                   maxdepth = 10  # el árbol no puede tener más de 10 niveles
                 ))

# Para entrenarlo también podemos usar caret

# En función de la profundidad del árbol

set.seed(1)
rpartTune2 <- train(y~x, data=datos,
                   method = "rpart2",
                   metric = "RMSE",
                   tuneGrid = data.frame(maxdepth = 1:20),
                   trControl = trainControl(method = "cv", number = 5))

rpartTune2$bestTune # = 5

# En función del parámetro de complejidad

set.seed(1)
rpartTune1 <- train(y~x, data=datos,
                    method = "rpart",
                    metric = "RMSE",
                    tuneLength = 10,
                    trControl = trainControl(method = "cv", number = 5))

rpartTune2$bestTune # = 5
# El mejor número de hojas es maxdepth=5, por lo tanto, vamos a ajustar dicho árbol de decisión con maxdepth = 5

fit.rpart5<-rpart(y~x, data=datos,control=rpart.control(maxdepth=5))
                  
ypred.rpart<-predict(fit.rpart5, datos.nuevos)

windows();plot(x,y)
lines(xnew, sin(xnew),lwd=3)
lines(xnew, ypred.rpart, col=4, lwd=3, t="s") # recordemos que la función resultante de un árbol de regresión es una función constante a trozos

# Con los datos de casas

rpt <- train(medv~lstat, data=Boston,
                    method = "rpart",
                    metric = "RMSE",
                    tuneLength = 50,
                    trControl = trainControl(method = "cv", number = 5))
cp<-rpt$bestTune # = 0.009027857

medv.fit.rpart<-rpart(medv~lstat, data=Boston,
                      control=rpart.control(cp=cp)) # ajustamos el árbol de decisión con el mejor cp
medv.pred.rpart<-predict(medv.fit.rpart,lsnew) # predecimos los valores de x nuevos

windows()
plot(Boston$lstat,Boston$medv)
lines(lsnew$lstat,medv.pred.rpart,col=4,lwd=3,t="s")


#########################################
### Bagging, randomForest y boosting
#########################################

library(randomForest)

set.seed(1)
fit.bagg<-randomForest(y~x, data=datos,
              mtry=1,  # si pusiésemos mtry = nº de predictores, entonces tendríamos bagging
              importance=TRUE,
              ntree=1000)

# Podemos pintar la estimación OOB del error (out-of-bag)
plot(fit.bagg)

n0<-which.min(fit.bagg$mse) #El menor error se alcanza con 83 árboles
ypred.bagg<-predict(fit.bagg,datos.nuevos,ntree=n0) # realizamos predicciones

windows();plot(x,y)
lines(xnew, sin(xnew),lwd=3)
lines(xnew, ypred.bagg, col=4, lwd=3, t="s")

# Podemos comparar el MSE con respecto al mejor árbol individual

mean((ypred.rpart-sin(xnew))^2)  # = 0.02574667 (ypred.rpart es la predicción del mejor árbol que obtuvimos antes)

mean((ypred.bagg-sin(xnew))^2) # = 0.01449201

# El error del bagging es casi la mitad


# CON LOS DATOS DE CASAS:


# Vamos a entrenar un árbol usando todos los predictores

set.seed(1)
train<-sample(1:506,400,replace=FALSE)  # muestra de entrenamiento
xtrain<-Boston[train,-14] # valores de la muestra de entrenamiento (no seleccionamos la fila 14 porque es la fila correspondiente a la variable medv, que es la variable que queremos predecir)
ytrain<-Boston[train,14] # valores de la variable medv para la muestra de entrenamiento
xtest<-Boston[-train,-14] # valores de x para la muestra de prueba = total - train
ytest<-Boston[-train,14]

rpt <- train(x=xtrain, y=ytrain,
             method = "rpart2",
             metric = "RMSE",
             tuneLength = 20,
             trControl = trainControl(method = "cv", number = 5))
md<-rpt$bestTune # = 9

medv.fit.rpart<-rpart(medv~., data=Boston, subset=train,
                      control=rpart.control(maxdepth=md))  # modelo con el mejor maxdepth
medv.pred.rpart<-predict(medv.fit.rpart, xtest)  # predicciones para el modelo con el mejor maxdepth

# MSE de prueba
MSE.rpart<-mean((ytest-medv.pred.rpart)^2) # = 17.09282

#Ahora con bagging
set.seed(1)
medv.fit.bagg<-randomForest(medv~., data=Boston, subset=train,
                       mtry=13,  # como vemos, ponemos como mtry el mismo número que variables predictoras tenemos
                       importance=TRUE,
                       ntree=1000)
medv.pred.bagg<-predict(medv.fit.bagg, xtest)
# MSE de prueba
MSE.bagg<-mean((ytest-medv.pred.bagg)^2) # = 6.220969 

# Reduce el error de forma notable


# GRÁFICO DE IMPORTANCIA


varImpPlot(medv.fit.bagg)

# Utilizamos ahora bosques aleatorios

# Para regresión, es recomendable tomar mtry=floor(p/3)
# Se puede también entrenar el mejor valor mtry con train (del paquete caret)

set.seed(1)
medv.fit.rf<-randomForest(medv~., data=Boston, subset=train,
                            mtry=4, # en este caso p/3 = 13/3 , tomamos el entero por debajo
                            importance=TRUE,
                            ntree=1000)
medv.pred.rf<-predict(medv.fit.rf, xtest)

# MSE de prueba

MSE.rf<-mean((ytest-medv.pred.rf)^2) # = 9.982705

# gráfico de importancia
varImpPlot(medv.fit.rf)

# Utilizamos ahora boosting

library(gbm)
set.seed(1)
fit.gbm<-gbm(y~x, data=datos, distribution="gaussian", n.trees=1000) # entrena un modelo de Gradient Boosting
best.iter<-gbm.perf(fit.gbm,method="OOB") # = 120
ypred.gbm<-predict(fit.gbm,datos.nuevos,n.trees=best.iter) # predicciones con este modelo

windows();plot(x,y)
lines(xnew, sin(xnew),lwd=3)
lines(xnew, ypred.gbm, col=4, lwd=3)

# Comparamos los MSE
mean((ypred.rpart-sin(xnew))^2) # = 0.02574667
mean((ypred.bagg-sin(xnew))^2) # = 0.01449201
mean((ypred.gbm-sin(xnew))^2) # = 0.007347989

# Tal y como observamos, es con el modelo BOOSTING con el que menor error se comete


# Con los datos de casas


gbmGrid <- expand.grid(n.trees = seq(1000, 5000, by = 1000),
                       interaction.depth = seq(2, 10, by = 2),
                       shrinkage = c(0.001, 0.01, 0.1),
                       n.minobsinnode = 10)


data.train<-Boston[train,]
set.seed(1)
gbmTune <- train(medv~., data= data.train,
             method = "gbm",
             metric = "RMSE",
             tuneGrid = gbmGrid,
             trControl = trainControl(method = "cv", number = 5),
             distribution = "gaussian")

# Es computacionalmente muy intenso

medv.fit.gbm<-gbm(medv~., data= data.train, distribution="gaussian",
                  n.trees=4000, interaction.depth = 6,
                  shrinkage=0.01, cv.folds=5)
best.iter<-gbm.perf(medv.fit.gbm,method="cv")

data.test<-Boston[-train,-14]
medv.pred.gbm<-predict(medv.fit.gbm, data.test, n.trees=best.iter)
MSE.gbm<-mean((ytest-medv.pred.gbm)^2)

# Importancia con gbm
summary(medv.fit.gbm)


#########################################
### Redes neuronales
#########################################

library(nnet)
set.seed(1)
fit.nnet<-nnet(x, y, size=5, decay=0.1, maxit=500, linout=TRUE)  # entrena una red neuronal artificial (ANN) simple
# size: número de neuronas en la capa oculta
# decay: regularización
# maxit: número máximo de iteraciones
# linout=TRUE: se utiliza para regresión, ya que activa una salida lineal (en lugar de softmax o sigmoide para clasificación)

# predecimos con el modelo ajustado
ypred.nnet<-predict(fit.nnet,newdata=matrix(xnew,nc=1))

windows();plot(x,y)
lines(xnew, sin(xnew),lwd=3)
lines(xnew, ypred.nnet, col=4, lwd=3)

# Comparamos los MSE
mean((ypred.rpart-sin(xnew))^2) # = 0.02574667
mean((ypred.bagg-sin(xnew))^2) # = 0.01449201
mean((ypred.gbm-sin(xnew))^2) # = 0.007347989
mean((ypred.nnet-sin(xnew))^2) # = 0.001621713

# Con los datos de casas

nnetGrid <- expand.grid(decay=c(0.01, 0.1, 0.5, 1),
                        size=c(5,10))  # crea los modelos para las combinaciones de decay y size: p. ej: modelo 1: decay = 0.01 y size = 5; modelo 2: decay = 0.01 y size = 5 ...

data.train<-Boston[train,]
set.seed(1)
nnetTune <- train(medv~., data= data.train,
                 method = "nnet",
                 metric = "RMSE",
                 tuneGrid = nnetGrid,
                 trControl = trainControl(method = "cv", number = 5),
                 linout = TRUE,
                 maxit = 10000)

medv.fit.nnet<-nnet(medv~., data=data.train, linout=TRUE,
                  maxit=2000, size=5, decay=0.1)
 
# converge en la iteración 600; tiene una evolución del error de 215097.50 a 2713.91 y tiene 76 parámetros

data.test<-Boston[-train,-14]
medv.pred.nnet<-predict(medv.fit.nnet, data.test)
MSE.nnet<-mean((ytest-medv.pred.nnet)^2) # = 12.82604

