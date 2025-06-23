# Cargamos el paquete ISLR

library(ISLR)
attach(Smarket)

# La tabla de datos Smarket del paquete ISLR contiene los porcentajes de ganancias en el índice de mercado S&P 500 durante 1250 días (desde 2001 hasta 2005). Para cada día, tenemos registrados:
# * los porcentajes de ganancias de los 5 días previos en las variables Lag1, Lag2, Lag3, Lag4, Lag5
# * el número de acciones cotizadas en el día previo Volume;
# * el porcentaje de ganancias del día en cuestión Today y
# * Direction: si el mercado sube (Up) o si baja (Down)

###############################################
# Creamos el modelo de regresión logística
###############################################

# Variable direction en función del porcentaje de ganancia del día previo:
modelo1<-glm(Direction~Lag1,data=Smarket,family=binomial)
summary(modelo1)
'
Call:
glm(formula = Direction ~ Lag1, family = binomial, data = Smarket)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept)  0.07401    0.05665   1.306    0.191
Lag1        -0.07023    0.05003  -1.404    0.160

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1731.2  on 1249  degrees of freedom
Residual deviance: 1729.2  on 1248  degrees of freedom
AIC: 1733.2

Number of Fisher Scoring iterations: 3
'

# Para ver cómo se codificada la variable respuesta categórica

contrasts(Direction)
'
     Up
Down  0
Up    1
'

# Predecimos para nuevos valores de Lag1

nuevos.valores<-data.frame(Lag1=c(5,10,15))
probs<-predict(modelo1,nuevos.valores,type="response") # = (0.4311542; 0.3478965; 0.2730004) 

# Pintamos la curva de regresión logística

windows()
plot(Lag1,Direction=="Up")
lines(Lag1,predict(modelo1,type="response"))


# Clasificación logística con varios predictores

modelo2<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket,family=binomial)
summary(modelo2)
'
Call:
glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    Volume, family = binomial, data = Smarket)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -0.126000   0.240736  -0.523    0.601
Lag1        -0.073074   0.050167  -1.457    0.145
Lag2        -0.042301   0.050086  -0.845    0.398
Lag3         0.011085   0.049939   0.222    0.824
Lag4         0.009359   0.049974   0.187    0.851
Lag5         0.010313   0.049511   0.208    0.835
Volume       0.135441   0.158360   0.855    0.392

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1731.2  on 1249  degrees of freedom
Residual deviance: 1727.6  on 1243  degrees of freedom
AIC: 1741.6

Number of Fisher Scoring iterations: 3
'

# Matriz de confusión
ypred.log<-ifelse(predict(modelo2,type="response")>0.5,"Up","Down") # si se cumple la condición predict(modelo2,type="response")>0.5, entonces ypred.log = "Up", sino se cumple, entonces ypred.log = "Down"
conf.log<-table(predichos=ypred.log,verdaderos=Direction)
'
             verdaderos
predichos     Down   Up
     Down      145   141
     Up        457   507
'

# Error de entrenamiento (L_test), sensibilidad y especificidad

error.log<-1-sum(diag(conf.log))/sum(conf.log) # = 0.4784
sens.log<-conf.log[2,2]/(conf.log[1,2]+conf.log[2,2]) # = 0.7824074
esp.log<-conf.log[1,1]/(conf.log[1,1]+conf.log[2,1]) # = 0.2408638

# Curva ROC

library(pROC)

mod2.roc<-roc(response=Smarket$Direction,   # response: variable respuesta (Direction)
             predictor=predict(modelo2,type="response"))   # predictor: vector que contiene las predicciones

windows()
plot(mod2.roc, legacy.axes=TRUE, print.thres=0.5, 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad",
     print.auc=TRUE)

# ÁREA DEBAJO DE LA CURVA
auc.log<-mod2.roc$auc # = 0.5387

windows()
plot(mod2.roc, legacy.axes=TRUE, print.thres="best", 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad")


#################################
# Clasificación mediante ADL
#################################


library(MASS)
ajuste1<-lda(Direction~Lag1,data=Smarket) # lda(): análisis discriminante lineal

# Las clases predichas 

predichos<-predict(ajuste1)$class

# Las probabilidades a posteriori

probs<-predict(ajuste1)$posterior  # la primera columna se corresponde con la probabilidad condicionada a que Y = "Up" y la segunda a que Y = "Down"

# LDA utilizando varios predictores

ajuste2<-lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket)

# Construimos la matriz de confusión

ypred.lda<-predict(ajuste2)$class
conf.lda<-table(predichos=ypred.lda,verdaderos=Direction)
'
         verdaderos
predichos Down  Up
     Down  145 141
     Up    457 507
'

# Error de entrenamiento, sensibilidad y especificidad

error.lda<-1-sum(diag(conf.lda))/sum(conf.lda) # = 0.4784
sens.lda<-conf.lda[2,2]/(conf.lda[1,2]+conf.lda[2,2]) # = 0.7824074
esp.lda<-conf.lda[1,1]/(conf.lda[1,1]+conf.lda[2,1]) # = 0.2408638

# Curva ROC

aj2.roc<-roc(response=Smarket$Direction, 
              predictor=predict(ajuste2)$posterior[,2])

windows()
plot(aj2.roc, legacy.axes=TRUE, print.thres=0.5, 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad",
     print.auc=TRUE)

auc.lda<-aj2.roc$auc # = 0.5387

windows()
plot(mod2.roc, legacy.axes=TRUE, print.thres="best", 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad")


#################################
# Clasificación mediante ADC (análisis discriminante cuadrático)
#################################

# ADC utilizando varios predictores

ajuste2.qda<-qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket)

# Construimos la matriz de confusión

ypred.qda<-predict(ajuste2.qda)$class
conf.qda<-table(predichos=ypred.qda,verdaderos=Direction)
'
         verdaderos
predichos Down  Up
     Down  181 136
     Up    421 512
'

# Error de entrenamiento, sensibilidad y especificidad

error.qda<-1-sum(diag(conf.qda))/sum(conf.qda) # = 0.4456
sens.qda<-conf.qda[2,2]/(conf.qda[1,2]+conf.qda[2,2]) # = 0.7901235
esp.qda<-conf.qda[1,1]/(conf.qda[1,1]+conf.qda[2,1]) # = 0.3006645

# Curva ROC

qda.roc<-roc(response=Smarket$Direction, 
             predictor=predict(ajuste2.qda)$posterior[,2])

windows()
plot(qda.roc, legacy.axes=TRUE, print.thres=0.5, 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad",
     print.auc=TRUE)

auc.qda<-qda.roc$auc # = 0.5672

windows()
plot(qda.roc, legacy.axes=TRUE, print.thres="best", 
     col=4, lwd=3, identity.lwd=2, 
     print.thres.pattern="%.3f (e=%.3f, s=%.3f)",
     xlab="1 - Especificidad", ylab="Sensibilidad")


# Analizamos ahora los datos iris
# Parece que las variables son normales y con la misma varianza

windows()
pairs(iris[,1:4],col=as.numeric(iris$Species)+2)

library(nnet)

iris.log<-multinom(Species~.,data=iris,maxit=1000) # ajusta un modelo de regresión logística multinomial, es decir, para clasificar cuando la variable dependiente tiene más de dos clases
'
# weights:  18 (10 variable)     INDICA QUE ESTIMA 18 PARÁMETROS DE LOS CUALES 10 ESTÁN ASOCIADOS A LA VARIABLE PREDICTORA
initial  value 164.791843 
iter  10 value 16.177348
iter  20 value 7.111438
iter  30 value 6.182999
iter  40 value 5.984028
iter  50 value 5.961278
iter  60 value 5.954900
iter  70 value 5.951851
iter  80 value 5.950343
iter  90 value 5.949904
iter 100 value 5.949867
iter 110 value 5.949850
iter 120 value 5.949821
iter 130 value 5.949767
iter 140 value 5.949743
iter 150 value 5.949722
iter 160 value 5.949686
iter 170 value 5.949424
iter 180 value 5.949393
final  value 5.949363 
converged

El algoritmo comienza con una función de pérdida de 164.791843 y finaliza con 5.949363 (minimizando la función de costes)
'
iris.lda<-lda(Species~.,data=iris)
'
Call:
lda(Species ~ ., data = iris)

Prior probabilities of groups:
    setosa versicolor  virginica 
 0.3333333  0.3333333  0.3333333 

Group means:
           Sepal.Length Sepal.Width Petal.Length Petal.Width
setosa            5.006       3.428        1.462       0.246
versicolor        5.936       2.770        4.260       1.326
virginica         6.588       2.974        5.552       2.026

Coefficients of linear discriminants:
                    LD1         LD2
Sepal.Length  0.8293776 -0.02410215
Sepal.Width   1.5344731 -2.16452123
Petal.Length -2.2012117  0.93192121
Petal.Width  -2.8104603 -2.83918785

Proportion of trace:
   LD1    LD2 
0.9912 0.0088
'
iris.qda<-qda(Species~.,data=iris)
'
Call:
qda(Species ~ ., data = iris)

Prior probabilities of groups:
    setosa versicolor  virginica 
 0.3333333  0.3333333  0.3333333 

Group means:
           Sepal.Length Sepal.Width Petal.Length Petal.Width
setosa            5.006       3.428        1.462       0.246
versicolor        5.936       2.770        4.260       1.326
virginica         6.588       2.974        5.552       2.026
'

ypred.log<-predict(iris.log)
ypred.lda<-predict(iris.lda)$class
ypred.qda<-predict(iris.qda)$class

conf.log<-table(predichos=ypred.log,verdaderos=iris$Species)
'
            verdaderos
predichos    setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          1        49
'
conf.lda<-table(predichos=ypred.lda,verdaderos=iris$Species)
'
            verdaderos
predichos    setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         1
  virginica       0          2        49
'
conf.qda<-table(predichos=ypred.qda,verdaderos=iris$Species)
'
            verdaderos
predichos    setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         1
  virginica       0          2        49
'

error.log<-mean(ypred.log!=iris$Species) # = 0.01333333
error.lda<-mean(ypred.lda!=iris$Species) # = 0.02
error.qda<-mean(ypred.qda!=iris$Species) # = 0.02

# El modelo de regresión logística multinomial es el que menos error comete

