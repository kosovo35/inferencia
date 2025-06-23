# Cargamos el paquete MASS

library(MASS)

# Miramos las variables de la tabla de datos Boston, y la ayuda

names(Boston)
?Boston

# Regresión lineal simple

modelo<-lm(medv~lstat, data=Boston)

# data = Boston se utiliza para no tener que poner Boston$medv y Boston$lstat. De esa forma sabe que las variables a las que te refieres son las de Boston.

plot(Boston$lstat,Boston$medv)   #diagrama de dispersión; 
abline(modelo,lwd=3,col="blue")  #recta de regresión

summary(modelo)    #devuelve más información

names(modelo)      #para ver qué más cosas contiene

confint(modelo,level=0.95)    #devuelve intervalos de confianza para los coeficientes (0.95 indica que es el intervalo de confianza al 95%)

nuevos<-data.frame(lstat=c(5,10,15))   # puntos donde vamos a predecir el intervalo de confianza
predict(modelo,nuevos,interval="prediction")  # quiero predecir el valor del precio mediano de la vivienda cuando el porcentaje de estatus bajo es de un 5, un 10 y un 15%. En el resultado, fit significa el valor que predice, lwr es el extremo inferior del IC y upr el extremo superior


plot(modelo)


# Regresión lineal múltiple

modelo2<-lm(medv~lstat+age,data=Boston)
summary(modelo2)

modelo3<-lm(medv~.,data=Boston) # haciendo medv~. lo que le estamos diciendo al modelo es que queremos todas las variables predictoras

summary(modelo3)

library(car)
vif(modelo3)  #factores de inflación de la varianza (se consideran altos si son mayores que 5 y muy altos si son mayores que 10)
'
    crim       zn    indus     chas      nox       rm      age      dis      rad      tax  ptratio    black    lstat 
1.792192 2.298758 3.991596 1.073995 4.393720 1.933744 3.100826 3.955945 7.484496 9.008554 1.799084 1.348521 2.941491 
'
cor(Boston)  #matriz de correlaciones. Cuando la correlación entre dos variables es muy alta, lo que se debe hacer es quitar una de las dos variables del modelo.

modelo4<-lm(medv~.-age,data=Boston)  #modelo sin la variable age
summary(modelo4)

# Pintar en 3D (solo se puede pintar en 3D cuando tenemos dos variables predictoras)

# Ajuste lineal
library(rgl); library(car)
scatter3d(medv~lstat+age, data=Boston, fit="linear")


# Un ajuste cualquiera (en este ejemplo, el modelo2)

x<-seq(0,40,length=10); y<-seq(0,100,length=10)  #rejilla de puntos para lstat y age
xy<-expand.grid(x,y)    #rejilla de puntos combinadas de las anteriores
new<-data.frame(lstat=xy[,1],age=xy[,2])   #nuevos puntos
ypred<-predict(modelo2,newdata=new)    #predicciones en los puntos de la rejilla
open3d(); plot3d(x=Boston$lstat, y=Boston$age, z=Boston$medv, type="s",
                 col="yellow", size=1)
persp3d(x=x, y=y, z=matrix(ypred,10,10), add=TRUE,
        col="lightblue")


# Modelo con interacción

modelo5<-lm(medv~lstat*rm,data=Boston)
summary(modelo5)

modelo6<-lm(medv~lstat+rm,data=Boston)
summary(modelo6)

summary(modelo5)$adj.r.squared # = 0.7386937 (modelo con interacción)
summary(modelo6)$adj.r.squared # = 0.6371245 (modelo sin interacción)
# Cuando introducimos interacción mejora el R^2 (con el modelo con interacción explicamos casi el 74% de la variabilidad de medv)

summary(modelo5)$sigma # = 4.701387
summary(modelo6)$sigma # = 5.540257
# Merece la pena meter la interacción porque el error ha bajado significativamente


x<-seq(0,40,length=10); y<-seq(1,10,length=10)
xy<-expand.grid(x,y)
new<-data.frame(lstat=xy[,1],rm=xy[,2])
ypred<-predict(modelo5,newdata=new)
open3d(); plot3d(x=Boston$lstat, y=Boston$age, z=Boston$medv, type="s",
                 col="yellow", size=1)
persp3d(x=x, y=y, z=matrix(ypred,10,10), add=TRUE,
        col="lightblue")

# Modelo cuadrático simple

modelo1<-lm(medv~lstat,data=Boston)
modelo2<-lm(medv~lstat+I(lstat^2),data=Boston)
summary(modelo1)
summary(modelo2) # obtenemos un p-valor del coeficiente de lstat^2 de <2e-16 < 0.05, por lo tanto, significativo y parece que el modelo cuadrático mejora al simple

plot(Boston$lstat,Boston$medv); abline(modelo1,lwd=3)
coef<-coef(modelo2);
x<-seq(min(Boston$lstat),max(Boston$lstat),length=100)
lines(x,coef[1]+coef[2]*x+coef[3]*x^2,col=2,lwd=3)

'
Si queremos contrastar la efectividad del modelo, podemos plantear el siguiente contraste de hipótesis:
H0: los dos modelos son igualmente efectivos
H1: uno de los dos modelos es más efectivo que el otro
'
anova(modelo1,modelo2) # p-valor = 2.2e-16 < 0.05, por lo tanto, resultado significativo (H1) y concluimos que uno de los dos modelos es más efectivo que el otro. Como tenemos que R^2(modelo simple) = 0.5432 y R^2(modelo cuadrático) = 0.6393, por lo tanto, concluimos que el modelo cuadrático es mejor que el simple

### Ridge y lasso

library(ISLR)
library(glmnet)
bateadores<-na.omit(Hitters)

x<-model.matrix(Salary~.,data=bateadores)[,-1]
y<-bateadores$Salary

lambdas<-10^seq(10,-2,length =100)

# La función glmnet aplica regresión ridge si se especifica alpha = 0 y aplica regresión lasso si se especifica alpha = 1 

modelo.ridge<-glmnet(x,y,alpha=0,lambda=lambdas)

coef(modelo.ridge) # devuelve una matriz con los coeficientes ajustados para cada variable + el intercept (20 en total) para cada uno de los posibles valores de lambda (hay 100), luego tendremos una matriz de orden 20x100.

mejor.lambda<-cv.glmnet(x=x,y=y,nfolds=length(y),alpha=0)$lambda.min # = 25.52821


test<-sample(1:263,65,replace=FALSE); train<- -test # test es el conjunto de prueba (que lo tomamos mediante MAS) y el conjunto de entrenamiento será el complementario del de prueba
modelo.ridge<-glmnet(x[train,],y[train],alpha=0,lambda=lambdas)
mejor.lambda<-cv.glmnet(x[train,],y[train],nfolds=263-65,alpha=0)$lambda.min # = 25.46894

# Calculamos el error de prueba para regresión ridge
ypred.ridge<-predict(modelo.ridge,s=mejor.lambda,newx=x[test,])
mean((ypred.ridge-y[test])^2) # = 73465.37

# Calculamos el error de prueba para regresión lineal
modelo.lineal<-glmnet(x[train,],y[train],alpha=0,lambda=0)
ypred.lineal<-predict(modelo.lineal,s=0,newx=x[test,],exact=T)
mean((ypred.lineal-y[test])^2) # = 85189.51

# Se comete bastante más error con regresión lineal que con ridge

# Comparamos los gráficos de ajuste:
windows()
plot(ypred.ridge,y[test]-ypred.ridge,ylim=c(-1600,600))
windows()
plot(ypred.lineal,y[test]-ypred.lineal,ylim=c(-1600,600))

# Regresión lasso:

modelo.lasso<-glmnet(x[train,],y[train],alpha=1,lambda=lambdas)
mejor.lambda<-cv.glmnet(x[train,],y[train],nfolds=263-65,alpha=1)$lambda.min # = 3.213815
ypred.lasso<-predict(modelo.lasso,s=mejor.lambda,newx=x[test,])
mean((ypred.lasso-y[test])^2) # = 71455.54

# Error(lasso) < error(ridge)


