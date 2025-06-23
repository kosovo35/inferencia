### Regresión penalizada

### Ridge y lasso

library(ISLR)
library(glmnet)
bateadores<-na.omit(Hitters)

x<-model.matrix(Salary~.,data=bateadores)[,-1] # genera una matriz de diseño para regresión, que convierte variables dummy las variables categóricas y prepara los datos para regresión. Esto se hace porque glmnet no permite introducir un      data.frame, debe ser una matriz numérica.
y<-bateadores$Salary

# Si escribimos alpha = 0, realiza regresión ridge, mientras que si ponemos alpha = 1, realiza regresión lasso. Glmnet automáticamente estandariza los predictores por defecto

lambdas<-10^seq(10,-2,length =100)

# MODELO DE REGRESIÓN RIDGE:

modelo.ridge<-glmnet(x,y,alpha=0,lambda=lambdas)

# Estamos haciendo regresión ridge para muchos lambdas, que van desde 10^10 (que corresponde con el modelo nulo) hasta   10^(-2) (que está cerca de regresión lineal múltiple)

# Observemos los valores para lambdas[50] y para lambdas[60]:

coef(modelo.ridge)[,50]
'
  (Intercept)         AtBat          Hits         HmRun          Runs           RBI         Walks         Years 
407.356050200   0.036957182   0.138180344   0.524629976   0.230701523   0.239841459   0.289618741   1.107702929 

       CAtBat         CHits        CHmRun         CRuns          CRBI        CWalks       LeagueN     DivisionW 
  0.003131815   0.011653637   0.087545670   0.023379882   0.024138320   0.025015421   0.085028114  -6.215440973 
  
      PutOuts       Assists        Errors    NewLeagueN 
  0.016482577   0.002612988  -0.020502690   0.301433531 
'
sqrt(sum(coef(modelo.ridge)[-1,50]^2)) # = 6.360612

coef(modelo.ridge)[,60]
'
(Intercept)        AtBat         Hits        HmRun         Runs          RBI        Walks        Years       CAtBat 
 54.32519950   0.11211115   0.65622409   1.17980910   0.93769713   0.84718546   1.31987948   2.59640425   0.01083413 
 
       CHits       CHmRun        CRuns         CRBI       CWalks      LeagueN    DivisionW      PutOuts      Assists 
  0.04674557   0.33777318   0.09355528   0.09780402   0.07189612  13.68370191 -54.65877750   0.11852289   0.01606037 
  
      Errors   NewLeagueN 
 -0.70358655   8.61181213  
'
sqrt(sum(coef(modelo.ridge)[-1,60]^2)) # = 57.11001

mejor.lambda<-cv.glmnet(x=x,y=y,nfolds=length(y),alpha=0)$lambda.min # = 25.52821

# Coeficientes del modelo ridge (coeficientes de cada variable + intercept)
coeficientes_ridge <- coef(modelo.ridge)

# COMPARACIÓN ENTRE REGRESIÓN RIDGE Y LINEAL MÚLTIPLE

# Tomamos un conjunto de prueba y de entrenamiento mediante MAS
test<-sample(1:263,65,replace=FALSE); train<- -test

# Calculamos el modelo de regresión ridge para el conjunto de prueba
modelo.ridge<-glmnet(x[train,],y[train],alpha=0,lambda=lambdas)
mejor.lambda<-cv.glmnet(x[train,],y[train],nfolds=263-65,
                        alpha=0)$lambda.min

# Calculamos el error de prueba para regresión ridge
ypred.ridge<-predict(modelo.ridge,s=mejor.lambda,newx=x[test,])
mean((ypred.ridge-y[test])^2) # = 125869.9

# Calculamos el modelo de regresión lineal múltiple para el conjunto de prueba
modelo.lineal<-glmnet(x[train,],y[train],alpha=0,lambda=0)

# Calculamos el error de prueba para regresión lineal múltiple
ypred.lineal<-predict(modelo.lineal,s=0,newx=x[test,],exact=T)
mean((ypred.lineal-y[test])^2) # = 141326.2

# Error_ridge < error_lineal

# Comparamos los gráficos de ajuste
windows()
plot(ypred.ridge,y[test]-ypred.ridge,ylim=c(-1600,600))
windows()
plot(ypred.lineal,y[test]-ypred.lineal,ylim=c(-1600,600))

# MODELO DE REGRESIÓN LASSO:

modelo.lasso<-glmnet(x[train,],y[train],alpha=1,lambda=lambdas)

mejor.lambda<-cv.glmnet(x[train,],y[train],nfolds=263-65,alpha=1)$lambda.min

# Calculamos el error de prueba para el modelo de regresión Lasso
ypred.lasso<-predict(modelo.lasso,s=mejor.lambda,newx=x[test,])
mean((ypred.lasso-y[test])^2) # = 133525.5

# error_ridge < error_lasso < error_lineal

### Validación cruzada

# El conjunto de datos Auto del paquete ISLR contiene observaciones de 9 variables sobre n=392 automóviles. El objetivo es predecir la variable millas por galón (mpg) en función de la potencia (horsepower)

library(ISLR)
windows()
plot(Auto$horsepower,Auto$mpg)

# Conjunto de prueba para estimar el MSE

set.seed(1)
train<-sample(392,196)
test<- -train

# REGRESIÓN LINEAL
modelo1<-lm(mpg~horsepower, data=Auto, subset=train)
abline(modelo1,col="blue",lwd=2)

# Realizamos predicciones:
ypred1<-predict(modelo1,Auto)

# Error cuadrático medio:
mse1<-mean((Auto$mpg-ypred1)[test]^2) # = 23.26601

# REGRESIÓN CUADRÁTICA
modelo2<-lm(mpg~poly(horsepower,2), data=Auto, subset=train)

# Realizamos predicciones:
ypred2<-predict(modelo2,Auto)

# Error cuadrático medio:
mse2<-mean((Auto$mpg-ypred2)[test]^2) # = 18.71646

# Con regresión cuadrática comete menos error que con regresión lineal y es que gráficamente se veía que la relación entre las variables no era de grado 1

xs<-data.frame(horsepower=seq(min(Auto$horsepower),max(Auto$horsepower),length=500)) # valores inventados de la potencia para aplicarlos al modelo 2
y2xs<-predict(modelo2,xs) # predicción 
lines(xs$horsepower,y2xs,lwd=2,col="red")

# REGRESIÓN CÚBICA
modelo3<-lm(mpg~poly(horsepower,3), data=Auto, subset=train)

# Realizamos predicciones:
ypred3<-predict(modelo3,Auto)

# Error cuadrático medio:
mse3<-mean((Auto$mpg-ypred3)[test]^2) # = 18.79401 (comete prácticamente el mismo error que en regresión cuadrática)

# Aplicamos los valores que hemos inventado antes al modelo nuevo
y3xs<-predict(modelo3,xs)
lines(xs$horsepower,y3xs,lwd=2,col="darkgreen")

# Todo indica que la regresión cuadrática es la mejor opción

# Con diferente semilla

set.seed(11)
train<-sample(392,196)
test<- -train

# Regresión lineal:
modelo1<-lm(mpg~horsepower, data=Auto, subset=train)
ypred1<-predict(modelo1,Auto)
mse1<-mean((Auto$mpg-ypred1)[test]^2) # = 23.22909

# Regresión cuadrática:
modelo2<-lm(mpg~poly(horsepower,2), data=Auto, subset=train)
ypred2<-predict(modelo2,Auto)
mse2<-mean((Auto$mpg-ypred2)[test]^2) # = 21.26451

# Regresión cúbica:
modelo3<-lm(mpg~poly(horsepower,3), data=Auto, subset=train)
ypred3<-predict(modelo3,Auto)
mse3<-mean((Auto$mpg-ypred3)[test]^2) # = 21.30421


# Validación cruzada


library(boot)

modelo1<-glm(mpg~horsepower, data=Auto)

# Error cuadrático medio por validación cruzada:
mse.cv<-cv.glm(Auto, modelo1)$delta[1] # = 24.23151

cvs<-rep(0,10)
for(i in 1:10){
  ajuste<-glm(mpg~poly(horsepower,i),data=Auto)
  cvs[i]<-cv.glm(Auto, ajuste)$delta[1]
  }
windows();
plot(1:10,cvs,t="b")  # Aconseja tomar un polinomio de grado 7

# Validación cruzada de 10 partes

set.seed(111)
cvs10<-rep(0,10)
for (i in 1:10){
  ajuste<-glm(mpg~poly(horsepower,i),data=Auto)
  cvs10[i]<-cv.glm(Auto,ajuste,K=10)$delta[1]
  }
windows();
plot(1:10,cvs10,t="b")  # Aconseja tomar un polinomio de grado 9

# EL GRADO DEL POLINOMIO QUE RECOMIENDA VALIDACIÓN CRUZADA SE VE UNA VEZ PINTADO, EL PUNTO QUE MÁS ABAJO ESTÉ


### Selección de variables


library(MASS)

null<-lm(medv~1,data=Boston) # regresión nula
full<-lm(medv~.,data=Boston) # regresión con todas las variables

# El método de selección de variables hacia delante (forward) comienza con el modelo sin predictores y va evaluando las regresiones simples, quedándose con las variables cuya regresión tenga menor RSS, se estudian todos los modelos añadiendo una nueva variable a dicha regresión simple y se fija un criterio de parada.
modelo.step<-step(null, scope=list(lower=null, upper=full),direction="forward")
'
Step:  AIC=1585.76
medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + 
    crim + rad + tax

        Df Sum of Sq   RSS    AIC
<none>               11081 1585.8
+ indus  1   2.51754 11079 1587.7
+ age    1   0.06271 11081 1587.8
'
# El modelo final es medv ~ lstat + rm + ptratio + dis + nox + chas + black + zn + crim + rad + tax (dejamos fuera las variables indus y age)


# Con interacción (dos a dos)


full2<-lm(medv~.^2,data=Boston)
summary(full2)

modelo.step2<-step(null, scope=list(lower=null, upper=full2),direction="forward")
'
Step:  AIC=1143.89
medv ~ lstat + rm + ptratio + dis + nox + chas + crim + rad + 
    tax + age + indus + lstat:rm + rm:ptratio + lstat:ptratio + 
    chas:crim + nox:chas + rm:nox + lstat:nox + dis:nox + lstat:rad + 
    rm:chas + lstat:chas + rm:rad + ptratio:chas + dis:rad + 
    ptratio:dis + lstat:crim + rm:crim + nox:crim + rad:age + 
    nox:age + chas:age + rm:age + lstat:age + lstat:dis + dis:age + 
    ptratio:age + nox:indus + lstat:indus + ptratio:tax + nox:rad + 
    crim:age + ptratio:rad + tax:age + rm:indus + tax:indus + 
    ptratio:indus

               Df Sum of Sq    RSS    AIC
<none>                      4013.7 1143.9
+ tax:lstat     1   13.6252 4000.0 1144.2
+ indus:age     1   13.4059 4000.3 1144.2
+ black         1    8.6270 4005.0 1144.8
+ indus:dis     1    8.0657 4005.6 1144.9
+ indus:rad     1    7.2373 4006.4 1145.0
+ crim:ptratio  1    6.0129 4007.6 1145.1
+ dis:tax       1    5.4158 4008.2 1145.2
+ indus:chas    1    4.7252 4008.9 1145.3
+ rad:tax       1    3.7674 4009.9 1145.4
+ chas:tax      1    3.2134 4010.4 1145.5
+ crim:indus    1    1.4943 4012.2 1145.7
+ zn            1    1.3772 4012.3 1145.7
+ rm:tax        1    0.7386 4012.9 1145.8
+ crim:dis      1    0.5936 4013.1 1145.8
+ crim:tax      1    0.4471 4013.2 1145.8
+ rm:dis        1    0.1861 4013.5 1145.9
+ nox:ptratio   1    0.1795 4013.5 1145.9
+ chas:rad      1    0.1733 4013.5 1145.9
+ nox:tax       1    0.1595 4013.5 1145.9
+ crim:rad      1    0.0480 4013.6 1145.9
+ chas:dis      1    0.0303 4013.6 1145.9
'
# El modelo final es medv ~ lstat + rm + ptratio + dis + nox + chas + crim + rad + tax + age + indus + lstat:rm +        rm:ptratio + lstat:ptratio + chas:crim + nox:chas + rm:nox + lstat:nox + dis:nox + lstat:rad + rm:chas + lstat:chas +    rm:rad + ptratio:chas + dis:rad + ptratio:dis + lstat:crim + rm:crim + nox:crim + rad:age + nox:age + chas:age + rm:age + lstat:age + lstat:dis + dis:age + ptratio:age + nox:indus + lstat:indus + ptratio:tax + nox:rad + crim:age + ptratio:rad + tax:age + rm:indus + tax:indus + ptratio:indus

full3<-lm(medv~.^3,data=Boston)
summary(full3)

modelo.step3<-step(null, scope=list(lower=null, upper=full3),direction="forward")
'
Step:  AIC=993.51
medv ~ lstat + rm + ptratio + dis + nox + chas + crim + rad + 
    tax + age + black + lstat:rm + rm:ptratio + lstat:ptratio + 
    chas:crim + nox:chas + rm:nox + lstat:nox + dis:chas + dis:nox + 
    ptratio:nox + lstat:rad + dis:rad + lstat:crim + rm:rad + 
    rm:crim + nox:crim + rm:chas + rad:age + nox:age + ptratio:crim + 
    ptratio:tax + rm:age + lstat:age + dis:age + lstat:dis + 
    age:black + crim:black + rm:black + dis:crim + crim:rad + 
    crim:tax + tax:age + crim:age + nox:rad + chas:black + nox:tax + 
    ptratio:dis + lstat:rm:nox + lstat:rm:ptratio + rm:ptratio:nox + 
    nox:chas:crim + lstat:ptratio:nox + rm:ptratio:crim + rm:chas:crim + 
    dis:rad:age + lstat:dis:rad + lstat:dis:nox + rm:rad:age + 
    rm:crim:black + dis:nox:crim + lstat:crim:rad + dis:crim:age + 
    lstat:crim:age + ptratio:crim:tax + ptratio:nox:tax + rm:crim:rad + 
    rm:nox:crim

                     Df Sum of Sq    RSS    AIC
<none>                            2744.3 993.51
+ nox:dis:rad         1    9.7620 2734.5 993.71
+ dis:black           1    8.7913 2735.5 993.89
+ black:lstat         1    8.5680 2735.7 993.93
+ crim:dis:rad        1    8.3362 2735.9 993.97
+ nox:rad:lstat       1    8.1709 2736.1 994.00
+ rad:ptratio         1    7.2031 2737.1 994.18
+ nox:rm:age          1    6.8174 2737.4 994.25
+ age:dis:lstat       1    6.2235 2738.1 994.36
+ crim:age:black      1    4.7161 2739.6 994.64
+ crim:nox:age        1    4.6130 2739.7 994.66
+ nox:black           1    4.3964 2739.9 994.70
+ tax:lstat           1    3.8501 2740.4 994.80
+ ptratio:black       1    3.7116 2740.6 994.83
+ crim:rm:lstat       1    2.9912 2741.3 994.96
+ chas:lstat          1    2.8027 2741.5 994.99
+ chas:nox:rm         1    2.6889 2741.6 995.02
+ chas:rad            1    2.2503 2742.0 995.10
+ chas:rm:black       1    1.9269 2742.3 995.16
+ zn                  1    1.8712 2742.4 995.17
+ crim:nox:rad        1    1.7688 2742.5 995.18
+ age:rad:lstat       1    1.7028 2742.6 995.20
+ tax:black           1    1.6527 2742.6 995.21
+ chas:tax            1    1.4668 2742.8 995.24
+ rm:tax              1    1.4595 2742.8 995.24
+ age:ptratio         1    1.3482 2742.9 995.26
+ nox:age:tax         1    1.1918 2743.1 995.29
+ crim:nox:ptratio    1    1.1586 2743.1 995.30
+ crim:ptratio:lstat  1    1.0537 2743.2 995.32
+ crim:nox:tax        1    1.0474 2743.2 995.32
+ nox:age:dis         1    0.9593 2743.3 995.33
+ chas:age            1    0.7237 2743.6 995.38
+ crim:dis:lstat      1    0.7007 2743.6 995.38
+ crim:dis:ptratio    1    0.6904 2743.6 995.38
+ dis:ptratio:lstat   1    0.6650 2743.6 995.39
+ chas:nox:dis        1    0.6198 2743.7 995.40
+ crim:age:tax        1    0.5950 2743.7 995.40
+ nox:age:rad         1    0.4968 2743.8 995.42
+ crim:age:rad        1    0.4821 2743.8 995.42
+ rm:rad:lstat        1    0.4795 2743.8 995.42
+ indus               1    0.4463 2743.8 995.43
+ dis:tax             1    0.3834 2743.9 995.44
+ crim:rm:age         1    0.2345 2744.0 995.47
+ nox:dis:ptratio     1    0.2254 2744.0 995.47
+ nox:age:lstat       1    0.1602 2744.1 995.48
+ nox:rm:rad          1    0.1598 2744.1 995.48
+ chas:ptratio        1    0.1506 2744.1 995.48
+ rm:dis              1    0.1488 2744.1 995.48
+ rad:tax             1    0.0514 2744.2 995.50
+ crim:nox:lstat      1    0.0444 2744.2 995.50
+ rm:age:lstat        1    0.0375 2744.2 995.50
+ rad:black           1    0.0168 2744.2 995.51
+ crim:chas:dis       1    0.0086 2744.3 995.51
+ crim:chas:black     1    0.0060 2744.3 995.51
+ rm:age:black        1    0.0030 2744.3 995.51
'
# El modelo final es medv ~ lstat + rm + ptratio + dis + nox + chas + crim + rad + tax + age + black + lstat:rm +        rm:ptratio + lstat:ptratio + chas:crim + nox:chas + rm:nox + lstat:nox + dis:chas + dis:nox + ptratio:nox + lstat:rad + dis:rad + lstat:crim + rm:rad + rm:crim + nox:crim + rm:chas + rad:age + nox:age + ptratio:crim + ptratio:tax + rm:age + lstat:age + dis:age + lstat:dis + age:black + crim:black + rm:black + dis:crim + crim:rad + crim:tax + tax:age + crim:age + nox:rad + chas:black + nox:tax + ptratio:dis + lstat:rm:nox + lstat:rm:ptratio + rm:ptratio:nox + nox:chas:crim +   lstat:ptratio:nox + rm:ptratio:crim + rm:chas:crim + dis:rad:age + lstat:dis:rad + lstat:dis:nox + rm:rad:age +          rm:crim:black + dis:nox:crim + lstat:crim:rad + dis:crim:age + lstat:crim:age + ptratio:crim:tax + ptratio:nox:tax +     rm:crim:rad + rm:nox:crim

# Por defecto, step utiliza el AIC. Podemos utilizar el BIC si ponemos k = log(n) donde n=nrow(Boston) es el tamaño muestral

modBIC <- step(full, k = log(nrow(Boston)))
'
Step:  AIC=1636.48
medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
    black + lstat

          Df Sum of Sq   RSS    AIC
<none>                 11081 1636.5
- chas     1    227.21 11309 1640.5
- crim     1    245.37 11327 1641.3
- zn       1    257.82 11339 1641.9
- black    1    270.82 11352 1642.5
- tax      1    273.62 11355 1642.6
- rad      1    500.92 11582 1652.6
- nox      1    541.91 11623 1654.4
- ptratio  1   1206.45 12288 1682.5
- dis      1   1448.94 12530 1692.4
- rm       1   1963.66 13045 1712.8
- lstat    1   2723.48 13805 1741.5
'
modAIC <- step(full, k = 2)
'
Step:  AIC=1585.76
medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
    black + lstat

          Df Sum of Sq   RSS    AIC
<none>                 11081 1585.8
- chas     1    227.21 11309 1594.0
- crim     1    245.37 11327 1594.8
- zn       1    257.82 11339 1595.4
- black    1    270.82 11352 1596.0
- tax      1    273.62 11355 1596.1
- rad      1    500.92 11582 1606.1
- nox      1    541.91 11623 1607.9
- ptratio  1   1206.45 12288 1636.0
- dis      1   1448.94 12530 1645.9
- rm       1   1963.66 13045 1666.3
- lstat    1   2723.48 13805 1695.0
'

# Más ejemplos de ridge, lasso y elastic net
# https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net



