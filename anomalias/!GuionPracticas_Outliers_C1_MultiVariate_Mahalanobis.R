# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> Multivariate Normal Distribution --> Mahalanobis
###########################################################################

# Los outliers son respecto a un conjunto de variables.
# Un registro ser? un outlier porque tenga un valor an?malo en alguna variable
# o porque tenga una combinaci?n an?mala de valores.


# Necesita:
# mydata.numeric
# mydata.numeric.scaled

# Trabajamos sobre mtcars[,-c(8:11)]

mydata.numeric = mtcars[,-c(8:11)]
mydata.numeric.scaled = scale(mydata.numeric)



###########################################################################
# Paquete mvoutlier
###########################################################################

###########################################################################
# Obtenci?n de los outliers multivariantes

# Transparencia 93

#
# Calcula los outliers calculando las distancias de Mahalanobis y usando la aproximaci?n de la Chi cuadrado
# La estimaci?n de la matriz de covarianzas es la estimaci?n robusta seg?n MCD
# No hay que normalizar los datos ya que la distancia de Mahalanobis est?
# dise?ada, precisamente para evitar el problema de la escala.

# uni.plot genera el gr?fico similar a MiPlot_Univariate_Outliers con todas las columnas
# Adem?s, devuelve en $outliers los ?ndices de los outliers

# Establecemos los valores de significaci?n
# alpha.value.penalizado es para tener en cuenta el error FWER

alpha.value = 0.05
alpha.value.penalizado = 1 - ( 1 - alpha.value) ^ (1 / nrow(mydata.numeric))       # Transparencia 92

# Establecemos la semilla para el m?todo iterativo que calcula MCD 
# IMPORTANTE: Para que el resultado sea el mismo en todas las ejecuciones, siempre hay que
# establecer la semilla antes de lanzar la funci?n correspondiente.
set.seed(12)  

# Llamamos a uni.plot del paquete mvoutlier con symb=FALSE, alpha = alpha.value.penalizado
# Guardamos el resultado en la variable mvoutlier.plot
# Esta funci?n calcula los outliers MULTIVARIANTES seg?n la distancia de Mahalanobis
# considerando la estimaci?n robusta de la matriz de covarianzas -MCD- y la estimaci?n robusta de la media de cada variable.
# Tambi?n imprime un plot 1-dimensional para ver los valores que toman los outliers en cada atributo
# pero el plot no imprime las etiquetas de los outliers 

# Nota: Es posible que haya que instalar el paquete pcaPP para que se pueda ejecutar uni.plot

X11()



# COMPLETAR
set.seed(12) 
mvoutlier.plot<-uni.plot(mydata.numeric,symb=FALSE,alpha=alpha.value.penalizado)



###########################################################################
# An?lisis de los outliers

# Vamos a ver las variables que m?s influyen en la designaci?n de los outliers
# a) Viendo el valor normalizado sobre cada variable para ver cu?nto se desv?a de la media
#     Pero esto no es suficiente ya que no es f?cil apreciar interacciones entre atributos
# b) Gr?ficamente, con un biplot sobre las componentes principales
#     El Biplot permite ver las dimensiones importantes que influyen en la designaci?n de los outliers


# Construimos las variables 
# is.MCD.outlier 
# numero.de.outliers.MCD
# que ser? un vector TRUE/FALSE que nos dice si cada dato es o no un outlier 
# Para ello, accedemos a mvoutlier.plot$outliers
# Contamos el n?mero total de outliers y lo guardamos en la variable numero.de.outliers.MCD
# Debe salir lo siguiente:

# is.MCD.outlier 
# Mazda RX4       Mazda RX4 Wag          Datsun 710   ......
# FALSE               FALSE               FALSE       ......
# ......  

# numero.de.outliers.MCD
# [1] 10



# COMPLETAR
is.MCD.outlier<-mvoutlier.plot$outliers
numero.de.outliers.MCD<-sum(is.MCD.outlier)


# Veamos qu? outliers son multivariantes "puros", es decir, que NO son 1 variantes
# con respecto a ninguna columna. Estos outliers multivariantes son interesantes
# ya que nos indican que no son outliers porque una de sus columnas tenga un valor
# extremo, sino porque hay alguna combinaci?n an?mala de valores de columnas.
# Por tanto, debemos construir las siguientes variables:

# indices.de.outliers.en.alguna.columna -> A trav?s de la funci?n vector_claves_outliers_IQR_en_alguna_columna 
# indices.de.outliers.multivariantes.MCD -> A partir de la variable is.MCD.outlier calculada anteriormente,


# indices.de.outliers.multivariantes.MCD.pero.no.1variantes   (debe usar setdiff sobre las anteriores variables)
# nombres.de.outliers.multivariantes.MCD.pero.no.1variantes   (debe usar rownames)

# Debe salir lo siguiente:

# indices.de.outliers.multivariantes.MCD
# Merc 230  Cadillac Fleetwood Lincoln Continental   Chrysler Imperial            Fiat 128 
# 9                  15                  16                  17                  18 
# Honda Civic      Toyota Corolla        Lotus Europa        Ferrari Dino       Maserati Bora 
# 19                  20                  28                  30                  31 

# indices.de.outliers.multivariantes.MCD.pero.no.1variantes
# [1] 18 19 28 30

# nombres.de.outliers.multivariantes.MCD.pero.no.1variantes
# [1] "Fiat 128"     "Honda Civic"  "Lotus Europa" "Ferrari Dino"




# COMPLETAR
indices.de.outliers.en.alguna.columna<-vector_claves_outliers_IQR_en_alguna_columna(mydata.numeric)
indices.de.outliers.multivariantes.MCD<-which(is.MCD.outlier)

indices.de.outliers.multivariantes.MCD.pero.no.1variantes<-setdiff(indices.de.outliers.multivariantes.MCD,indices.de.outliers.en.alguna.columna)
nombres.de.outliers.multivariantes.MCD.pero.no.1variantes<-names(is.MCD.outlier[indices.de.outliers.multivariantes.MCD.pero.no.1variantes])

# -------------------------------------------------------------------------


# ?Cu?l es el valor normalizado de cada outlier, es decir, ?Cu?nto se desv?a de la media de cada columna?
# Esta desviaci?n ya se ha mostrado antes al llamar a uni.plot, pero s?lo se muestran los outliers como puntos rojos
# Al no tener las etiquetas, no sabemos cu?les son los valores de los outliers en cada columna.

# Construimos una tabla num?rica data.frame.solo.outliers que muestre los valores normalizados de los outliers en todas las columnas 
# Para ello, usamos mydata.numeric.scaled y is.MCD.outlier:

# mpg        cyl       disp         hp        drat          wt        qsec
# Merc 230             0.44954345 -1.2248578 -0.7255351 -0.7538702  0.60491932 -0.06873063  2.82675459
# Cadillac Fleetwood  -1.60788262  1.0148821  1.9467538  0.8504968 -1.24665983  2.07750476  0.07344945
# Lincoln Continental -1.60788262  1.0148821  1.8499318  0.9963483 -1.11574009  2.25533570 -0.01608893
# Chrysler Imperial   -0.89442035  1.0148821  1.6885616  1.2151256 -0.68557523  2.17459637 -0.23993487
# Fiat 128             2.04238943 -1.2248578 -1.2265893 -1.1768396  0.90416444 -1.03964665  0.90727560
# Honda Civic          1.71054652 -1.2248578 -1.2507948 -1.3810318  2.49390411 -1.63752651  0.37564148
# Toyota Corolla       2.29127162 -1.2248578 -1.2879099 -1.1914248  1.16600392 -1.41268280  1.14790999
# Lotus Europa         1.71054652 -1.2248578 -1.0942658 -0.4913374  0.32437703 -1.74177223 -0.53093460
# Ferrari Dino        -0.06481307 -0.1049878 -0.6916474  0.4129422  0.04383473 -0.45709704 -1.31439542
# Maserati Bora       -0.84464392  1.0148821  0.5670394  2.7465668 -0.10578782  0.36051645 -1.81804880




# COMPLETAR
data.frame.solo.outliers<-mydata.numeric.scaled[is.MCD.outlier,]



# -------------------------------------------------------------------------


# Mostramos los boxplots de forma conjunta con las etiquetas de los outliers
# Para ello llamamos a la funci?n MiBoxPlot_juntos pasando como par?metro is.MCD.outlier
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  




# COMPLETAR
set.seed(12) 
MiBoxPlot_juntos(mydata.numeric,is.MCD.outlier)


# -------------------------------------------------------------------------

# Transparencia 74  (Biplot)


# El BoxPlot conjunto nos informa sobre los valores extremos que hay en cada variable
# Puede apreciarse que casi todos los outliers multivariate corresponden a outliers univariate
# Las ?nicas excepciones son Fiat 128 y Ferrari Dino, aunque Fiat 128 es casi un outlier en mpg

# El BiPlot nos muestra tambi?n esta informaci?n, junto con las correlaciones entre variables
# Los puntos mostrados son resultados de proyecciones de n dimensiones a 2, por lo que 
# s?lo es una representaci?n aproximada (mejor cuanto mayor sea la suma de los  porcentajes
# que aparecen como componentes principales PC1 y PC2)
# Llamamos a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)





# COMPLETAR
set.seed(12) 
MiBiPlot_Multivariate_Outliers(mydata.numeric,is.MCD.outlier,"MTCARS")



# -------------------------------------------------------------------------


# El BiPlot muestra claramente que Ferrari Dino no es outlier univariate en ninguna variable
# (no est? en el extremo delimitado por los vectores correspondientes a las variables)
# Posiblemente sea un outlier multivariate debido a la combinaci?n anormal de varias variables.

# Vamos a construir una matriz con los gr?ficos de dispersi?n obtenidos al cruzar todas las variables
# Y vamos a destacar en rojo el dato correspondiente a Ferrari Dino.
# Para ello, obtenemos el ?ndice de Ferrari Dino usando las funciones which y rownames
# y llamamos a la funci?n MiPlot_Univariate_Outliers 
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)
# El par?metro indices_de_Outliers ?nicamente contendr? el ?ndice del Ferrari Dino.
# Puede apreciarse que no hay una combinaci?n clara de 2 variables que hagan del Ferrari un outlier.
# Es posible que intervengan m?s de dos variables.
# Efectivamente, si observamos la tabla data.frame.solo.outliers
# parece ser que consigue una aceleraci?n qsec muy buena -1.3 (bastante cercana a la mayor -> Maserati Bora -1.8)
# con una potencia hp normal 0.4 (Maserati 2.7). Tener un peso wt ligero -0.4 seguramente es un factor decisivo (Maserati 0.3)
# La combinaci?n peso, aceleraci?n, hp es lo que hace de Ferrari Dino un outlier multivariate.




# COMPLETAR
set.seed(12) 
MiPlot_Univariate_Outliers(mydata.numeric,is.MCD.outlier,"MTCARS")







