###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> Multivariate Normal Distribution --> Mahalanobis
###########################################################################

# Los outliers son respecto a un conjunto de variables.
# Un registro ser? un outlier porque tenga un valor an?malo en alguna variable
# o porque tenga una combinaci?n an?mala de valores.


# Necesita:
# mydata.numeric
# mydata.numeric.scaled

library(mlbench)
data("LetterRecognition")
mydata.numeric = LetterRecognition[1:200,-c(1)]
mydata.numeric.scaled = scale(mydata.numeric)



###########################################################################
# Paquete mvoutlier
###########################################################################

###########################################################################
# Obtenci?n de los outliers multivariantes

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
mvoutlier.plot<-uni.plot(mydata.numeric[1:10],symb=FALSE,alpha=alpha.value.penalizado)
#solo con las 10 primeras variables, uni.plot no permite mas


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
# Para ello, usamos mydata.numeric.scaled y is.MCD.outlier


# COMPLETAR
data.frame.solo.outliers<-mydata.numeric.scaled[is.MCD.outlier,]



# -------------------------------------------------------------------------


# Mostramos los boxplots de forma conjunta con las etiquetas de los outliers
# Para ello llamamos a la funci?n MiBoxPlot_juntos pasando como par?metro is.MCD.outlier
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  




# COMPLETAR
set.seed(12) 
x11()
MiBoxPlot_juntos(mydata.numeric,is.MCD.outlier)


# -------------------------------------------------------------------------

# El BoxPlot conjunto nos informa sobre los valores extremos que hay en cada variable
# Puede apreciarse que casi todos los outliers multivariate corresponden a outliers univariate

# El BiPlot nos muestra tambi?n esta informaci?n, junto con las correlaciones entre variables
# Los puntos mostrados son resultados de proyecciones de n dimensiones a 2, por lo que 
# s?lo es una representaci?n aproximada (mejor cuanto mayor sea la suma de los  porcentajes
# que aparecen como componentes principales PC1 y PC2)
# Llamamos a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)





# COMPLETAR
set.seed(12)
MiBiPlot_Multivariate_Outliers(mydata.numeric,is.MCD.outlier,"LETTERRECOGNITION")



# -------------------------------------------------------------------------


# Vamos a construir una matriz con los gr?ficos de dispersi?n obtenidos al cruzar todas las variables
# Llamamos a la funci?n MiPlot_Univariate_Outliers 
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)


# COMPLETAR
set.seed(12) 
MiPlot_Univariate_Outliers(mydata.numeric,indices.de.outliers.en.alguna.columna,"LETTERRECOGNITION")