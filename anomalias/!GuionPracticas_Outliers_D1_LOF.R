# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> LOF 
###########################################################################

# Los outliers son respecto a un conjunto de variables.


#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################


# Tanto LOF como clustering usan distancias entre registros, por lo que habr?
# que trabajar sobre los datos previamente normalizados

# Construimos las siguientes variables:

# mis.datos.numericos -> Contendr? las columnas num?ricas de iris, es decir, iris [1:4]
# mis.datos.numericos.normalizados-> Contendr? los datos normalizados
# Asignamos como nombres de filas de mis.datos.numericos.normalizados los mismos nombres de filas que mis.datos.numericos
# Para ello, use rownames tanto a la izquierda como a la derecha de la asignaci?n


# COMPLETAR
mis.datos.numericos<-iris[1:4]
mis.datos.numericos.normalizados<-scale(mis.datos.numericos)
row.names(mis.datos.numericos.normalizados)<-row.names(mis.datos.numericos)

###########################################################################

# Transparencia 99

# Para comprobar que el m?todo de Mahalanobis no es aplicable, 
# obtenga las variables is.MCD.outlier y numero.de.outliers.MCD 
# tal y como se hizo en el script anterior (hay que tener cargada la librer?a mvoutlier)
# Observe que hay un n?mero muy elevado de outliers (50) y adem?s con valores de Petal.Length y Petal.Width
# muy similares. Realmente no son outliers sino que forman un grupo homog?neo.


# COMPLETAR
alpha.value = 0.05
alpha.value.penalizado = 1 - ( 1 - alpha.value) ^ (1 / nrow(mis.datos.numericos))
set.seed(12) 
mvoutlier.plot<-uni.plot(mis.datos.numericos,symb=FALSE,alpha=alpha.value.penalizado)
is.MCD.outlier<-mvoutlier.plot$outliers
numero.de.outliers.MCD<-sum(is.MCD.outlier)


# Ejecute tambi?n lo siguiente:

X11()
corr.plot(mis.datos.numericos[,1], mis.datos.numericos[,3]) 

# El gr?fico nos muestra un gr?fico de dispersi?n al cruzar las variables 1 y 3.
# Vemos que hay dos grupos bien definidos de datos.
# Los puntos que hay entre ellos deber?an ser marcados como outliers
# Usando la distancia de Mahalanobis cl?sica (azul) el elipsoide
# contiene a ambos grupos por lo que los puntos que hubiese entre ellos no ser?an outliers
# Usando la distancia de Mahalanobis construida con la estimaci?n robusta de la matriz de covarianzas
# y las correspondientes medias, el elipsoide (rojo) se construye con el grupo de datos
# m?s numeroso y todos los datos del otro grupo se marcan como outliers :-(

# Tambi?n podemos mostrar un BiPlot llamando a la funci?n MiBiplot sobre mis.datos.numericos
# El gr?fico mostrado es una simplificaci?n ya que ahora estamos mostrando las cuatro variables conjuntamente 
# en un gr?fico 2 dimensional (Transparencia 74)
# Podemos apreciar que hay dos nubes de puntos bien separadas.

# As? pues, el m?todo de detecci?n de outliers usando la distancia de Mahalanobis no es adecuado


MiBiplot(mis.datos.numericos)




###########################################################################
###########################################################################
# DISTANCE BASED OUTLIERS (LOF)
###########################################################################
###########################################################################

# Transparencia 114

numero.de.vecinos.lof = 5

# Establecemos el n?mero de vecinos a considerar numero.de.vecinos.lof = 5 y llamamos a la funci?n lofactor
# pas?ndole como primer par?metro el conjunto de datos normalizados y como par?metro k el valor de numero.de.vecinos.lof
# Esta funci?n devuelve un vector con los scores de LOF de todos los registros
# Lo llamamos lof.scores
# [1] 1.0036218 1.0244637 1.0198058 1.0394019 ......

# Hacemos un plot de los resultados (basta llamar a la funci?n plot sobre lof.scores) 
# para ver los scores obtenidos por LOF.
# Podemos apreciar que hay 4 valores de lof notablemente m?s altos que el resto
# As? pues, establecemos la variable siguiente:
# numero.de.outliers = 4

# Ordenamos los lof.scores y obtenemos los ?ndices de los registros ordenados seg?n el lof.score
# indices.de.lof.outliers.ordenados
# [1]  42 118 132 110 107  16  61  23  ......

# Seleccionamos los 4 primeros y los almacenamos en indices.de.lof.top.outliers
# [1]  42 118 132 110 

# Construimos un vector is.lof.outlier de TRUE/FALSE que nos dice si cada registro de los datos
# originales es o no un outlier. Para ello, debemos usar la funci?n rownames sobre el dataset
# y el operador %in% sobre indices.de.lof.top.outliers
# is.lof.outlier
# [1] FALSE FALSE FALSE FALSE FALSE

# Mostramos un Biplot de los outliers llamando a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)

# Tal vez, el dato m?s interesante sea el 42 ya que no parece que sea un outlier univariante
# (luego lo comprobaremos)



# COMPLETAR
set.seed(12) 
lof.scores<-lofactor(mis.datos.numericos.normalizados,numero.de.vecinos.lof)
plot(lof.scores)
numero.de.outliers = 4
indices.de.lof.outliers.ordenados<-order(lof.scores,decreasing=TRUE)
indices.de.lof.top.outliers<-indices.de.lof.outliers.ordenados[1:4]
is.lof.outlier<-row.names(mis.datos.numericos) %in% indices.de.lof.top.outliers
MiBiPlot_Multivariate_Outliers(mis.datos.numericos,is.lof.outlier,"IRIS")



# Comparamos con los outliers en una sola dimensi?n que habr?amos obtenido con el m?todo IQR 
# Construimos las variables:

# vector.claves.outliers.IQR.en.alguna.columna: Contiene los ?ndices de los que son outliers en alguna columna
#   Hay que llamar a la funci?n vector_claves_outliers_IQR_en_alguna_columna
# vector.es.outlier.IQR.en.alguna.columna: Vector de T/F indicando si cada dato es outlier o no seg?n el criterio IQR
#   Hay que llamar a la funci?n vector_es_outlier_IQR_en_alguna_columna

# Debe salir lo siguiente:
# vector.claves.outliers.IQR.en.alguna.columna
# [1] 16 33 34 61

# Mostramos el Biplot usando el vector de T/F vector.es.outlier.IQR.en.alguna.columna

# Construimos la variable
# indices.de.outliers.multivariantes.LOF.pero.no.1variantes: Contiene los outliers LOF que no son outliers IQR
#   Para ello, usamos setdiff y vemos que el resultado es el mismo conjunto de outliers LOF
#   es decir, que ning?n outlier LOF es outlier IQR

# indices.de.outliers.multivariantes.LOF.pero.no.1variantes
# [1] 16 33 34 61




# COMPLETAR
vector.claves.outliers.IQR.en.alguna.columna<-vector_claves_outliers_IQR_en_alguna_columna(mis.datos.numericos)
vector.es.outlier.IQR.en.alguna.columna<-vector_es_outlier_IQR_en_alguna_columna(mis.datos.numericos)
MiBiPlot_Multivariate_Outliers(mis.datos.numericos,vector.es.outlier.IQR.en.alguna.columna,"IRIS")
indices.de.outliers.multivariantes.LOF.pero.no.1variantes<-setdiff(vector.claves.outliers.IQR.en.alguna.columna,indices.de.lof.top.outliers)

# AMPLIACI?N: Utilice la funci?n is.numeric y sapply para construir autom?ticamente un data frame
# con las columnas num?ricas de otro data frame.


