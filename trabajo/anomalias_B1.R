# Vamos a trabajar con los siguientes objetos:

# mydata.numeric: frame de datos
# indice.columna: ?ndice de una columna de datos de mydata.numeric
# nombre.mydata:  Nombre del frame para que aparezca en los plots

library(mlbench)
data("LetterRecognition")
mydata.numeric  = LetterRecognition[,-c(1)] 
indice.columna  = 1
nombre.mydata   = "LetterRecognition"

# ------------------------------------------------------------------------

# Ahora creamos los siguientes objetos:

# mydata.numeric.scaled -> Debe contener los valores normalizados demydata.numeric. Para ello, usad la funci?n scale
# columna -> Contendr? la columna de datos correspondiente a indice.columna. Basta realizar una selecci?n con corchetes de mydata.numeric
# nombre.columna -> Debe contener el nombre de la columna. Para ello, aplicamos la funci?n names sobre mydata.numeric
# columna.scaled -> Debe contener los valores normalizados de la anterior


# COMPLETAR
mydata.numeric.scaled<-scale(mydata.numeric,center=TRUE,scale=TRUE)
columna<-mydata.numeric[1:200,indice.columna]
nombre.columna<-names(mydata.numeric[indice.columna])
columna.scaled<-scale(columna)

###########################################################################
###########################################################################
# Parte primera. C?mputo de los outliers IQR
###########################################################################
###########################################################################



###########################################################################
# Calcular los outliers seg?n la regla IQR. Directamente sin funciones propias
###########################################################################

# ------------------------------------------------------------------------------------

# Calculamos las siguientes variables:

# cuartil.primero -> primer cuartil, 
# cuartil.tercero -> tercer cuartil
# iqr             -> distancia IQR

# Para ello, usamos las siguientes funciones:
# quantile(columna, x) para obtener los cuartiles
#    x=0.25 para el primer cuartil, 0.5 para la mediana y 0.75 para el tercero
# IQR para obtener la distancia intercuartil 
#    (o bien reste directamente el cuartil tercero y el primero)

# Calculamos las siguientes variables -los extremos que delimitan los outliers-

# extremo.superior.outlier.normal  = cuartil tercero + 1.5 IQR
# extremo.inferior.outlier.normal  = cuartil primero - 1.5 IQR
# extremo.superior.outlier.extremo = cuartil tercero + 3 IQR
# extremo.inferior.outlier.extremo = cuartil primero - 3 IQR

# Construimos sendos vectores: 

# vector.es.outlier.normal 
# vector.es.outlier.extremo

# Son vectores de valores l?gicos TRUE/FALSE que nos dicen 
# si cada registro es o no un outlier con respecto a la columna fijada
# Para ello, basta comparar con el operador > o el operador < la columna con alguno de los valores extremos anteriores

# COMPLETAR
cuartil.primero<-quantile(columna,0.25)
cuartil.tercero<-quantile(columna,0.75)
iqr<-IQR(columna)
quantile(columna.scaled)

extremo.superior.outlier.normal  <- cuartil.tercero+1.5*iqr
extremo.inferior.outlier.normal  <- cuartil.primero-1.5*iqr
extremo.superior.outlier.extremo <- cuartil.tercero+3*iqr
extremo.inferior.outlier.extremo <- cuartil.primero-3*iqr

vector.es.outlier.normal<-columna>extremo.superior.outlier.normal|columna<extremo.inferior.outlier.normal
vector.es.outlier.extremo<-columna>extremo.superior.outlier.extremo|columna<extremo.inferior.outlier.extremo
###########################################################################
# ?ndices y valores de los outliers
###########################################################################

# Construimos las siguientes variables:

# claves.outliers.normales     -> Vector con las claves (identificador num?rico de fila) de los valores que son outliers. Para obtenerlo, usad which sobre vector.es.outlier.normal
# data.frame.outliers.normales -> data frame obtenido con la selecci?n del data frame original de las filas que son outliers. Puede usarse o bien vector.es.outlier.normal o bien claves.outliers.normales
#                                 Este dataframe contiene los datos de todas las columnas de aquellas filas que son outliers.                                  
# nombres.outliers.normales    -> vector con los nombres de fila de los outliers. Para obtenerlo, usad row.names sobre el data frame anterior
# valores.outliers.normales    -> vector con los datos de los outliers. Se muestra s?lo el valor de la columna que se fij? al inicio del script 
# Idem con los extremos



# COMPLETAR
claves.outliers.normales<-which(vector.es.outlier.normal)
data.frame.outliers.normales<-mydata.numeric[claves.outliers.normales,]
nombres.outliers.normales<-row.names(data.frame.outliers.normales)
valores.outliers.normales<-columna[claves.outliers.normales]
###########################################################################
# Desviaci?n de los outliers con respecto a la media de la columna
###########################################################################

# Construimos la variable:

# valores.normalizados.outliers.normales -> Contiene los valores normalizados de los outliers. 
# Usad columna.scaled y (o bien vector.es.outlier.normal o bien claves.outliers.normales)

# COMPLETAR
valores.normalizados.outliers.normales<-columna.scaled[vector.es.outlier.normal]



###########################################################################
# Plot
###########################################################################

# Mostramos en un plot los valores de los registros (los outliers se muestran en color rojo)
# Para ello, llamamos a la siguiente funci?n:
# MiPlot_Univariate_Outliers (columna de datos, indices -claves num?ricas- de outliers , nombre de columna)
# Lo hacemos con los outliers normales y con los extremos



# COMPLETAR
MiPlot_Univariate_Outliers(columna,claves.outliers.normales,nombre.columna)

###########################################################################
# BoxPlot
###########################################################################


# Vemos el diagrama de caja 

# Para ello, llamar?amos a la funci?n boxplot, pero no muestra el outlier en la columna mpg :-(
# boxplot(columna, xlab=nombre.columna, main=nombre.mydata, las = 1)   # las = 1 all axis labels horizontal, range = 3 for exteme outliers

# Para resolverlo, vemos el diagrama de caja con ggplot geom_boxplot
# Para ello, llamamos a la siguiente funci?n
# MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5)

# Llamamos a la misma funci?n pero con los datos normalizados
# Lo hacemos para resaltar que el Boxplot es el mismo ya que el poder de la normalizaci?n es que no afecta a la posici?n relativa de los datos 

# COMPLETAR
boxplot(columna, xlab=nombre.columna, main=nombre.mydata, las = 1)
MiBoxPlot_IQR_Univariate_Outliers(columna.scaled, indice.columna, coef = 1.5)

###########################################################################
# C?mputo de los outliers IQR con funciones propias
###########################################################################

# En este apartado hacemos lo mismo que antes, pero llamando a funciones que est?n dentro de !Outliers_A3_Funciones.R :

# vector_es_outlier_IQR      -> devuelve un vector TRUE/FALSE
# vector.claves.outliers.IQR -> devuelve los ?ndices de los outliers



# COMPLETAR
vector_es_outlier_IQR(columna.scaled,indice.columna)
vector_claves_outliers_IQR(columna.scaled,indice.columna)


###########################################################################
# Trabajamos con varias columnas simult?neamente

# Los outliers siguen siendo univariate, es decir, con respecto a una ?nica columna
# Pero vamos a aplicar el proceso anterior de forma autom?tica a todas las columnas
# Para ello, llamamos a la funci?n 
# vector_claves_outliers_IQR_en_alguna_columna = function(datos, coef = 1.5)
# y construimos la variable:
# indices.de.outliers.en.alguna.columna 
# Esta variable contiene los ?ndices de aquellos registros que tienen un valor an?malo
# con respecto a alguna columna
# Mostramos los datos normalizados de dichos registros.


# COMPLETAR  
indices.de.outliers.en.alguna.columna<-vector_claves_outliers_IQR_en_alguna_columna(mydata.numeric)
mydata.numeric.scaled[indices.de.outliers.en.alguna.columna,]



###########################################################################
###########################################################################
# AMPLIACI?N

# TODO LO QUE HAY A PARTIR DE AHORA ES DE AMPLIACI?N
# RESU?LVALO S?LO CUANDO TERMINE EL RESTO DE FICHEROS

# Se pide completar el apartado anterior, pero directamente,
# sin llamar a la funci?n vector_claves_outliers_IQR_en_alguna_columna
###########################################################################
###########################################################################



###########################################################################
# ?ndices y valores de los outliers
###########################################################################



# Obtenemos la siguiente matriz:
# frame.es.outlier -> matriz de T/F en la que por cada registro (fila), nos dice si
#                     es un outlier IQR en la columna correspondiente
# Tenemos que aplicar la funci?n vector.es.outlier.IQR sobre cada una de las columnas
# Para ello, usamos sapply:
#   El primer argumento de sapply ser? el rango de las columnas que vamos a barrer, es decir, 1:ncol(mydata.numeric)
#   El segundo argumento de sapply ser? la funci?n a aplicar, es decir, vector_es_outlier_IQR 
#   Consulte la ayuda para obtener m?s informaci?n sobre sapply



# COMPLETAR
frame.es.outlier<-as.matrix(sapply(1:ncol(mydata.numeric),function(x,y){vector_es_outlier_IQR(y,x)},mydata.numeric))



#----------------------------------------------------------------------



# Construyamos la variable:
# numero.total.outliers.por.columna -> N?mero de outliers que hay en cada variable (columna)

# Para ello, usamos apply sobre la dimensi?n 2 (las columnas) y aplicamos la funci?n sum
# Consulte la ayuda para obtener m?s informaci?n sobre apply



# COMPLETAR
numero.total.outliers.por.columna<-apply(frame.es.outlier,2,sum)




#----------------------------------------------------------------------

# Obtenemos la siguiente variable:

# indices.de.outliers.en.alguna.columna -> Contiene los ?ndices de aquellos registros que tengan un valor an?malo en cualquiera de las columnas

# Para ello, usamos sapply como hac?amos anteriormente, pero con la funci?n vector_claves_outliers_IQR
# sapply devuelve una lista con los resultados de applicar la funci?n correspondiente,
# en nuestro caso, vector_claves_outliers_IQR. 
# Como esta funci?n devuelve una lista, al final, tenemos una lista de listas.
# Puede observar que en la columna 6 hay tres outliers, en las filas con ?ndices 15, 15, 17.
# Para "desempaquetar" el resultado, usamos la funci?n unlist
# El resultado lo guardamos en indices.de.outliers.en.alguna.columna


# COMPLETAR
indices.de.outliers.en.alguna.columna<-unlist(sapply(1:ncol(mydata.numeric),function(x,y){vector_claves_outliers_IQR(y,x)},frame.es.outlier))



#----------------------------------------------------------------------








###########################################################################
# Desviaci?n de los outliers con respecto a la media de la columna
###########################################################################


# Mostramos los valores normalizados de los registros que tienen un valor an?malo en cualquier columna 
# Pero mostramos los valores de todas las columnas 
# (no s?lo la columna con respecto a la cual cada registro era un valor an?malo)



# COMPLETAR
mydata.numeric.scaled[indices.de.outliers.en.alguna.columna,]



###########################################################################
# BoxPlot
###########################################################################


# Mostramos los boxplots en un mismo gr?fico.
# Tenemos que usar los datos normalizados, para que as? sean comparables

# Llamamos a la funci?n boxplot


# Llamamos a la funci?n  MiBoxPlot_Juntos
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  
# Esta funci?n normaliza los datos y muestra, de forma conjunta, los diagramas de cajas
# As?, podemos apreciar qu? rango de valores toma cada outlier en las distintas columnas.

# Para etiquetar los outliers en el gr?fico
# llamamos a la funci?n MiBoxPlot_juntos_con_etiquetas 


windows()


# COMPLETAR
MiBoxPlot_juntos_con_etiquetas(mydata.numeric)



