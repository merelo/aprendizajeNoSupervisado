# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS. CLUSTERING OUTLIERS 
###########################################################################

# Los outliers son respecto a un conjunto de variables.


#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################

# Trabajamos sobre las columnas num?ricas de iris [1:4]
# Este conjunto de datos est? disponible en R
# Tanto LOF como clustering usan distancias entre registros, por lo que habr?
# que trabajar sobre los datos previamente normalizados

# Construimos los siguiente conjuntos:

# mis.datos.numericos -> con las columnas 1:4 de iris
# mis.datos.numericos.normalizados -> con los valores normalizados
# a Los rownames de mis.datos.numericos.normalizados les asignamos los rownames de mis.datos.numericos

# Establecemos la variable numero.de.outliers a 5 y numero.de.clusters a 3


mis.datos.numericos   = iris[,1:4]
#mis.datos.numericos   = mis.datos.originales[,sapply(mis.datos.originales, is.numeric)]
mis.datos.numericos.normalizados           = scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)

numero.de.outliers   = 5
numero.de.clusters   = 3

set.seed(2)  # Para establecer la semilla para la primera iteraci?n de kmeans


###########################################################################
# C?mputo de los outliers seg?n la distancia eucl?dea de cada dato 
# al centroide de su cluster
# El centroide podr? ser cualquiera (podr? provenir de un k-means 
# o ser un medoide, por ejemplo)
###########################################################################



###########################################################################
# k-Means

# Construimos el modelo kmeans (modelo.kmeans) con los datos normalizados. 
# Para ello, usamos la funci?n de R llamada "kmeans"

# A partir del resultado de kmeans, accedemos a:

# a) $cluster para obtener 
#   los ?ndices de asignaci?n de cada dato al cluster correspondiente 
#   El resultado lo guardamos en la variable indices.clustering.iris
#   Por ejemplo, si el dato con ?ndice 69 est? asignado al tercer cluster,
#   en el vector indices.clustering.iris habr? un 3 en la componente n?mero 69

# b) $centers para obtener los datos de los centroides.
#   Los datos est?n normalizados por lo que los centroides tambi?n lo est?n.
#   El resultado lo guardamos en la variable centroides.normalizados.iris


# indices.clustering.iris
# 1   2   3   4   ... 69  70  71 ...
# 1   1   1   1   ... 3   3   2  ...

# centroides.normalizados.iris
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -1.01119138  0.85041372   -1.3006301  -1.2507035
# 2   1.13217737  0.08812645    0.9928284   1.0141287
# 3  -0.05005221 -0.88042696    0.3465767   0.2805873



# COMPLETAR
set.seed(2)
result<-kmeans(mis.datos.numericos.normalizados,3)
indices.clustering.iris<-result$cluster
centroides.normalizados.iris<-result$centers

# -------------------------------------------------------------------------

# Calculamos la distancia eucl?dea de cada dato a su centroide (con los valores normalizados)
# Para ello, usad la siguiente funci?n (intente entender c?mo est? implementada)

distancias_a_centroides = function (datos.normalizados, 
                                    indices.asignacion.clustering, 
                                    datos.centroides.normalizados){
  
  sqrt(rowSums(   (datos.normalizados - datos.centroides.normalizados[indices.asignacion.clustering,])^2   ))
}

# dist.centroides.iris
# 1          2          3             ......
# 0.21224719 0.99271979 0.64980753    ......

# Ordenamos dichas distancias a trav?s de la funci?n order y obtenemos
# los ?ndices correspondientes. Nos quedamos con los primeros
# (tantos como diga la variable numero.de.outliers)

# top.outliers.iris
# [1]  42  16 132 118  61



# COMPLETAR
dist.centroides.iris<-distancias_a_centroides(mis.datos.numericos.normalizados,indices.clustering.iris,centroides.normalizados.iris)
top.outliers.iris<-order(dist.centroides.iris,decreasing=TRUE)[1:numero.de.outliers]


###########################################################################
# Creamos la funci?n top_clustering_outliers para realizar las tareas anteriores
# top_clustering_outliers = function(datos.normalizados, 
#                                   indices.asignacion.clustering, 
#                                   datos.centroides.normalizados, 
#                                   numero.de.outliers)
# La funci?n devolver? una lista con dos miembros:
# indices    -> Contiene los ?ndices de los top outliers
# distancias -> Contiene las distancias a los centroides de los anteriores outliers


# Devuelve los ?ndices de los top-k clustering outliers y sus distancias a los centroides



# COMPLETAR
# top_clustering_outliers = function(datos.normalizados, 
#                                   indices.asignacion.clustering, 
#                                   datos.centroides.normalizados, 
#                                   numero.de.outliers)


# Llamamos a la funci?n top_clustering_outliers e imprimimos los ?ndices y las distancias a sus 
# centroides de los outliers



# COMPLETAR





###########################################################################
# Biplot de los outliers

# Creamos un vector is.kmeans.outlier de TRUE/FALSE que nos diga si cada
# registro es o no un outlier.

# is.kmeans.outlier
# [1] FALSE FALSE FALSE FALSE ....

# Para crear el Biplot llamamos a la funci?n MiBiPlot_Clustering_Outliers
# Dentro de esta funci?n se llama a la funci?n ggbiplot, la cual est? basada
# en la funci?n ggplot que tiene un bug de dise?o ya que dentro del par?metro aes
# s?lo se pueden llamar a variables del entorno global y no del entorno local.
# Por tanto, desgraciadamente, debemos establecer variables globales que 
# son usadas dentro de nuestra funci?n MiBiPlot_Clustering_Outliers.
# Dichas variables son BIPLOT.isOutlier, BIPLOT.cluster.colors y BIPLOT.asignaciones.clusters


numero.de.datos   = nrow(mis.datos.numericos)
is.kmeans.outlier = rep(FALSE, numero.de.datos) 
is.kmeans.outlier[top.outliers.kmeans$indices] = TRUE
# is.kmeans.outlier[top.outliers.kmeans.distancia.relativa] = TRUE


BIPLOT.isOutlier             = is.kmeans.outlier
BIPLOT.cluster.colors        = c("blue","red","brown")     # Tantos colores como diga numero.de.clusters
BIPLOT.asignaciones.clusters = indices.clustering.iris
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")



###########################################################################
#
# Los datos de los centroides construidos por el modelo est?n normalizados:
# centroides.normalizados.iris
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -1.01119138  0.85041372   -1.3006301  -1.2507035
# 2   1.13217737  0.08812645    0.9928284   1.0141287
# 3  -0.05005221 -0.88042696    0.3465767   0.2805873

# Queremos revertir la operaci?n z-score para que nos quede as?:
# centroides.valores
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.780851    3.095745     5.510638    1.972340
# 3     5.801887    2.673585     4.369811    1.413208


# Para revertir la operaci?n de normalizaci?n, simplemente tenemos que despejar
# en la f?rmula:
# z-score = (dato - media.columna) / sd.columna
# dato = z-score * sd.columna + media.columna 

# Para aplicar la anterior f?rmula, seguimos los siguientes pasos:

# Construimos un vector mis.datos.medias con las medias de cada columna (usad la funci?n colMeans)
# mis.datos.medias
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.843333     3.057333     3.758000      1.199333

# Construimos un vector mis.datos.desviaciones con las desviaciones t?picas de cada columna.
# Para ello usamos apply con la funci?n sd (standard deviation) 
# mis.datos.desviaciones
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 0.8280661    0.4358663    1.7652982     0.7622377

# Ahora hay que multiplicar cada dato del centroide por la desviaci?n de la correspondiente columna.
# es decir, tenemos que multiplicar centroides.normalizados.iris[i]  por mis.datos.desviaciones[i] 
# Para ello, usamos la funci?n sweep con el operador producto "*", aplic?ndolo sobre las columnas (MARGIN = 2)

# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -0.83733333  0.37066667   -2.2960000  -0.9533333
# 2   0.93751773  0.03841135    1.7526383   0.7730071
# 3  -0.04144654 -0.38374843    0.6118113   0.2138742


# Finalmente, tenemos que sumar a dichos valores la media de la columna correspondiente
# para lo que volvemos a usar sweep con el anterior resultado y mis.datos.medias

# centroides.valores
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.780851    3.095745     5.510638    1.972340
# 3     5.801887    2.673585     4.369811    1.413208




# COMPLETAR





###########################################################################
# AMPLIACI?N
#
# Aplicar clustering con PAM (Partition around medoids)
# Previamente tenemos que calcular la matriz de distancias de todos con todos usando la funci?n dist
# A continuaci?n, usamos la funci?n pam del paquete cluster, pas?ndole como par?metros la matriz
# de distancias y k = n?mero de clusters
# Guardamos el resultado en modelo.pam

# Para obtener las asignaciones de cada dato a su cluster accedemos a modelo.pam$clustering
# ?ndices:             1   2   ... 55  56  57  58
# asignaci?n cluster:  1   1   ...  3   3   2   3

# Para obtener los ?ndices de los medoides accedemos a modelo.pam$medoids
# [1] "8"   "113" "56" 

# Mostramos los valores normalizados y no normalizados de los ?ndices
# Para ello, como los medoides son registros reales de nuestro conjunto de datos
# basta con acceder a sus valores. No tenemos que revertir el proceso de normalizaci?n
# como tuvimos que hacer con los centroides.

# medoides.valores.normalizados
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 8            5.0         3.4          1.5         0.2
# 113          6.8         3.0          5.5         2.1
# 56           5.7         2.8          4.5         1.3

# medoides.valores
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 8     -1.0184372   0.7861738   -1.2791040  -1.3110521
# 113    1.1553023  -0.1315388    0.9868021   1.1816087
# 56    -0.1730941  -0.5903951    0.4203256   0.1320673

# Mostramos los ?ndices de los outliers llamando a la funci?n top_clustering_outliers
# [1]  42  16 132 118  61




# COMPLETAR





###########################################################################


# SI LE HA SOBRADO TIEMPO INTENTE HACER LO QUE VIENE A CONTINUACI?N SIN MIRAR LA SOLUCI?N
# EN CUALQUIER CASO, AL SER ESTA PARTE ALGO M?S COMPLEJA, SE PROPORCIONA LA SOLUCI?N
# PARA QUE PUEDA EJECUTARLO DIRECTAMENTE EN EL CASO DE QUE OPTE POR REALIZAR EL TRABAJO
# CORRESPONDIENTE A LA PARTE DE ANOMAL?AS.



###########################################################################
# AMPLIACI?N

# Transparencia 124

# El objetivo es calcular la distancia de cada punto a su centroide usando la distancia de Mahalanobis.

# Vamos a construir la siguiente funci?n:
# top_clustering_outliers_distancia_mahalanobis = function(datos, 
#                                                          indices.asignacion.clustering, 
#                                                          numero.de.outliers)

# Para hacerlo, tenemos que aislar en un mismo data frame aquellos registros que
# pertenezcan al mismo cluster, obteniendo as? k data frames.
# El data frame -i- tendr? los valores (en todas las variables) de los registros
# que est?n en el cluster -i-
# A cada data frame, le calcularemos la matriz de covarianzas, necesaria
# para calcular las distancias de Mahalanobis de todos los registros
# de ese cluster al centro de su distribuci?n.

# As? pues, realizamos el siguiente proceso:

# Construimos el data frame "seleccion", de forma que 
# seleccion[, i] ser? un vector de T/F indicando qu? registros pertenecen al cluster i.
# seleccion
#     cluster 1   cluster 2   cluster 3
# 1   TRUE        FALSE        FALSE     -> El registro 1 est? en el cluster 1
# 2   FALSE       FALSE        TRUE      -> El registro 2 est? en el cluster 3
# 3   ....

# En el ejemplo del iris, nos quedar?a:
#    [,1]  [,2]  [,3]
# 1   TRUE FALSE FALSE
# 2   TRUE FALSE FALSE
# ......
# 57  FALSE TRUE FALSE
# ......

# As? pues, datos.numericos[seleccion[, i] , ]  son los datos num?ricos de los registros que pertenecen al cluster i
# Basta usar la funci?n cov sobre las anteriores selecciones, para obtener las k matrices de covarianzas
# Guardamos las matrices de covarianzas en una lista lista.matriz.de.covarianzas (usad para ello la funci?n lapply)
# Construimos tambi?n una lista lista.vector.de.medias, de forma que la lista i-?sima contendr?
# las medias de las variables de aquellos registros que est?n en el cluster -i-

# De forma alternativa, podemos usar la funci?n cov.rob del paquete MASS
# Esta funci?n realiza una estimaci?n robusta de la matriz de covarianzas y de la media (Transparencia 94)
# Cunado apliquemos dicha funci?n, accederemos a $cov para obtener la estimaci?n robusta
# de la matriz de covarianzas y a $center para obtener la estimaci?n robusta de la media.

# Ahora, basta obtener las distancias de Mahalanobis. Para ello, usamos la funci?n mahalanobis
# a la que se le pasa como par?metros:
# - El data frame de datos. En nuestro caso ser?n cada uno de los data frames obtenidos a partir de "seleccion"
# - El vector que contiene la medias de las variables. En nuestro caso, ser? la componente correspondiente de lista.vector.de.medias
#   Nota: Para extraer una componente x de una lista L, debe usar L[[x]]
# - La matriz de covarianzas. En nuestro caso, ser? la componente correspondiente de lista.matriz.de.covarianzas
# Construimos la variable mah.distances aplicando lo anterior a los k data frames, usando la funci?n lapply
# mah.distances es una lista de k listas. La lista -i- contiene las distancias de Mahalanobis del cluster -i-

# mah.distances
# [[1]]
# 1          2          3          ......
# 0.8032616  4.2108010  1.2007133  ......
# ......
# ......
# [[3]]
# 54         55         ......
# 2.7151536  4.6704382  ......

# Una vez obtenido mah.distances, ya s?lo nos queda:
# - Unir todas las distancias en una ?nica lista
# - Ordenar las distancias
# - Quedarnos con los top n

# La funci?n devolver? una lista con:
# - Los ?ndices de los top outliers
# - Las distancias de Mahalanobis de dichos outliers

# Llamamos a la funci?n as? construida con los datos de iris y mostramos el Biplot correspondiente.


top_clustering_outliers_distancia_mahalanobis = function(datos, 
                                                         indices.asignacion.clustering, 
                                                         numero.de.outliers){
  
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  seleccion   = sapply(1:k, function(x) indices.asignacion.clustering == x)
  
  
  # Usando medias y covarianzas:
  # lista.matriz.de.covarianzas   = lapply(1:k, function(x) cov(mis.datos.numericos[seleccion[,x],]))
  # lista.vector.de.medias        = lapply(1:k, function(x) colMeans(mis.datos.numericos[seleccion[,x],]))
  
  
  # Usando la estimaci?n robusta de la media y covarianza: (cov.rob del paquete MASS:
  lista.matriz.de.covarianzas   = lapply(1:k, function(x) cov.rob(mis.datos.numericos[seleccion[,x],])$cov)
  lista.vector.de.medias        = lapply(1:k, function(x) cov.rob(mis.datos.numericos[seleccion[,x],])$center)
  
  
  mah.distances   = lapply(1:k, 
                           function(x) mahalanobis(mis.datos.numericos[seleccion[,x],], 
                                                   lista.vector.de.medias[[x]], 
                                                   lista.matriz.de.covarianzas[[x]]))  
  
  todos.juntos = unlist(mah.distances)
  todos.juntos.ordenados = names(todos.juntos[order(todos.juntos, decreasing=TRUE)])
  indices.top.mah.outliers = as.numeric(todos.juntos.ordenados[1:numero.de.outliers])
  
  
  list(distancias = mah.distances[indices.top.mah.outliers]  , indices = indices.top.mah.outliers)
}

top.clustering.outliers.mah = top_clustering_outliers_distancia_mahalanobis(mis.datos.numericos, 
                                                                            indices.clustering.iris, 
                                                                            numero.de.outliers)

numero.de.datos = nrow(mis.datos.numericos)
is.kmeans.outlier.mah = rep(FALSE, numero.de.datos) 
is.kmeans.outlier.mah[top.clustering.outliers.mah$indices] = TRUE

BIPLOT.isOutlier             = is.kmeans.outlier.mah
BIPLOT.cluster.colors        = c("blue","red","brown")     # Tantos colores como diga numero.de.clusters
BIPLOT.asignaciones.clusters = indices.asignacion.clustering.kmeans
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")




###########################################################################
# AMPLIACI?N: 

# Definir la funci?n top_clustering_outliers_distancia_relativa
# Esta funci?n har? lo mismo que la funci?n top_clustering_outliers
# pero usando como criterio la distancia relativa 
# (pag. 123 de las transparencias)

top_clustering_outliers_distancia_relativa = function(datos.normalizados, 
                                                      indices.asignacion.clustering, 
                                                      datos.centroides.normalizados, 
                                                      numero.de.outliers){
  
  dist_centroides = distancias_a_centroides (datos.normalizados, 
                                             indices.asignacion.clustering, 
                                             datos.centroides.normalizados)
  
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  
  distancias.a.centroides.por.cluster    = sapply(1:k , 
                                                  function(x) dist_centroides [indices.asignacion.clustering  == cluster.ids[x]])
  
  distancias.medianas.de.cada.cluster    = sapply(1:k , 
                                                  function(x) median(dist_centroides[[x]]))
  
  todas.las.distancias.medianas.de.cada.cluster  =  distancias.medianas.de.cada.cluster[indices.asignacion.clustering]
  ratios = dist_centroides   /  todas.las.distancias.medianas.de.cada.cluster
  
  indices.top.outliers           = order(ratios, decreasing=T)[1:numero.de.outliers]
  
  list(distancias = ratios[indices.top.outliers]  , indices = indices.top.outliers)
}



top.outliers.kmeans.distancia.relativa = top_clustering_outliers_distancia_relativa(mis.datos.numericos.normalizados, 
                                                                                    indices.clustering.iris, 
                                                                                    centroides.normalizados.iris, 
                                                                                    numero.de.outliers)


cat("?ndices de los top k clustering outliers (k-means, usando distancia relativa)")
top.outliers.kmeans.distancia.relativa$indices 
cat("Distancias a sus centroides de los top k clustering outliers (k-means, usando distancia relativa)")
top.outliers.kmeans.distancia.relativa$distancias
