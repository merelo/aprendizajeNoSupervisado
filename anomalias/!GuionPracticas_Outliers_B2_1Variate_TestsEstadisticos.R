# M�ster -> Detecci�n de anomal�as
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> 1-variate Normal Distribution

# Grubbs' test. Normal 1-dim. 1 �nico outlier
# grubbs.test {outliers}

# Rosner's test. Normal 1-dim. <= k outliers.
# rosnerTest {EnvStats}
###########################################################################



###########################################################################
# Conjuntos de datos 
###########################################################################


datos.con.un.outlier           = c(45,56,54,34,32,45,67,45,67,140,65)
datos.con.dos.outliers.masking = c(45,56,54,34,32,45,67,45,67,154,125,65)
datos.con.varios.outliers = c(-0.25,0.68,0.94,1.15,1.20,1.26,1.26,1.34,1.38,1.43,1.49,1.49,1.55,1.56,
                              1.58,1.65,1.69,1.70,1.76,1.77,1.81,
                              1.91,1.94,1.96,1.99,2.06,2.09,2.10,
                              2.14,2.15,2.23,2.24,2.26,2.35,2.37,
                              2.40,2.47,2.54,2.62,2.64,2.90,2.92,
                              2.92,2.93,3.21,3.26,3.30,3.59,3.68,
                              4.30,4.64,5.34,5.42,6.01)

mydata.numeric = datos.con.un.outlier


###########################################################################
# Test de Grubbs
###########################################################################

# Transparencia 78


###########################################################################
# datos.con.un.outlier

# Mostramos el histograma de mydata.numeric usando la funci�n hist
# y un gr�fico de puntos con la funci�n plot
# Observamos que hay un dato con un valor extremo


# COMPLETAR



# -------------------------------------------------------------------------


# Aplicamos el test de Grubbs sobre datos.con.un.outlier
# Usamos la funci�n grubbs.test (two.sided = TRUE)
# Guardamos el resultado en test.de.Grubbs y vemos el p.value correspondiente

# [1] 0.001126431  

# Este resultado es significativo con los valores de alpha usuales 0.025, 0.01



# COMPLETAR



# -------------------------------------------------------------------------

# El test de Grubbs es significativo por lo que se concluye que hay un �NICO outlier
# El valor que toma (140) los podr�amos obtener a trav�s de la funci�n outlier del paquete outliers
# pero �ste no nos dice cu�l es el �ndice correspondiente (10).
# Por lo tanto, calculamos manualmente cu�l es el �ndice de aquel registro
# que m�s se desv�a de la media de la columna correspondiente.
# Tendremos que usar las funciones abs(valor absoluto), mean(media) y order (para ordenar)
# Tenga en cuenta que el resultado de order es un conjunto de �ndices y no de valores originales
# El resultado lo guardamos en las siguientes variables:
# indice.de.outlier.Grubbs
# valor.de.outlier.Grubbs

# [1] 10
# [1] 140



# COMPLETAR



# -------------------------------------------------------------------------

# Ahora que sabemos el �ndice del outlier, podemos usar la funci�n MiPlot_Univariate_Outliers
# Esta funci�n muestra un plot similar al que ya hab�amos mostrado, pero usa el color rojo para mostrar el outlier
# Los par�metros son: el conjunto de datos, los �ndices de los outliers (s�lo uno en este caso) y el t�tulo a mostrar
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)

# Resultado:

# N�mero de datos: 11
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE




# COMPLETAR




###########################################################################
# El mismo proceso anterior empaquetado en una funci�n 
###########################################################################


# Llamamos a la funci�n MiPlot_resultados_TestGrubbs
# MiPlot_resultados_TestGrubbs = function(datos)
# Esta funci�n realiza todo el proceso de aplicar el test de Grubbs tal y como hemos hecho anteriormente
# Tambi�n muestra los resultados: para ello, la funci�n llama directamente a MiPlot_Univariate_Outliers
# El par�metro a pasar a la funci�n MiPlot_resultados_TestGrubbs es el conjunto de datos
# 
# p.value: 0.001126431
# �ndice de outlier: 10
# Valor del outlier: 140
# N�mero de datos: 11
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE




# COMPLETAR




###########################################################################
# Volvemos a aplicar el mismo proceso con los otros conjuntos de datos
###########################################################################

###########################################################################
# datos.con.dos.outliers.masking
###########################################################################


# Transparencia 81


# Mostramos un gr�fico de puntos con la funci�n plot
# Vemos que hay dos outliers

# Aplicamos el test de Grubbs sobre datos.con.dos.outliers.masking

# [1] 0.05614091

# El resultado no es significativo con ninguno de los valores de alpha usuales (<= 0.05)
# Sin embargo, hay dos outliers. (125, 154). 
# La raz�n es que se ha producido un efecto de "masking"  
# Ning�n outlier es detectado por Grubbs :-(



# COMPLETAR





###########################################################################
# Test de Rosner
###########################################################################



# Hay tests para detectar un n�mero exacto de k outliers, pero no son muy �tiles
# Mejor usamos un test para detectar un n�mero menor o igual que k outliers (Rosner)


# Transparencia 83


# Aplicamos el Test de Rosner (rosnerTest) con k=4 sobre datos.con.dos.outliers.masking
# Nos dar� un aviso ocasionado por tener pocos datos
# Guardamos el resultado en test.de.rosner 
# El test ordena los valores de mayor a menor distancia de la media y lanza el test de hip�tesis
# para ver si hay menos de k=4 outliers.

# Imprimimos los siguientes campos:
#   test.de.rosner$all.stats$Outlier 
#     Es un vector de 4 boolean. 
#     Nos indica si son considerados outliers los 4 valores que m�s se alejan de la media
#     En este caso:
#     [1]  TRUE  TRUE FALSE FALSE
#     Los dos primeros son TRUE y el resto FALSE => El test indica que hay dos outliers :-)
#   
#   test.de.rosner$all.stats$Obs.Num
#     Es un vector con los cuatro �ndices de los 4 valores que
#     m�s se alejan de la media
#     En este caso:
#     [1]  10    11   5     4

# Construimos el vector con los �ndices de los que son outliers (10, 11)
# y se lo pasamos como par�metro a la funci�n
# MiPlot_Univariate_Outliers
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){

# N�mero de datos: 12
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE TRUE FALSE



# COMPLETAR




#######################################################################

# La funci�n
# MiPlot_resultados_TestRosner = function(datos)
# hace directamente las anteriores tareas, es decir, lanza el test y dibuja el plot.
# Lanzamos esta funci�n con el dataset datos.con.dos.outliers.masking 
# y comprobamos que ofrece los resultados vistos anteriormente



# COMPLETAR




#######################################################################

# Para ver el comportamiento del Test de Rosner con el conjunto de datos inicial 
# lanzamos la funci�n MiPlot_resultados_TestRosner con k=4 sobre datos.con.un.outlier

# El resultado es el siguiente:
# Test de Rosner
# �ndices de las k-mayores desviaciones de la media: 10 5 4 7
# De las k mayores desviaciones, �Qui�n es outlier? TRUE FALSE FALSE FALSE
# Los �ndices de los outliers son: 10
# Los valores de los outliers son: 140
# N�mero de datos: 11
# �Qui�n es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE

# El test indica que s�lo hay un outlier :-)


# COMPLETAR




#######################################################################
# Lanzamos tambi�n el test de Rosner con k=4 sobre datos.con.varios.outliers

# Mostamos el plot de datos.con.varios.outliers
# Aplicamos el Test de Rosner (rosnerTest) con k=4 sobre datos.con.varios.outliers
# [1]  TRUE  TRUE  TRUE FALSE
# [1] 54 53 52 51
# Indica que hay tres outliers :-)


# COMPLETAR


