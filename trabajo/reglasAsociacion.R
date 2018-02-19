library(mlbench)
library(arules)
library(dummy)

data(Zoo)

zooNuevo<-as.data.frame(sapply(Zoo,as.factor))
rownames(zooNuevo)<-rownames(Zoo)
zooNuevo$legs<-cut(Zoo$legs,breaks = c(-1,0.5,8),labels=c("no_legs","has_legs"))

summary(zooNuevo)
zooTrans<-as(zooNuevo,"transactions")
summary(zooTrans)

image(zooTrans) #no muy representativo
itemFrequencyPlot(zooTrans,support=0.5,cex.names=0.8)

apriZoo<-apriori(zooTrans,parameter = list(support = 0.2, target="frequent"))
apriZoo<-sort(apriZoo,by="support")
inspect(head(apriZoo,n=10))

barplot(table(size(apriZoo)),xlab="itemset size", ylab="count")
inspect(apriZoo[size(apriZoo)==1&apriZoo@quality$support<0.8])

#maximales
maxZoo<-apriZoo[is.maximal(apriZoo)]
inspect(head(sort(maxZoo, by="support")))

#cerrados
cerrZoo<-apriZoo[is.closed(apriZoo)]
inspect(head(sort(cerrZoo,by="support")))

barplot( c(frequent=length(apriZoo), closed=length(cerrZoo),maximal=length(maxZoo)), ylab="count", xlab="itemsets")




#reglas
rules <- apriori(zooNuevo, parameter = list(support = 0.2, confidence = 0.8, minlen = 2))

inspect(head(rules))
summary(rules)

limpiar<-function(rulesSorted){
#limpiar de redundantes
  subsetMatrix <- is.subset(rulesSorted, rulesSorted)
  subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
  redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
  rulesPruned <- rulesSorted[!redundant] # remove redundant rules
  rulesPruned
}

#mejor lift y mejor support*confidence
rulesSorted = sort(rules, by = "lift")
inspect(head(rulesSorted))
mejores<-rulesSorted[rulesSorted@quality$lift>2]
mejores<-mejores[order(mejores@quality$support*mejores@quality$confidence)]
inspect(head(mejores,n=25))


#{eggs=TRUE,fins=FALSE} => {toothed=FALSE} sop: 0.3861386  conf: 0.8478261
rulesPruned<- subset(rules, subset = lhs %in% "airborne=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#{eggs=TRUE,aquatic=FALSE}  => {toothed=FALSE} sop: 0.2574257 conf: 0.8965517
rulesPruned<- subset(rules, subset = lhs %in% "eggs=TRUE" & rhs %in% "toothed=FALSE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)


#{toothed=FALSE} => {eggs=TRUE}
rulesPruned<- subset(rules, subset = lhs %in% "toothed=FALSE" & rhs %in% "eggs=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)

#{hair=FALSE}    => {eggs=TRUE}
rulesPruned<- subset(rules, subset = lhs %in% "hair=FALSE" & rhs %in% "eggs=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)


#{type=mammal} => {milk=TRUE} confianza=1
rulesPruned<- subset(rules, subset = lhs %in% "type=mammal")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#{milk=TRUE} => {type=mammal} confianza=1
rulesPruned<- subset(rules, subset = lhs %in% "milk=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)




##################
# REGLAS NEGADAS #
##################
zooNegados<-cbind(zooNuevo[1:16],dummy(zooNuevo)[33:39])
levels(zooNegados$type_amphibian)<-c(FALSE,TRUE)
levels(zooNegados$type_bird)<-c(FALSE,TRUE)
levels(zooNegados$type_fish)<-c(FALSE,TRUE)
levels(zooNegados$type_insect)<-c(FALSE,TRUE)
levels(zooNegados$type_mammal)<-c(FALSE,TRUE)
levels(zooNegados$type_mollusc.et.al)<-c(FALSE,TRUE)
levels(zooNegados$type_reptile)<-c(FALSE,TRUE)

#como hemos visto previamente, milk=TRUE y mammal=TRUE se cumplen siempre, eliminamos una de las
#dos columnas para evitarnos informacion duplicada
zooNegados<-zooNegados[-4]


rulesNegadas <- apriori(as(zooNegados,"transactions"), parameter = list(support = 0.2, confidence = 0.8, minlen = 2))
#mejor lift y mejor support*confidence
mejoresNegadas<-rulesNegadas
rulesSorted = sort(rulesNegadas, by = "lift")
inspect(head(rulesSorted))
mejoresNegadas<-rulesSorted[rulesSorted@quality$lift>2]
mejoresNegadas<-mejoresNegadas[order(mejoresNegadas@quality$support*mejoresNegadas@quality$confidence)]
inspect(head(mejoresNegadas,n=10))


#{feathers=FALSE, backbone=TRUE, type_amphibian=FALSE, type_fish=FALSE} => {eggs=FALSE} soporte: 0.4059 confianza: 0.89
rulesPruned<- subset(mejoresNegadas, subset = lhs %in% "predator=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(head(rulesPruned,n=20))
