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

image(zooTrans)
itemFrequencyPlot(zooTrans,support=0.5,cex.names=0.8)

apriZoo<-apriori(zooTrans,parameter = list(support = 0.4, target="frequent"))
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
rules <- apriori(zooNuevo, parameter = list(support = 0.37, confidence = 0.8, minlen = 2))

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
mejores<-limpiar(mejores)
mejores<-mejores[order(mejores@quality$support*mejores@quality$confidence)]
inspect(head(mejores,n=25))


#{eggs=TRUE,fins=FALSE} => {toothed=FALSE}
rulesPruned<- subset(rules, subset = lhs %in% "eggs=TRUE" & lhs %in% "fins=FALSE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#{milk=FALSE,fins=FALSE} => {toothed=FALSE}
rulesPruned<- subset(rules, subset = lhs %in% "milk=FALSE" & lhs %in% "fins=FALSE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned) #{milk=FALSE,fins=FALSE} => {eggs=TRUE}
#{type=mammal} => {milk=TRUE} confianza=1
rulesPruned<- subset(rules, subset = lhs %in% "type=mammal")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#{milk=TRUE} => {type=mammal} confianza=1
rulesPruned<- subset(rules, subset = lhs %in% "milk=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
# View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$toothed==FALSE,])
# View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$toothed==TRUE,])
# View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$type=="reptile",c("milk","fins","toothed","type")])
# View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$type=="amphibian",c("milk","fins","toothed","type")])





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

rulesNegadas <- apriori(zooNegados, parameter = list(support = 0.4, confidence = 0.8, minlen = 2))
#mejor lift y mejor support*confidence
rulesSorted = sort(rulesNegadas, by = "lift")
inspect(head(rulesSorted))
mejoresNegadas<-rulesSorted[rulesSorted@quality$lift>2]
mejoresNegadas<-limpiar(mejoresNegadas)
mejoresNegadas<-mejoresNegadas[order(mejoresNegadas@quality$support*mejoresNegadas@quality$confidence)]
inspect(head(mejoresNegadas,n=10))

#{feathers=FALSE,backbone=TRUE,type_fish=FALSE} => {type_mammal=TRUE}
#{type_bird=FALSE,backbone=TRUE,type_fish=FALSE} => {type_mammal=TRUE}
rulesPruned<- subset(mejoresNegadas, subset = lhs %in% "backbone=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)

rulesPruned<- subset(rulesNegadas, subset = lhs %in% "type_bird=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(head(rulesPruned,n=10))
rulesPruned<- subset(mejoresNegadas, subset = lhs %in% "feathers=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)







rulesPruned<- subset(rules, subset = lhs %in% "legs=has_legs")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#explicar por que no es viable, porcentaje de casos
