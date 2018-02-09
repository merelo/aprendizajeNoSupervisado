library(mlbench)
library(arules)

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
View(inspect(head(mejores,n=50)))
#{milk=FALSE,fins=FALSE} => {toothed=FALSE}
rulesPruned<- subset(rules, subset = lhs %in% "milk=FALSE" & lhs %in% "fins=FALSE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$toothed==FALSE,])
View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$toothed==TRUE,])
View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$type=="reptile",c("milk","fins","toothed","type")])
View(zooNuevo[zooNuevo$milk==FALSE&zooNuevo$fins==FALSE&zooNuevo$type=="amphibian",c("milk","fins","toothed","type")])













rulesPruned<- subset(rules, subset = lhs %in% "type=mammal")
rulesPruned<- subset(rules, subset = lhs %in% "milk=TRUE")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#type=mammal, milk=TRUE tienen las mismas reglas


rulesPruned<- subset(rules, subset = lhs %in% "legs=has_legs")
rulesPruned<-limpiar(rulesPruned)
inspect(rulesPruned)
#explicar por que no es viable, porcentaje de casos

mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage"
                                                   ,"phi", "gini"), transactions=zooNuevo)
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

#install.packages("arulesViz")
library (arulesViz)

plot(rulesPruned)
plot(rulesPruned[1:5], method="graph")
plot(rulesPruned[1:5], method="graph", engine="interactive")
