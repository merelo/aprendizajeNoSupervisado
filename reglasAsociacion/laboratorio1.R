library(arules)

data("AdultUCI")
dim(AdultUCI)
AdultUCI[1:2,]

AdultUCI[["fnlwgt"]] = NULL
AdultUCI[["education-num"]] = NULL

AdultUCI[[ "age"]] = ordered( cut ( AdultUCI[[ "age"]], c(15,25,45,65,100) ) ,
                              labels = c ("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[[ "hours-per-week"]] = ordered( cut ( AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168) ) ,
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[[ "capital-gain"]] = ordered( cut ( AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[["capital-gain"]][AdultUCI[[ "capital-gain"]]>0]), Inf) ) ,
                                       labels = c("None", "Low", "High"))
AdultUCI[[ "capital-loss"]] = ordered( cut ( AdultUCI[[ "capital-loss"]], c(-Inf,0, median(AdultUCI[["capital-loss"]][AdultUCI[[ "capital-loss"]]>0]), Inf) ) , 
                                       labels = c("None", "Low", "High"))
Adult <- as(AdultUCI, "transactions")
Adult

summary(Adult)


data(Epub)
summary(Epub)
image(Epub)

itemFrequencyPlot(Adult,support=0.1,cex.names=0.8)

iAdult<-apriori(Adult, parameter = list(support = 0.1, target="frequent"))
iAdult<-sort(iAdult, by="support") # Los ordenamosporel valor del soporte
inspect(head(iAdult, n=10)) # Inspeccionamoslos 10 primeros


size(iAdult)
barplot(table(size(iAdult)), xlab="itemsetsize", ylab="count")
inspect(iAdult[size(iAdult)==1])

#maximales
imaxAdult<-iAdult[is.maximal(iAdult)]
inspect(head(sort(imaxAdult, by="support")))

#itemsets cerrados
icloAdult<-iAdult[is.closed(iAdult)]
inspect(head(sort(icloAdult, by="support")))

#grafica itemset frecuentes, cerrados y maximales
barplot( c(frequent=length(iAdult), closed=length(icloAdult), maximal=length(imaxAdult)), ylab="count", xlab="itemsets")

#minimo soporte 0.1 confianza 0.8
rules <-apriori(Adult, parameter = list(support = 0.1, confidence = 0.8, minlen= 2))
summary(rules)

#vemos reglas
inspect(head(rules))
quality(head(rules)) #medidas de calidad

#ordenar por valor de confianza
rulesSorted= sort(rules, by = "confidence")
inspect(head(rulesSorted))

#subconjunto que cumpla una regla
rulesRaceWhite<-subset(rules, subset = lhs %in% "race=White" & lift > 1.2)
inspect(head(rulesRaceWhite))

#eliminar reglas redundantes
subsetMatrix<-is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <-FALSE
redundant <-colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesPruned<-rulesSorted[!redundant] # remove redundant rules
inspect(head(rulesPruned))

