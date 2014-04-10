library(MASS)
library(cluster)

data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$classe <- iris[, 5]
distIris <- dist(donnees$num)

cluster <- agnes(distIris, method = "ward")
png("DendrogrammeIrisWard.png", width = 500, height = 500)
plot(cluster,
     which.plots = 2,
     hang = -1,
     main = "Dendrogramme des iris\npar la methode de Ward",
     xlab = "",
     ylab = "",
     sub = "")
dev.off()
cat("DendrogrammeIrisWard.png sauvegardee\n")

classesAscendantes <- cutree(cluster, 3)

cluster <- diana(distIris)
png("DendrogrammeIrisDesc.png", width = 500, height = 500)
plot(cluster,
     which.plots = 2,
     hang = -1,
     main = "Dendrogramme des iris,\nclassification hierarchique descendate",
     xlab = "",
     ylab = "",
     sub = "")
dev.off()
cat("DendrogrammeIrisDesc.png sauvegardee\n")

classesDescendantes <- cutree(cluster, 3)

cat("Tableau de contingence des classes predites par la CAH par rapport 
    aux classes reelles\n")
print(table(donnees$classe, classesAscendantes))
cat("\n")

cat("Tableau de contingence des classes predites par la CDH par rapport
    aux classes reelles\n")
print(table(donnees$classe, classesDescendantes))
cat("\n")

acp <- princomp(donnees$num)
png("ACPReelles.png", width = 600, height = 500)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
plot(acp$scores[,1],
     acp$scores[,2],
     col = c("purple", "blue", "red")[donnees$classe],
     main = "ACP des iris avec coloration
     en accordance avec les classes reelles",
     xlab = "Axe factoriel 1",
     ylab = "Axe factoriel 2")
legend(4.3, 1.0,
       c("Setosa", "Versicolor", "Virginica"),
       col = c("purple", "blue", "red"),
       cex = 0.8,
       pch = 1)
dev.off()
cat("ACPReelles.png sauvegardee\n")

png("ACPAscendantes.png", width = 600, height = 500)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
plot(acp$scores[,1],
     acp$scores[,2],
     col = c("purple", "blue", "red")[classesAscendantes],
     main = "ACP des iris avec coloration
     en accordance avec les classes predites par la CAH",
     xlab = "Axe factoriel 1",
     ylab = "Axe factoriel 2")
legend(4.3, 1.0,
       c("Setosa", "Versicolor", "Virginica"),
       col = c("purple", "blue", "red"),
       cex = 0.8,
       pch = 1)
dev.off()
cat("ACPAscendantes.png sauvegardee\n")

png("ACPDescendantes.png", width = 600, height = 500)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
plot(acp$scores[,1],
     acp$scores[,2],
     col = c("purple", "blue", "red")[classesDescendantes],
     main = "ACP des iris avec coloration
     en accordance avec les classes predites par la CDH",
     xlab = "Axe factoriel 1",
     ylab = "Axe factoriel 2")
legend(4.3, 1.0,
       c("Setosa", "Versicolor", "Virginica"),
       col = c("purple", "blue", "red"),
       cex = 0.8,
       pch = 1)
dev.off()
cat("ACPDescendantes.png sauvegardee\n")
