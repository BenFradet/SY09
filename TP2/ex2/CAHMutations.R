mut <- read.table("mutations2.txt", header = F, row.names = 1)
distMut <- as.dist(mut)
cluster <- hclust(distMut, "single")
png("DendrogrammeMutationsSingle.png", width = 500, height = 500)
plot(cluster,
     hang = -1,
     main = "Dendrogramme des especes\npar le critere du lien minimum",
     xlab = "",
     ylab = "Indice",
     sub = "")
dev.off()
cat("DendrogrammeMutationsSingle.png sauvegardee\n")

cluster <- hclust(distMut, "complete")
png("DendrogrammeMutationsComplete.png", width = 500, height = 500)
plot(cluster,
     hang = -1,
     main = "Dendrogramme des especes\npar le critere du lien maximum",
     xlab = "",
     ylab = "Indice",
     sub = "")
dev.off()
cat("DendrogrammeMutationsComplete.png sauvegardee\n")

cluster <- hclust(distMut, "average")
png("DendrogrammeMutationsAverage.png", width = 500, height = 500)
plot(cluster,
     hang = -1,
     main = "Dendrogramme des especes\npar le critere du lien moyen",
     xlab = "",
     ylab = "Indice",
     sub = "")
dev.off()
cat("DendrogrammeMutationsAverage.png sauvegardee\n")

library(cluster)
cluster <- agnes(distMut, method = "ward")
png("DendrogrammeMutationsWard.png", width = 500, height = 500)
plot(cluster,
     which.plots = 2,
     hang = -1,
     main = "Dendrogramme des especes\npar la methode de Ward",
     xlab = "",
     ylab = "Indice",
     sub = "")
dev.off()
cat("DendrogrammeMutationsWard.png sauvegardee\n")
