library(MASS)

#sans traitement
acpSansTraitement <- princomp(crabs[, 4:8])
png("ex3ACPSansTraitementBiplot.png", width = 400, height = 400)
biplot(acpSansTraitement)
dev.off()

#traitement
crabsNormalises <- crabs[, c(4, 5, 7, 8)] / rowSums(crabs[, c(4, 5, 7, 8)])
acp <- princomp(crabsNormalises)
png("ex3ACPBiplot.png", width = 400, height = 400)
biplot(acp,
       main = "ACP apres traitement",
       xlab = "Composante 1",
       ylab = "Composante 2")
dev.off()

#plot par sexe
png("ex3ACPPlotSpecies.png", width = 450, height = 400)
par(xpd = T, mar = par()$mar + c(0,0,0,7))
plot(acp$scores[, 1], 
     acp$scores[, 2], 
     main = "ACP avec coloration pour les especes",
     xlab = "Composante 1",
     ylab = "Composante 2",
     col = c("blue", "orange")[crabs[, 1]])
legend(0.03, 0.025, 
       c("Espece bleue", "Espece orange"),
       col = c("blue", "orange"),
       cex = 0.8,
       pch = 1)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

png("ex3ACPPlotSexes.png", width = 450, height = 400)
par(xpd = T, mar = par()$mar + c(0,0,0,5))
plot(acp$scores[, 1],
     acp$scores[, 2],
     main = "ACP avec coloration pour le sexe",
     xlab = "Composante 1",
     ylab = "Composante 2",
     col = c("deeppink", "blue")[crabs[, 2]])
legend(0.03, 0.025,
       c("Male", "Femelle"),
       col = c("blue", "deeppink"),
       cex = 0.8,
       pch = 1)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()
