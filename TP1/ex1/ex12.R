library(MASS)

#graph sexes
variableNames <- c("Fontal Lobe\nSize", "Rear\nWidth", "Carapace\nLength",
                "Carapace\nWidth", "Body\nDepth")
png("sexesGraphMatriciel.png", width = 800, height = 800)
plot(crabs[, 4:8], 
     col = c("deeppink", "blue")[crabs[, 2]],
     pch = 24,
     main = "Graphique matriciel des caracteristiques morphologiques
     de 200 crabes en fonction de leur sexe",
     labels = variableNames)
par(xpd = T)
legend("topright", 
       c("Male", "Femelle"),
       col = c("blue", "deeppink"),
       cex = 0.8,
       horiz = T,
       pch = 24)
par(xpd = F)
dev.off()
cat("sexesGraphMatriciel.png sauvegardee\n")

#graph especes
png("speciesGraphMatriciel.png", width = 800, height = 800)
par(xpd = T)
plot(crabs[, 4:8],
     col = c("blue", "orange")[crabs[, 1]],
     pch = 24,
     main = "Graphique matriciel des caracteristiques morphologiques
     de 200 crabes en fonction de leur espece",
     labels = variableNames)
legend("topright",
       c("Espece bleue", "Espece orange"),
       col = c("blue", "orange"),
       cex = 0.8,
       horiz = T,
       pch = 24)
par(xpd = F)
dev.off()
cat("speciesGraphMatriciel.png sauvegardee\n")

#regression lineaire
cat("\n\n\tRegression lineaire\n")
X1 <- crabs[, 4]
X2 <- crabs[, 5]
X3 <- crabs[, 6]
X4 <- crabs[, 7]
X5 <- crabs[, 8]
linearModel <- lm(X1 ~ X2 + X3 + X4 + X5, data = crabs)
print(summary(linearModel))

#correlation
cat("\n\n\tCorrelation\n")
print(cor.test(X1, X2, X3, X4, X5, alternative = "greater", method = "kendall"))
print(cor(crabs[, 4:8]))
