library(MASS)
library(cluster)

data(crabs)
crabsquant <- crabs[,4:8]
crabsquant <- crabsquant / matrix(rep(crabsquant[,4], dim(crabsquant)[2]),
                                  nrow = dim(crabsquant)[1], byrow = F)
crabsquant <- crabsquant[, -4]
classes <- NULL
classes$sexe <- crabs[, 2]
classes$espece <- crabs[, 1]

inertiesMinimales <- c()
for(j in 1:10) {
    iw <- c()
    for(i in 1:100) {
        kmeans <- kmeans(crabsquant, j, algorithm = "MacQueen")
        iw[i] <- kmeans$tot.withinss
    }
    inertiesMinimales[j] <- apply(as.matrix(iw), 2, min)
}

x <- c(1:10)
png("inertiesMinimales.png")
plot(x, inertiesMinimales,
     type = "l",
     xlab = "Nombre de classes",
     ylab = "Inertie intra-classe minimale",
     xaxt = "n",
     main = "Inerties intra-classes minimales")
axis(side = 1, at = seq(0, 10, 1), labels = seq(0, 10, 1))
dev.off()
cat("inertiesMinimales.png sauvegardee\n")

iw <- c()
for(i in 1:100) {
    kmeans2 <- kmeans(crabsquant, 2, algorithm = "MacQueen")
    iw[i] <- round(kmeans2$tot.withinss, digits = 2)
}
print(table(iw))
cat("\n")

for(i in 1:10) {
    kmeans2 <- kmeans(crabsquant, 2, algorithm = "MacQueen")
    filename <- paste("kmeansCrabs2", i, ".png", sep = "")
    png(filename, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
    clusplot(crabsquant, kmeans2$cluster,
             color = T,
             shade = T,
             labels = 0,
             lines = 0,
             col.p = "black",
             main = "Représentation des classes prédites par la méthode des
             centres mobiles en partitionnant en 2 classes",
             xlab = "Composante 1",
             ylab = "Composante 2",
             sub = "")
    legend(3.8, 2,
           c("Classe 1", "Classe 2"),
           pch = c(1:2))
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(filename, "sauvegardee\n")

    cat("Tableau de contingence des classes prédites par la méthode
        des centres mobiles par rapport aux classes réelles\n")
    print(table(classes$sexe, kmeans2$cluster))
    cat("\n")

    cat("Tableau de contingence des classes prédites par la méthode
        des centres mobiles par rapport aux classes réelles\n")
    print(table(classes$espece, kmeans2$cluster))
    cat("\n")
}

kmeans4 <- kmeans(crabsquant, 4, algorithm = "MacQueen")
png("kmeansCrabs4.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(crabsquant, kmeans4$cluster,
         color = T,
         shade = T,
         labels = 0,
         lines = 0,
         col.p = "black",
         main = "Representation des classes prédites par la méthode des
         centres mobiles en partitionnant en 4 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(3.5, 2,
       c("Classe 1", "Classe 2", "Classe 3", "Classe 4"),
       pch = c(1:4))
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansCrabs4.png sauvegardee\n")

cat("Tableau de contingence des classes prédites par la méthode
    des centres mobiles par rapport aux classes réelles\n")
print(table(classes$sexe, kmeans4$cluster))
cat("\n")

cat("Tableau de contingence des classes prédites par la méthode
    des centres mobiles par rapport aux classes réelles\n")
print(table(classes$espece, kmeans4$cluster))
cat("\n")
