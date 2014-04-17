library(MASS)
library(cluster)

data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]

kmeans2 <- kmeans(donnees$num, 2, algorithm = "MacQueen")
png("kmeansIris2.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(donnees$num, kmeans2$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 2 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2"),
       pch = c(1,2))
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris2.png sauvegardee\n")

for(i in 1:5) {
    kmeans3 <- kmeans(donnees$num, 3, algorithm = "MacQueen")
    filename <- paste("kmeansIris3", i, ".png", sep = "")
    png(filename, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
    clusplot(donnees$num, kmeans3$cluster,
             color = T, 
             shade = T, 
             labels = 0, 
             lines = 0,
             col.p = "black",
             main = "Representation des classes predites par la methode des
             centres mobiles en partitionnant en 3 classes",
             xlab = "Composante 1",
             ylab = "Composante 2",
             sub = "")
    legend(4.2, 2,
           c("Classe 1", "Classe 2", "Classe 3"),
           pch = c(1:3))
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(filename, "sauvegardee\n")

    if(i == 1) {
        cat("Tableau de contingence des classes predites par la methode
            des centres mobiles par rapport aux classes reelles\n")
        print(table(donnees$cls, kmeans3$cluster))
        cat("\n")
    }
}

kmeans4 <- kmeans(donnees$num, 4, algorithm = "MacQueen")
png("kmeansIris4.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(donnees$num, kmeans4$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 4 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2", "Classe 3", "Classe 4"),
       pch = c(1:4))
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris4.png sauvegardee\n")

inertiesMinimales <- c()
for(j in 1:10) {
    iw <- c()
    for(i in 1:100) {
       kmeans <- kmeans(donnees$num, j, algorithm = "MacQueen")
       iw[i] <- kmeans$tot.withinss
    }

    if(j %in% c(3:5)) {
        roundedIw <- round(iw, digits = 1)
        filename <- paste("hist", j, "Classes.png", sep = "")
        png(filename)
        #barplot(table(round(iw, digits = 0)))
        hist(iw, 
             breaks = length(unique(roundedIw)),
             xlab = "Inertie intra-classe",
             ylab = "Frequence",
             main = paste("Histogramme des inerties intra-classes pour\n", 
                          j, "classes"))
        dev.off()
        cat(filename, "sauvegardee\n")
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

