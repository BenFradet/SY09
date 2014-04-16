library(MASS)
library(cluster)

data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]

kmeans2 <- kmeans(donnees$num, 2, algorithm = "MacQueen")
png("kmeansIris2.png")
clusplot(donnees$num, kmeans2$cluster, 
         color = T, shade = T, labels = 2, lines = 0)
dev.off()

kmeans3 <- kmeans(donnees$num, 3, algorithm = "MacQueen")
png("kmeansIris3.png")
clusplot(donnees$num, kmeans3$cluster,
         color = T, shade = T, labels = 0, lines = 0)
dev.off()

kmeans4 <- kmeans(donnees$num, 4, algorithm = "MacQueen")
png("kmeansIris4.png")
clusplot(donnees$num, kmeans4$cluster,
         color = T, shade = T, labels = 0, lines = 0)
dev.off()

inertiesMinimales <- c()
for(j in 1:10) {
    iw <- c()
    for(i in 1:100) {
       kmeans <- kmeans(donnees$num, j, algorithm = "MacQueen")
       iw[i] <- kmeans$tot.withinss
    }

    if(j %in% c(2, 3, 4, 5)) {
        iwCategorisees <- c()
        iwCategorisees[1] <- 0
        iwCategorisees[2] <- 0
        for(i in 1:100) { 
            if(iw[i] < 100) {
                iwCategorisees[1] <- iwCategorisees[1] + 1
            } else {
                iwCategorisees[2] <- iwCategorisees[2] + 1
            }
        }
        png(paste(j, ".png", sep = ""))
        barplot(iwCategorisees)
        dev.off()
    }

    inertiesMinimales[j] <- apply(as.matrix(iw), 2, min)
    cat(j)
}

x <- c(1:10)
png("inertiesMinimales.png")
plot(x, inertiesMinimales)
dev.off()
