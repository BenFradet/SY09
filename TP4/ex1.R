library(MASS)
library(klaR)

doStuff <- function(filename) {
    data <- read.table(filename)
    len <- dim(data)[1]
    lda <- lda(data[,c(1, 2)], data$Classe) 
    qda <- qda(data[,c(1,2)], data$Classe)

    x1 <- seq(min(data$V1), max(data$V1), length = len)
    x2 <- seq(min(data$V2), max(data$V2), length = len)
    grille <- data.frame(expand.grid(V1 = x1, V2 = x2))

    ly <- predict(lda, grille)
    #choisir posterior1 ou posterior2
    lyp <- ly$posterior[,1] - ly$posterior[,2]

    qy <- predict(qda, grille)
    qyp <- qy$posterior[,1] - qy$posterior[,2]

    png(paste(filename, ".png", sep = ""))
    plot(data[,c(1,2)],
         col = c("red", "blue")[data$Classe],
         pch = c(1, 2)[data$Classe],
         main = paste("Frontières de décision des données provenant de", filename, "après analyse linéaire et quadratique"))
    contour(x1, x2, 
            matrix(lyp, len),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "black")
    contour(x1, x2, 
            matrix(qyp, len),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "black")
}

data1 <- read.table("data1.txt")
data2 <- read.table("data2.txt")
data3 <- read.table("data3.txt")
data4 <- read.table("data4.txt")
data5 <- read.table("data5.txt")
data6 <- read.table("data6.txt")

len <- dim(data1)[1]

d1 <- data1[,c(1,2)]
lda1 <- lda(d1, data1$Classe)
qda1 <- qda(d1, data1$Classe)

x1p <- seq(min(data1$V1), max(data1$V1), length = len)
x2p <- seq(min(data1$V2), max(data2$V2), length = len)
grille <- data.frame(expand.grid(V1 = x1p, V2 = x2p))

ly <- predict(lda1, grille)
lyp <- ly$posterior[,1] - ly$posterior[,2]

qy <- predict(qda1, grille)
qyp <- qy$posterior[,1] - qy$posterior[,2]

png("data1.png")
plot(d1, col = c("red", "blue")[data1$Classe], pch = 1, main = "")
contour(x1p, x2p, matrix(lyp, len), add = T, levels = 0, drawlabels = F, col = "black")
contour(x1p, x2p, matrix(qyp, len), add = T, levels = 0, drawlabels = F, col = "black")
dev.off()
