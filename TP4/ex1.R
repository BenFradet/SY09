library(MASS)
#library(klaR)

doStuff <- function(filename) {
    data <- read.table(filename)
    len <- dim(data)[1]
    lda <- lda(data[,c(1, 2)], data$Classe) 
    qda <- qda(data[,c(1,2)], data$Classe)

    #frontieres de decision
    x1 <- seq(min(data$V1), max(data$V1), length = len)
    x2 <- seq(min(data$V2), max(data$V2), length = len)
    grille <- data.frame(expand.grid(V1 = x1, V2 = x2))

    ly <- predict(lda, grille)
    lyp <- ly$posterior[,1] - ly$posterior[,2]

    qy <- predict(qda, grille)
    qyp <- qy$posterior[,1] - qy$posterior[,2]

    pngname <- paste(filename, ".png", sep = "")
    png(pngname, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 8))
    plot(data[,c(1,2)],
         col = c("red", "blue")[data$Classe],
         pch = c(1, 2)[data$Classe],
         main = paste("Frontières de décision des données provenant de", 
                      filename, "\naprès analyse linéaire et quadratique"))
    legend(max(data[,1]) + 2, max(data[,2]),
           c("Classe 1", "Classe 2"),
           col = c("red", "blue"),
           pch = c(1, 2),
           cex = 0.8)
    legend(max(data[,1]) + 2, max(data[,2]) - 4,
           c("Frontiere de decision\nissue de la lda\n",
             "Frontiere de decision\nissue de la qda"),
           col = c("skyblue", "orange"),
           lwd = 0.8,
           cex = 0.8)
    contour(x1, x2, 
            matrix(lyp, len),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "skyblue")
    contour(x1, x2, 
            matrix(qyp, len),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "orange")
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(paste(pngname, "sauvegardee\n"))

    #probas d'erreur
    data <- cbind(data, predict(lda, data[,c(1,2)])$class)
    colnames(data)[4] <- "predictedLDA"

    data <- cbind(data, predict(qda, data[,c(1,2)])$class)
    colnames(data)[5] <- "predictedQDA"

    probaLDA <- sum(apply(data, 1,
                          function(row) {
                              if(row["Classe"] != row["predictedLDA"]) {
                                  return(1)
                              }
                              return(0)
                          })) / len
    probaQDA <- sum(apply(data, 1,
                          function(row) {
                              if(row["Classe"] != row["predictedQDA"]) {
                                  return(1)
                              }
                              return(0)
                          })) / len
    cat(filename, "proba lda:", probaLDA, ", proba qda:", probaQDA, "\n")
}

for(i in 1:6) {
    doStuff(paste("data",i,".txt", sep = ""))
}
