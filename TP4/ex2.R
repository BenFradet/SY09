source("newtonRaphson.R")

graph <- function(data, filename) {
    len <- dim(data)[1]

    x1 <- seq(min(data$V1), max(data$V1), length = len / 4)
    x2 <- seq(min(data$V2), max(data$V2), length = len / 4)
    grille <- data.frame(expand.grid(V1 = x1, V2 = x2))

    grilleQ <- constructDF(grille)

    lreg <- logreg(data[,c(1,2)], data[,3], T, 1e-3)
    logeva <- logeva(grille, lreg$beta)
    probasL <- logeva$prob[,1] - logeva$prob[,2]

    lreg <- logreg(data[,c(1, 2, 4, 5, 6)], data[,3], T, 1e-3)
    logeva <- logeva(grilleQ, lreg$beta)
    probasQ <- logeva$prob[,1] - logeva$prob[,2]

    pngname <- paste("ex2-", filename, ".png", sep = "")
    png(pngname, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 8))
    plot(data[,c(1,2)],
         col = c("red", "blue")[data$Classe],
         pch = c(1, 2)[data$Classe],
         main = paste("Frontieres de decision des donnees provenant de",
                      filename, "\n apres regression logistique"))
    legend(max(data[,1]) + 2, max(data[,2]),
           c("Classe 1", "Classe 2"),
           col = c("red", "blue"),
           pch = c(1, 2),
           cex = 0.8)
    legend(max(data[,1]) + 2, max(data[,2]) - 6,
           c("Frontiere de decision\nregression lineaire\n",
             "Frontiere de decision\nregression quadratique\n"),
           col = c("skyblue", "orange"),
           lwd = 0.8,
           cex = 0.8)
    contour(x1, x2,
            matrix(probasL, len / 4),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "skyblue")
    contour(x1, x2,
            matrix(probasQ, len / 4),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "orange")
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(paste(pngname, "sauvegardee\n"))
}

constructDF <- function(data) {
    data <- cbind(data, data.frame(V1V2 = data[,1] * data[,2]))
    data <- cbind(data, data.frame(V1V1 = data[,1] * data[,1]))
    data <- cbind(data, data.frame(V2V2 = data[,2] * data[,2]))
}

probaErreur <- function(data, trainingRatio) {
    len <- as.integer(dim(data)[1] * trainingRatio)
    index <- sample(nrow(data), len)
    training <- data[index,]
    test <- data[-index,]

    lregL <- logreg(training[,c(1,2)], training[,3], T, 1e-3)
    lineaire <- logeva(test[,c(1,2)], lregL$beta)

    lregQ <- logreg(training[,c(1, 2, 4, 5, 6)], training[,3], T, 1e-3)
    quadratique <- logeva(test[,c(1, 2, 4, 5, 6)], lregQ$beta)

    test <- cbind(test, lineaire$deci)
    test <- cbind(test, quadratique$deci)
    
    probaErreurL <- sum(apply(test, 1,
                              function(row) {
                                  if(row[7] != row[3]) {
                                      return(1)
                                  }
                                  return(0)
                              })) / dim(test)[1]
    probaErreurQ <- sum(apply(test, 1,
                              function(row) {
                                  if(row[8] != row[3]) {
                                      return(1)
                                  }
                                  return(0)
                              })) / dim(test)[1]
    return(c(probaErreurL, probaErreurQ))
}

data <- c()
for(i in 1:4) {
    filename <- paste("data", i, ".txt", sep = "")
    data[[i]] <- read.table(filename)
    data[[i]] <- constructDF(data[[i]])

    #graph(data[[i]], filename)

    m <- matrix(ncol = 2)
    for(j in 1:200) {
        m <- rbind(m, probaErreur(data[[i]], 2/3))
    }
    cat(paste("for", filename, ":\n"))
    cat("\tproba erreur reg lineaire", mean(m[, 1], na.rm = T), "\n")
    cat("\tvariance reg lineaire", sd(m[, 1], na.rm = T) ^ 2, "\n")
    cat("\tproba erreur reg quadratique", mean(m[, 2], na.rm = T), "\n")
    cat("\tvariance reg quadratique", sd(m[, 2], na.rm = T) ^ 2, "\n\n")

    #lreg <- logreg(data[[i]][,c(1,2)], data[[i]][,3], T, 1e-3)
    #classes <- logeva(data[[i]][,c(1,2)], lreg$beta)
    #data[[i]] <- cbind(data[[i]], classes$deci)
    #probaErreur <- sum(apply(data[[i]], 1,
    #                         function(row) {
    #                             if(row[3] != row[4]) {
    #                                 return(1)
    #                             }
    #                             return(0)
    #                         })) / nrow(data[[i]])
    #cat(probaErreur, "\n")
} 
