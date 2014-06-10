library(MASS)
#library(klaR)

graph <- function(data, filename) {
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

    pngname <- paste("ex1-", filename, ".png", sep = "")
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
    legend(max(data[,1]) + 2, max(data[,2]) - 6,
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
}

probaErreur <- function(data, trainingRatio, pi1, pi2) {
    #probas d'erreur
    len <- as.integer(dim(data)[1] * trainingRatio)
    index <- sample(nrow(data), len)
    training <- data[index,] 
    test <- data[-index,]

    lda <- lda(data[,c(1,2)], data$Classe, 
               prior = c(pi1, pi2), 
               subset = index)
    qda <- qda(data[,c(1,2)], data$Classe,
               prior = c(pi1, pi2),
               subset = index)
    
    training <- cbind(training, predict(lda, training[,c(1,2)])$class)
    colnames(training)[4] <- "predictedLDA"
    training <- cbind(training, predict(qda, training[,c(1,2)])$class)
    colnames(training)[5] <- "predictedQDA"

    if(len != dim(data)[1]) {
        test <- cbind(test, predict(lda, test[,c(1,2)])$class)
        colnames(test)[4] <- "predictedLDA"
        test <- cbind(test, predict(qda, test[,c(1,2)])$class)
        colnames(test)[5] <- "predictedQDA"
    }

    probaTrainingLDA <- sum(apply(training, 1,
                                  function(row) {
                                      if(row["Classe"] != row["predictedLDA"]) {
                                          return(1)
                                      }
                                      return(0)
                                  })) / dim(training)[1]
    probaTrainingQDA <- sum(apply(training, 1,
                                  function(row) {
                                      if(row["Classe"] != row["predictedQDA"]) {
                                          return(1)
                                      }
                                      return(0)
                                  })) / dim(training)[1]

    if(len != dim(data)[1]) {
        probaTestLDA <- sum(apply(test, 1,
                                  function(row) {
                                      if(row["Classe"] != row["predictedLDA"]) {
                                          return(1)
                                      }
                                      return(0)
                                  })) / dim(test)[1]
        probaTestQDA <- sum(apply(test, 1,
                                  function(row) {
                                      if(row["Classe"] != row["predictedQDA"]) {
                                          return(1)
                                      }
                                      return(0)
                                  })) / dim(test)[1]
    } else {
        probaTestLDA <- 0
        probaTestQDA <- 0
    }
    return(c(probaTrainingLDA, probaTrainingQDA, probaTestLDA, probaTestQDA))
}

data <- c()
for(i in 1:6) {
    filename <- paste("data",i,".txt", sep = "")
    data[[i]] <- read.table(filename)
    graph(data[[i]], filename)
    probas <- probaErreur(data[[i]], 1, 0.5, 0.5)
    cat(filename, "for p(test) = 1\n",
        "\tproba erreur lda:", probas[1], "\n",
        "\tproba erreur qda:", probas[2], "\n")

    if(i %in% c(1,2)) {
        for(p in c(1/4, 1/3, 1/2, 2/3, 3/4)) {
            probaDF <- data.frame(trainingLDA = numeric(), 
                                  trainingQDA = numeric(),
                                  testLDA = numeric(),
                                  testQDA = numeric())
            for(j in 1:200) {
                probaDF[j,] <- probaErreur(data[[i]], p, 0.5, 0.5)
            }
            cat(filename, "for p(test) =", p, "\n",
                "\tproba erreur lda training:", mean(probaDF$trainingLDA), "\n",
                "\tproba erreur qda training:", mean(probaDF$trainingQDA), "\n",
                "\tproba erreur lda test:", mean(probaDF$testLDA), "\n",
                "\tproba erreur qda test:", mean(probaDF$testQDA), "\n")
            if(p == 2/3) {
                cat("\tvariance lda test:", sd(probaDF$testLDA) ^ 2, "\n",
                    "\tvariance qda test:", sd(probaDF$testQDA) ^ 2, "\n")
            }
        }
        cat("\n")
    } else {
        for(j in 1:200) {
            probaDF[j,] <- probaErreur(data[[i]], 2/3, 0.5, 0.5)
        }
        cat(filename, "for p(test) = 2/3\n",
            "\tproba erreur lda training:", mean(probaDF$trainingLDA), "\n",
            "\tproba erreur qda training:", mean(probaDF$trainingQDA), "\n",
            "\tproba erreur lda test:", mean(probaDF$testLDA), "\n",
            "\tproba erreur qda test:", mean(probaDF$testQDA), "\n")
        cat("\tvariance lda test:", sd(probaDF$testLDA) ^ 2, "\n",
            "\tvariance qda test:", sd(probaDF$testQDA) ^ 2, "\n\n")
    }
}
