source("functions.R", local = T)

createMatrices <- function(n) {
    mats <- list()
    mats[[1]] <- simul(n, 0.5, c(0,0), c(10, 0), diag(2), diag(2))
    mats[[2]] <- simul(n, 0.5, c(0, 0), c(10, 0), diag(2), 5 * diag(2))
    mats[[3]] <- simul(n, 0.5, c(0, 0), c(10, 0), diag(2), 9 * diag(2))
    mats[[4]] <- simul(n, 0.5, c(0, 0), c(10, 0), 5 * diag(2), 5 * diag(2))
    mats[[5]] <- simul(n, 0.5, c(0, 0), c(10, 0), 9 * diag(2), 9 * diag(2))
    return(mats)
}

regleEuclidienne <- function(x, mu1, mu2) {
    d1 <- sqrt((x[1] - mu1[1]) ^ 2 + (x[2] - mu1[2]) ^ 2)
    d2 <- sqrt((x[1] - mu2[1]) ^ 2 + (x[2] - mu2[2]) ^ 2)
    if(d1 < d2) {
        return(1)
    } else {
        return(2)
    }
}

erreurEstimee <- function(ech, regle, mu1, mu2) {
    classement <- apply(ech, 1, regle, mu1 = mu1, mu2 = mu2)
    ech <- cbind(ech, classement)
    diff <- sum(apply(ech, 1,
                      function(row) {
                          if(row[3] != row[4]) {
                              return(1)
                          }
                          return(0)
                      }))
    return(diff / dim(ech)[1])
}

matrices <- createMatrices(600)
for(i in 1:5) {
    filename <- paste("plot", i, ".png", sep = "")
    png(filename, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 4))
    plot(matrices[[i]][,1], matrices[[i]][,2],
         main = "",
         xlab = "Axe x",
         ylab = "Axe y",
         pch = c(1, 2)[matrices[[i]][,3]],
         col = c("red", "blue")[matrices[[i]][,3]])
    legend(max(matrices[[i]][,1]) + 1.5, max(matrices[[i]][,2]),
           c("Classe 1", "Classe 2"),
           col = c("red", "blue"),
           pch = c(1, 2),
           cex = 0.8)
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(paste(filename, "sauvegardee\n"))
}

repetition <- 10
probas <- data.frame(matrix(nrow = 5, ncol = repetition),
                     row.names = c("1,1", "1,5", "1,9", "5,5", "9,9"))
for(j in 1:repetition) {
    matrices <- createMatrices(600)
    mus <- list()
    matricesApp <- list()
    matricesTest <- list()
    cat("\nMUS\n")
    cat("mu1 x\t\tmu1 y\t\tmu2 x\t\tmu2 y\n")
    for(i in 1:5) {
        #shuffle
        matrices[[i]] <- matrices[[i]][sample(nrow(matrices[[i]])),]
        matricesApp[[i]] <- matrices[[i]][1:300,]
        matricesTest[[i]] <- matrices[[i]][301:600,]
        mus[[2 * i - 1]] <-
            moyenneEmpirique(matricesApp[[i]][matricesApp[[i]][,3] == 1,])
        mus[[2 * i]] <- 
            moyenneEmpirique(matricesApp[[i]][matricesApp[[i]][,3] == 2,])
        cat(mus[[2 * i - 1]][1], "\t", mus[[2 * i - 1]][2], "\t",
            mus[[2 * i]][1], "\t", mus[[2 * i]][2], "\n",
            sep = "")
    }

    cat("\nProbabilites d'erreur\n")
    for(i in 1:5) {
        proba <- erreurEstimee(matricesTest[[i]],
                               regleEuclidienne,
                               mus[[2 * i - 1]],
                               mus[[2 * i]])
        cat(proba, "\n")
        probas[i,j] <- proba
    }
}

cat("\nprobas\n")
print(probas)
cat("\nmeans\n")
print(apply(probas, 1, mean))
cat("\nvariances\n")
print(apply(probas, 1,
            function(row) {
                return(sd(row) ^ 2)
            }))
