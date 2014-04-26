library(MASS)

simul <- function(n, pi, mu1, mu2, sigma1, sigma2) {
    n1 <- rbinom(1, n, pi)
    n2 <- n - n1
    d1 <- mvrnorm(n1, mu1, sigma1)
    d1 <- cbind(d1, matrix(1, nrow = n1))
    d2 <- mvrnorm(n2, mu2, sigma2)
    d2 <- cbind(d2, matrix(2, nrow = n2))
    d <- rbind(d1, d2)
    return(d)
}

createMatrices <- function(n) {
    mats <- list()
    mats[[1]] <- simul(n, 0.5, c(0,0), c(10, 0), diag(2), diag(2))
    mats[[2]] <- simul(n, 0.5, c(0, 0), c(10, 0), diag(2), 5 * diag(2))
    mats[[3]] <- simul(n, 0.5, c(0, 0), c(10, 0), diag(2), 9 * diag(2))
    mats[[4]] <- simul(n, 0.5, c(0, 0), c(10, 0), 5 * diag(2), 5 * diag(2))
    mats[[5]] <- simul(n, 0.5, c(0, 0), c(10, 0), 9 * diag(2), 9 * diag(2))
    return(mats)
}

moyenneEmpirique <- function(ech) {
    xMean <- round(mean(ech[, 1]), digits = 6)
    yMean <- round(mean(ech[, 2]), digits = 6)
    return(c(xMean, yMean))
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

muMatrices <- list()
matricesApp <- list()
matricesTest <- list()
cat("\nMUS\n")
cat("mu1 x\t\tmu1 y\t\tmu2 x\t\tmu2 y\n")
for(i in 1:5) {
    #shuffle
    matrices[[i]] <- matrices[[i]][sample(nrow(matrices[[i]])),]
    matricesApp[[i]] <- matrices[[i]][1:300,]
    matricesTest[[i]] <- matrices[[i]][301:600,]
    muMatrices[[2 * i - 1]] <-
        moyenneEmpirique(matricesApp[[i]][matricesApp[[i]][,3] == 1,])
    muMatrices[[2 * i]] <- 
        moyenneEmpirique(matricesApp[[i]][matricesApp[[i]][,3] ==2 ,])
    cat(muMatrices[[2 * i - 1]][1], "\t", muMatrices[[2 * i - 1]][2], "\t",
        muMatrices[[2 * i]][1], "\t", muMatrices[[2 * i]][2], "\n",
        sep = "")
}

cat("\nProbabilites d'erreur\n")
for(i in 1:5) {
    cat(erreurEstimee(matricesTest[[i]],
                      regleEuclidienne,
                      muMatrices[[2 * i -1]],
                      muMatrices[[2 * i]]),
        "\n")
}
