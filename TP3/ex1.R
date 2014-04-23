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

matrix1 <- simul(600, 0.5, c(0,0), c(10, 0), diag(2), diag(2))
matrix2 <- simul(600, 0.5, c(0, 0), c(10, 0), diag(2), 5 * diag(2))
matrix3 <- simul(600, 0.5, c(0, 0), c(10, 0), diag(2), 9 * diag(2))
matrix4 <- simul(600, 0.5, c(0, 0), c(10, 0), 5 * diag(2), 5 * diag(2))
matrix5 <- simul(600, 0.5, c(0, 0), c(10, 0), 9 * diag(2), 9 * diag(2))
plot(matrix5[,1], matrix5[, 2], 
     main = "",
     xlab = "Axe x",
     ylab = "Axe y",
     pch = c(1, 2)[matrix5[,3]], 
     col = c("red", "blue")[matrix5[,3]])

matrixApp1 <- simul(300, 0.5, c(0,0), c(10, 0), diag(2), diag(2))
matrixApp2 <- simul(300, 0.5, c(0, 0), c(10, 0), diag(2), 5 * diag(2))
matrixApp3 <- simul(300, 0.5, c(0, 0), c(10, 0), diag(2), 9 * diag(2))
matrixApp4 <- simul(300, 0.5, c(0, 0), c(10, 0), 5 * diag(2), 5 * diag(2))
matrixApp5 <- simul(300, 0.5, c(0, 0), c(10, 0), 9 * diag(2), 9 * diag(2))

moyenneEmpirique <- function(ech) {
    xMean <- mean(ech[, 1])
    yMean <- mean(ech[, 2])
    return(c(xMean, yMean))
}

mu1Matrix1 <- moyenneEmpirique(matrixApp1[matrixApp1[, 3] == 1, ])
mu2Matrix1 <- moyenneEmpirique(matrixApp1[matrixApp1[, 3] == 2, ])

mu1Matrix2 <- moyenneEmpirique(matrixApp2[matrixApp2[, 3] == 1, ])
mu2Matrix2 <- moyenneEmpirique(matrixApp2[matrixApp2[, 3] == 2, ])

mu1Matrix3 <- moyenneEmpirique(matrixApp3[matrixApp3[, 3] == 1, ])
mu2Matrix3 <- moyenneEmpirique(matrixApp3[matrixApp3[, 3] == 2, ])

mu1Matrix4 <- moyenneEmpirique(matrixApp4[matrixApp4[, 3] == 1, ])
mu2Matrix4 <- moyenneEmpirique(matrixApp4[matrixApp4[, 3] == 2, ])

mu1Matrix5 <- moyenneEmpirique(matrixApp5[matrixApp5[, 3] == 1, ])
mu2Matrix5 <- moyenneEmpirique(matrixApp5[matrixApp5[, 3] == 2, ])

cat(mu1Matrix1, "\t", mu2Matrix1, "\n")
cat(mu1Matrix2, "\t", mu2Matrix2, "\n")
cat(mu1Matrix3, "\t", mu2Matrix3, "\n")
cat(mu1Matrix4, "\t", mu2Matrix4, "\n")
cat(mu1Matrix5, "\t", mu2Matrix5, "\n")
