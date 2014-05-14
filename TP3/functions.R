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

moyenneEmpirique <- function(ech) {
    xMean <- round(mean(ech[, 1]), digits = 6)
    yMean <- round(mean(ech[, 2]), digits = 6)
    return(c(xMean, yMean))
}
