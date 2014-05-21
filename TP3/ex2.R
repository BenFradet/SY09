source("functions.R", local = T)

for(i in 1:5) {
    mat <- simul(1 * 10 ** i, 0.5, c(-1, -1), c(1, 1), diag(2), diag(2))
    filename <- paste("ex2Plot", i, ".png", sep = "")
    png(filename, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 4))
    plot(mat[,1], mat[,2],
         main = "",
         xlab = "Axe x",
         ylab = "Axe y",
         pch = c(1, 2)[mat[,3]],
         col = c("red", "blue")[mat[,3]])
    legend(max(mat[,1]) + 0.5, max(mat[,2]),
           c("Classe 1", "Classe 2"),
           col = c("red", "blue"),
           pch = c(1, 2),
           cex = 0.8)
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(paste(filename, "sauvegardee\n"))

    mu1 = moyenneEmpirique(mat[mat[,3] == 1,])
    mu2 = moyenneEmpirique(mat[mat[,3] == 2,])
    cat(paste("echantillon", i, "mean f1", mu1[1], mu1[2], "\n"))
    cat(paste("echantillon", i, "variance f1",
              sd(mat[mat[,3] == 1,][,1]), sd(mat[mat[,3] == 1,][,2]), "\n"))
    cat(paste("echantillon", i, "mean f2", mu2[1], mu2[2], "\n"))
    cat(paste("echantillon", i, "variance f2",
              sd(mat[mat[,3] == 2,][,1]), sd(mat[mat[,3] == 2,][,2]), "\n"))
}

#cste <- round(1 / (2 * pi), 2)
cste <- 0.15
png("cercles.png", width = 400, height = 400)
plot(0, 0,
     type = "n",
     xlab = "x1",
     ylab = "x2",
     xlim = c(-3, 3),
     ylim = c(-3, 3))

circle(c(-1, -1), sqrt(-2 * log(2 * pi * cste)), border = "red")
circle(c(-1, -1), sqrt(-2 * log(2 * pi * cste * 2 / 3)), border = "red")
circle(c(-1, -1), sqrt(-2 * log(2 * pi * cste * 1 / 3)), border = "red")

circle(c(1, 1), sqrt(-2 * log(2 * pi * cste)), border = "blue")
circle(c(1, 1), sqrt(-2 * log(2 * pi * cste * 2 / 3)), border = "blue")
circle(c(1, 1), sqrt(-2 * log(2 * pi * cste * 1 / 3)), border = "blue")

dev.off()
cat("cercles.png sauvegardee\n")

mat <- simul(1000, 0.5, c(-1, -1), c(1, 1), diag(2), diag(2))
png("cas1et2.png", width = 400, height = 400)
plot(mat[,1], mat[,2],
     main = "",
     xlab = "x1",
     ylab = "y1",
     pch = c(1, 2)[mat[,3]],
     col = c("red", "blue")[mat[,3]])

abline(h = 0)
abline(v = 0)
abline(0, -1)
abline(-log(10) / 2, -1)
dev.off()

mat <- simul(1000, 1 / 11, c(-1, -1), c(1, 1), diag(2), diag(2))
png("cas3.png", width = 400, height = 400)
plot(mat[,1], mat[,2],
     main = "",
     xlab = "x1",
     ylab = "x2",
     pch = c(1, 2)[mat[,3]],
     col = c("red", "blue")[mat[,3]])

abline(h = 0)
abline(v = 0)
abline(-log(10) / 2, -1)
dev.off()
