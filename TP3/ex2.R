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
              sd(mat[mat[,3] == 1,][, 1]), sd(mat[mat[,3] == 1,][, 2]), "\n"))
    cat(paste("echantillon", i, "mean f2", mu2[1], mu2[2], "\n"))
    cat(paste("echantillon", i, "variance f2",
              sd(mat[mat[,3] == 2,][, 1]), sd(mat[mat[,3] == 2,][, 2]), "\n"))
}