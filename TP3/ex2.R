source("functions.R", local = T)
source("circle.R", local = T)

regleBayes <- function(x, a, b) {
    if(x[2] < a * x[1] + b) {
        return(1)
    } else {
        return(2)
    }
}

bindBayes <- function(mat, regle, a, b) {
    classement <- apply(mat, 1, regle, a = a, b = b)
    mat <- cbind(mat, classement)
    return(mat)
}

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

mat1 <- simul(1000, 0.5, c(-1, -1), c(1, 1), diag(2), diag(2))
png("cas1et2.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 4))

plot(mat1[,1], mat1[,2],
     main = "",
     xlab = "x1",
     ylab = "x2",
     pch = c(1, 2)[mat1[,3]],
     col = c("red", "blue")[mat1[,3]])

legend(max(mat1[,1]) + 0.5, max(mat1[,2]),
       c("Classe 1", "Classe 2"),
       col = c("red", "blue"),
       pch = c(1, 2),
       cex = 0.8)

text(-2, 3,
     labels = "x2 = -x1",
     cex = 0.8,
     col = "purple")
text(1, -4,
     labels = "x2 = -x1 - ln(10) / 2",
     cex = 0.8,
     col = "aquamarine4")

abline(h = 0, xpd = F)
abline(v = 0, xpd = F)
abline(0, -1, xpd = F, col = "purple")
abline(-log(10) / 2, -1, xpd = F, col = "aquamarine4")

par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("cas1et2.png sauvegardee\n")

mat3 <- simul(1000, 1 / 11, c(-1, -1), c(1, 1), diag(2), diag(2))
png("cas3.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 4))

plot(mat3[,1], mat3[,2],
     main = "",
     xlab = "x1",
     ylab = "x2",
     pch = c(1, 2)[mat3[,3]],
     col = c("red", "blue")[mat3[,3]])

legend(max(mat3[,1]) + 0.5, max(mat3[,2]),
       c("Classe 1", "Classe 2"),
       col = c("red", "blue"),
       pch = c(1, 2),
       cex = 0.8)

text(3, -3, 
     labels = "x2 = -x1 - ln(10) / 2", 
     cex = 0.8, 
     col = "purple")

abline(h = 0, xpd = F)
abline(v = 0, xpd = F)
abline(-log(10) / 2, -1, xpd = F, col = "purple")

par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("cas3.png sauvegardee\n")

#calcul des estimateurs regle de bayes
#cas1
mat <- simul(1000, 0.5, c(-1, -1), c(1, 1), diag(2), diag(2))
mat <- bindBayes(mat, regleBayes, -1, 0)
estimateurAlpha <- sum(apply(mat, 1,
                             function(row) {
                                 if(row[3] == 1 && row[4] == 2) {
                                     return(1)
                                 }
                                 return(0)
                             })) / sum(mat[,3] == 1)
estimateurBeta <- sum(apply(mat, 1,
                            function(row) {
                                if(row[3] == 2 && row[4] == 1) {
                                    return(1)
                                }
                                return(0)
                            })) / sum(mat[,3] == 2)
cat("cas 1 alpha:", estimateurAlpha, "beta:", estimateurBeta, "\n")

#cas2
mat <- simul(1000, 0.5, c(-1, -1), c(1, 1), diag(2), diag(2))
mat <- bindBayes(mat, regleBayes, -1, -log(10) / 2)
estimateurAlpha <- sum(apply(mat, 1,
                             function(row) {
                                 if(row[3] == 1 && row[4] == 2) {
                                     return(1)
                                 }
                                 return(0)
                             })) / sum(mat[,3] == 1)
estimateurBeta <- sum(apply(mat, 1,
                            function(row) {
                                if(row[3] == 2 && row[4] == 1) {
                                    return(1)
                                }
                                return(0)
                            })) / sum(mat[,3] == 2)
cat("cas 2 alpha:", estimateurAlpha, "beta:", estimateurBeta, "\n")

#cas3
mat <- simul(1000, 1/11, c(-1, -1), c(1, 1), diag(2), diag(2))
mat <- bindBayes(mat, regleBayes, -1, -log(10) / 2)
estimateurAlpha <- sum(apply(mat, 1,
                             function(row) {
                                 if(row[3] == 1 && row[4] == 2) {
                                     return(1)
                                 }
                                 return(0)
                             })) / sum(mat[,3] == 1)
estimateurBeta <- sum(apply(mat, 1,
                            function(row) {
                                if(row[3] == 2 && row[4] == 1) {
                                    return(1)
                                }
                                return(0)
                            })) / sum(mat[,3] == 2)
cat("cas 3 alpha:", estimateurAlpha, "beta:", estimateurBeta, "\n")
