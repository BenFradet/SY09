M <- read.table("mutations2.txt", header = F, row.names = 1)
AFTD <- cmdscale(dist(M))
png("AFTDMutations.png", width = 600, height = 600)
plot(AFTD[,1], AFTD[,2],
     type = "n",
     main = "Représentation de l'AFTD des espèces",
     xlab = "",
     ylab = "",
     xlim = c(-50, 200))
text(AFTD[,1], AFTD[,2],
     rownames(AFTD),
     cex = 0.8)
dev.off()
cat("AFTDMutations.png sauvegardee\n")

library(MASS)
shepard <- Shepard(dist(M), cmdscale(dist(M)))
png("ShepardMutations.png", width = 450, height = 450)
plot(shepard$x, shepard$y)
points(shepard$x, shepard$yf, col = "red")
title("Vérification de l'amélioration des données
      par la méthode de Shepard")
dev.off()
cat("ShepardMutations.png sauvegardee\n")
