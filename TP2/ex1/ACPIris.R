library(MASS)
data(iris)
donnees <- NULL
donnees$num <- iris[,c(1:4)]
donnees$cls <- iris[,5]
ACP <- princomp(donnees$num)
png("ACPIris.png", width = 500, height = 500)
biplot(ACP, 
       xlim = c(-0.20, 0.22),
       xlab = "Axe factoriel 1",
       ylab = "Axe factoriel 2",
       main = "Biplot de l'ACP des iris")
dev.off()
cat("ACPIris.png sauvegardee\n")
