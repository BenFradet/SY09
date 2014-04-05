library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
crabsquant <- crabsquant / matrix(rep(crabsquant[,4], dim(crabsquant)[2]),
                                  nrow = dim(crabsquant)[1], byrow = F)
crabsquant <- crabsquant[,-4]
ACP <- princomp(crabsquant)
png("ACPCrabs.png", width = 500, height = 500)
biplot(ACP,
       xlab = "Axe factoriel 1",
       ylab = "Axe factoriel 2",
       main = "biplot de l'ACP des crabs")
dev.off()
cat("ACPCrabs.png sauvegardee\n")
