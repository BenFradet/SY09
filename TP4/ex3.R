library(MASS)
data(Pima.tr)
data(Pima.te)

xApp <- as.matrix(Pima.tr[, 1:7])
yApp <- rep(0, dim(Pima.tr)[1])
yApp[Pima.tr[,8] == "No"] <- 1
yApp[Pima.tr[,8] == "Yes"] <- 2
yApp <- as.factor(yApp)

xTest <- as.matrix(Pima.te[,1:7])
yTest <- rep(0, dim(Pima.te)[1])
yTest[Pima.te[,8] == "No"] <- 1
yTest[Pima.te[,8] == "Yes"] <- 2
tTest <- as.factor(yTest)

print(head(xApp))
