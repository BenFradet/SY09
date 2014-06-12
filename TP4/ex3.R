library(MASS)
data(Pima.tr)
data(Pima.te)

source("newtonRaphson.R")

xApp <- as.matrix(Pima.tr[, 1:7])
yApp <- rep(0, dim(Pima.tr)[1])
yApp[Pima.tr[,8] == "No"] <- 1
yApp[Pima.tr[,8] == "Yes"] <- 2
yApp <- as.factor(yApp)

xTest <- as.matrix(Pima.te[,1:7])
yTest <- rep(0, dim(Pima.te)[1])
yTest[Pima.te[,8] == "No"] <- 1
yTest[Pima.te[,8] == "Yes"] <- 2
yTest <- as.factor(yTest)

probaDF <- data.frame(lda = numeric(), qda = numeric(), logreg = numeric())
for(i in 1:200) {
    lda <- lda(xApp, yApp)
    ldaTest <- cbind(yTest, predict(lda, xTest)$class)

    qda <- qda(xApp, yApp)
    qdaTest <- cbind(yTest, predict(qda, xTest)$class)

    lreg <- logreg(xApp, yApp, T, 1e-3)
    leva <- logeva(xTest, lreg$beta)
    logregTest <- cbind(yTest, leva$deci)

    erreurLda <- sum(apply(ldaTest, 1,
                           function(row) {
                               if(row[1] != row[2]) {
                                   return(1)
                               }
                               return(0)
                           })) / dim(xTest)[1]

    erreurQda <- sum(apply(qdaTest, 1,
                           function(row) {
                               if(row[1] != row[2]) {
                                   return(1)
                               }
                               return(0)
                           })) / dim(xTest)[1]

    erreurLogreg <- sum(apply(logregTest, 1,
                              function(row) {
                                  if(row[1] != row[2]) {
                                      return(1)
                                  }
                                  return(0)
                              })) / dim(xTest)[1]

    probaDF[i,] <- c(erreurLda, erreurQda, erreurLogreg)
}

cat("erreur lda:", mean(probaDF$lda), "\n")
cat("erreur qda:", mean(probaDF$qda), "\n")
cat("erreur logreg:", mean(probaDF$logreg), "\n")
