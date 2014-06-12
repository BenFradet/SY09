#xapp tableau individus-variables
#zapp vecteur Z des variables indicatrices de la classe 1
#intr variable binaire indiquant s'il faut ajouter un intercept
#epsi seuil de convergence
#res$beta estimateur du maximum de vraisemblance
#res$niter nombre d'iteration
#res$logl vraisemblance à l'optimum
logreg <- function(xapp, zapp, intr, epsi) {
    niter <- 0
    if(intr) {
        beta <- matrix(0, ncol = 1, nrow = dim(xapp)[2] + 1)
        xapp <- cbind(rep(1, times = nrow(xapp)), xapp)
    } else {
        beta <- matrix(0, ncol = 1, nrow = dim(xapp)[2])
    }
    
    #fill zapp with 1 and 0s
    zapp <- sapply(zapp,
                  function(elem) {
                      if(elem == 2) {
                          elem = 0
                      } else {
                          elem = 1
                      }
                  })

    repeat {
        xappMat <- as.matrix(xapp)
        proba <- exp(xappMat %*% beta) / (1 + exp(xappMat %*% beta))
        w <- diag(proba[,1])
        logl <- t(xappMat) %*% (zapp - proba)
        hess <- -t(xappMat) %*% w %*% xappMat
        betaPlus1 <- beta - solve(hess) %*% logl

        distance <- sqrt(sum((betaPlus1 - beta) ^ 2))
        if(distance < epsi) {
            break
        } else {
            beta <- betaPlus1
            niter <- niter + 1
        }
    }
    res <- list()
    res$beta <- betaPlus1
    res$niter <- niter
    res$logl <- logl
    return(res)
}

#xtst matrice de test
#beta parametres estimes
#res$deci classe decidee
#res$prob probas a posteriori
logeva <- function(xtst, beta) {
   classe <- c()
   probaDF <- data.frame(proba1 = numeric(), proba2 = numeric())
   if(dim(beta)[1] != dim(xtst)[2]) {
       for(i in 1:nrow(xtst)) {
           nb <- beta[,1][[1]]
           for(j in 1:dim(xtst)[2]) {
               nb <- nb + beta[,1][[j + 1]] * xtst[i,j]
           }
           probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
           probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
           probaDF$proba2[i] <- 1 / (1 + exp(nb))
           if(nb >= 0) {
               classe[i] <- 1
           } else {
               classe[i] <- 2
           }
       }
   } else {
       for(i in 1:nrow(xtst)) {
           nb <- 0
           for(j in 1:dim(xtst)[2]) {
               nb <- nb + beta[,1][[j]] * xtst[i,j]
           }
           probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
           probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
           probaDF$proba2[i] <- 1 / (1 + exp(nb))
           if(nb >= 0) {
               classe[i] <- 1
           } else {
               classe[i] <- 2
           }
       }
   }
   #if(dim(beta)[1] == 3) {
   #    for(i in 1:nrow(xtst)) {
   #        nb <- beta[, 1][[1]] + beta[, 1][[2]] * xtst[i,1] + 
   #             beta[, 1][[3]] * xtst[i,2]
   #        probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
   #        probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
   #        probaDF$proba2[i] <- 1 / (1 + exp(nb))
   #        if(nb >= 0) {
   #            classe[i] <- 1
   #        } else {
   #            classe[i] <- 2
   #        }
   #    }
   #} else if(dim(beta)[1] == 2) {
   #    for(i in 1:nrow(xtst)) {
   #        nb <- beta[1] * xtst[i,1] + beta[2] * xtst[i,2]
   #        probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
   #        probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
   #        probaDF$proba2[i] <- 1 / (1 + exp(nb))
   #        if(nb >= 0) {
   #            classe[i] <- 1
   #        } else {
   #            classe[i] <- 2
   #        }
   #    }
   #} else if(dim(beta)[1] == 5) {
   #    for(i in 1:nrow(xtst)) {
   #        nb <- beta[, 1][[1]] * xtst[i,1] + beta[, 1][[2]] * xtst[i,2] + 
   #         beta[, 1][[3]] * xtst[i, 3] + beta[, 1][[4]] * xtst[i, 4] +
   #         beta[, 1][[5]] * xtst[i, 5]
   #        probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
   #        probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
   #        probaDF$proba2[i] <- 1 / (1 + exp(nb))
   #        if(nb >= 0) {
   #            classe[i] <- 1
   #        } else {
   #            classe[i] <- 2
   #        }
   #    }
   #} else {
   #    for(i in 1:nrow(xtst)) {
   #        nb <- beta[, 1][[1]] +  beta[, 1][[2]]  * xtst[i,1] + 
   #         beta[, 1][[3]] * xtst[i,2] + beta[, 1][[4]] * xtst[i, 3] + 
   #         beta[, 1][[5]] * xtst[i, 4] + beta[, 1][[6]] * xtst[i, 5]
   #        probaDF <- rbind(probaDF, data.frame(proba1 = 0, proba2 = 0))
   #        probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
   #        probaDF$proba2[i] <- 1 / (1 + exp(nb))
   #        if(nb >= 0) {
   #            classe[i] <- 1
   #        } else {
   #            classe[i] <- 2
   #        }
   #    }
   #}
   res <- list()
   res$deci <- classe
   res$prob <- probaDF
   return(res)
}
