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
        beta <- matrix(0, ncol = 1, nrow = dim(xapp)[2])
    } else {
        beta <- matrix(0, ncol = 1, nrow = dim(xapp)[2] + 1)
    }
    
    #fill zapp with 1 and 0s
    zapp <- apply(zapp, 1,
                  function(row) {
                      if(zapp[1] == 2) {
                          zapp[1] = 0
                      }
                  })

    repeat {
        proba <- exp(xapp %*% beta) / (1 + exp(xapp %*% beta))
        logl <- t(xapp) %*% (zapp - proba)
        w <- diag(proba)
        hess <- -t(xapp) %*% w %*% xapp
        betaPlus1 <- beta - apply(hess, 2, rev) %*% logl#doute

        if(intr) {
            if(sqrt((betaPlus1[1] - beta[1]) ^ 2 + 
                    (betaPlus1[2] - beta[2]) ^ 2 +
                    (betaPlus1[3] - beta[3]) ^ 2)
               < epsi) {
                break
            } else {
                beta <- betaPlus1
                niter++;
            }
        } else {
            if(sqrt((betaPlus1[1] - beta[1]) ^ 2 + (betaPlus1[2] - beta[2]) ^ 2) 
               < epsi) {
                break
            } else {
                beta <- betaPlus1
                niter++;
            }
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
   if(dim(beta)[1] == 3) {
       for(i in 1:nrow(xtst)) {
           nb <- beta[0] + beta[1] * xtst[i,1] + beta[2] * xtst[i,2]
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
           nb <- beta[1] * xtst[i,1] + beta[2] * xtst[i,2]
           probaDF$proba1[i] <- exp(nb) / (1 + exp(nb))
           probaDF$proba2[i] <- 1 / (1 + exp(nb))
           if(nb >= 0) {
               classe[i] <- 1
           } else {
               classe[i] <- 2
           }
       }
   }
   res <- list
   res$deci <- classe
   res$prob <- probaDF
   return(res)
}
