#xapp tableau individus-variables
#zapp vecteur Z des variables indicatrices de la classe 1
#intr variable binaire indiquant s'il faut ajouter un intercept
#epsi seuil de convergence
#beta estimateur du maximum de vraisemblance
#niter nombre d'iteration
#logl vraisemblance à l'optimum
logreg <- function(xapp, zapp, intr, epsi) {
    niter <- 0
    beta <- matrix(0, ncol = 1, nrow = dim(xapp)[2] - 1)

    repeat {
        proba <- exp(xapp %*% beta) / (1 + exp(xapp %*% beta))
        logl <- t(xapp) %*% (zapp - proba)
        w <- diag(c(proba[1], proba[2]))
        hess <- -t(xapp) %*% w %*% xapp
        betaPlus1 <- beta - apply(hess, 2, rev) %*% logl
        if(sqrt((betaPlus1[1] - beta[1]) ^ 2 + (betaPlus1[2] - beta[2]) ^ 2) 
           < epsi) {
            break
        } else {
            beta <- betaPlus1
            niter++;
        }
    }
    res <- list()
    res$beta <- betaPlus1
    res$niter <- niter
    res$logl <- logl
    return(res)
}
