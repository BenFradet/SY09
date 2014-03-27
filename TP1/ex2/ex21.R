X <- matrix(c(3, 4, 3, 1, 4, 3, 2, 3, 6, 2, 1, 4), nrow = 4, ncol = 3, byrow = T)
X <- scale(X, center = T, scale = F)
n <- dim(X)[1]
Dp <- diag(n) * 1 / n
V <- t(X) %*% Dp %*% X
tmp <- eigen(V)
L <- diag(tmp$values)
U <- tmp$vectors
C <- X %*% U

png('individus.png')
plot(C[, 1], C[, 2])
dev.off()

png('variables.png')
plot(C[1,], C[2,])
dev.off()

K1 <- C[, 1] %*% t(U[, 1])
K2 <- K1 + C[, 2] %*% t(U[, 2])
K3 <- K2 + C[, 3] %*% t(U[, 3])
