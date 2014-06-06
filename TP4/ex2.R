source("newtonRaphson.R")


data <- c()
for(i in 1:4) {
    filename <- paste("data", i, ".txt", sep = "")
    data[[i]] <- read.table(filename)
    lreg <- logreg(data[[i]][,c(1,2)], data[[i]][,3], T, 1e-3)
    #print(lreg)
    classes <- logeva(data[[i]][,c(1,2)], lreg$beta)
    data[[i]] <- cbind(data[[i]], classes$deci)
    probaErreur <- sum(apply(data[[i]], 1,
                             function(row) {
                                 if(row[3] != row[4]) {
                                     return(1)
                                 }
                                 return(0)
                             })) / nrow(data[[i]])
    cat(probaErreur, "\n")
} 
