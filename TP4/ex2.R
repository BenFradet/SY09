source("newtonRaphson.R")

graph <- function(data, filename) {
    len <- dim(data)[1]

    x1 <- seq(min(data$V1), max(data$V1), length = len / 4)
    x2 <- seq(min(data$V2), max(data$V2), length = len / 4)
    grille <- data.frame(expand.grid(V1 = x1, V2 = x2))

    lreg <- logreg(data[,c(1,2)], data[,3], T, 1e-3)
    logeva <- logeva(grille, lreg$beta)

    probas <- logeva$prob[,1] - logeva$prob[,2]

    pngname <- paste("ex2-", filename, ".png", sep = "")
    png(pngname, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 6))
    plot(data[,c(1,2)],
         col = c("red", "blue")[data$Classe],
         pch = c(1, 2)[data$Classe],
         main = paste("Frontieres de decision des donnees provenant de",
                      filename, "\n apres regression logistique"))
    legend(max(data[,1]) + 2, max(data[,2]),
           c("Classe 1", "Classe 2"),
           col = c("red", "blue"),
           pch = c(1, 2),
           cex = 0.8)
    legend(max(data[,1]) + 2, max(data[,2]) - 6,
           "Frontiere de\ndecision",
           col = "skyblue",
           lwd = 0.8,
           cex = 0.8)
    contour(x1, x2,
            matrix(probas, len / 4),
            add = T,
            levels = 0,
            drawlabels = F,
            col = "skyblue")
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(paste(pngname, "sauvegardee\n"))
}

data <- c()
for(i in 1:4) {
    filename <- paste("data", i, ".txt", sep = "")
    data[[i]] <- read.table(filename)
    graph(data[[i]], filename)
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
