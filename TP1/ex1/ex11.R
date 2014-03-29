babies <- read.table("babies23.txt", header = T)

babies <- babies[c(7, 5, 8, 10, 12, 13, 21, 11)]
names(babies) <- c("bwt", "gestation", "parity", "age", "height", "weight",
                  "smoke", "education")
babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 5] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- NA

babies$smoke <- factor(c("NonSmoking", "Smoking", "NonSmoking", "NonSmoking")
                      [babies$smoke + 1])
babies$education <- factor(babies$education, ordered = T)

png("boxplotPoids.png", width = 400, height = 400)
boxplot(babies[babies$smoke == "Smoking", ]$bwt, 
        babies[babies$smoke == "NonSmoking", ]$bwt,
        notch = T, 
        names = c("Mères fumeuses", "Mères non fumeuses"),
        ylim = c(50, 370))
title("Boxplot du poids des nouveaux-nés en onces
      pour les mères fumeuses et non fumeuses")
dev.off()
cat("boxplotPoids.png sauvegardee\n")

png("boxplotGestation.png", width = 400, height = 400)
boxplot(babies[babies$smoke == "Smoking", ]$gestation,
        babies[babies$smoke == "NonSmoking", ]$gestation,
        notch = T, 
        names = c("Mères fumeuses", "Mères non fumeuses"),
        ylim = c(50, 370))
title("Boxplot du temps de gestation en jours
      pour les mères fumeuses et non fumeuses")
dev.off()
cat("boxplotGestation.png sauvegardee\n")

table <- table(babies$smoke, babies$education)
educationCategories = c("<8th grade", "8th<grade<12th", "HS graduate",
                     "HS + trade", "Some college", "College graduate", 
                     "Trade school")
png("barplotEducation.png", width = 650, height = 400)
barplot(table, main = "Repartition des meres fumeuses et non fumeuses
        en fonction de leur niveau d'education", 
        col = c("darkblue", "skyblue"),
        legend = c("Meres non fumeuses", "Meres fumeuses"),
        names.arg = educationCategories,
        cex.names = 0.8,
        xlab = "Niveau d'education", 
        ylab = "Effectif de meres",
        ylim = c(0,500), 
        font.lab = 2)
dev.off()
cat("barplotEducation.png sauvegardee\n")

#chi2
cat("\n\n\tChi2\n")
smokingEducationTable <- table(babies$education, babies$smoke)
smokingEducationMatrix <- as.matrix(smokingEducationTable)
rownames(smokingEducationMatrix) <- educationCategories
print(smokingEducationMatrix)
print(chisq.test(smokingEducationMatrix))
