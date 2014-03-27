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

png("boxplotPoids.png")
boxplot(babies[babies$smoke == "Smoking", ]$bwt, 
        babies[babies$smoke == "NonSmoking", ]$bwt,
        notch = T, names = c("Mères fumeuses", "Mères non fumeuses"),
        ylim = c(50, 370))
title("Boxplot du poids des nouveaux-nés en onces
      pour les mères fumeuses et non fumeuses")
dev.off()

png("boxplotGestation.png")
boxplot(babies[babies$smoke == "Smoking", ]$gestation,
        babies[babies$smoke == "NonSmoking", ]$gestation,
        notch = T, names = c("Mères fumeuses", "Mères non fumeuses"),
        ylim = c(50, 370))
title("Boxplot du temps de gestation en jours
      pour les mères fumeuses et non fumeuses")
dev.off()

# Barplot missing
