source("preparation.R")

# entree de l'aval de traite
avalTraite <- 1000000


HorsBilan = matrix(nrow = 5, ncol = 3)
HorsBilan[1,2] = abs(sum(data$CREDOC)) # credit documentaire
HorsBilan[2,2] = abs(sum(data$CAUTION)) # cautions
HorsBilan[3,2] = avalTraite # avaldeTraite
HorsBilan[4,2] = abs(sum(data$CONFIRME)) # Engagement confirmés
HorsBilan[5,2] = sum(HorsBilan[,2], na.rm = TRUE)

# proportion
HorsBilan[,3] = round(HorsBilan[,2] / sum(HorsBilan[1:4,2]),4)

# noms
HorsBilan[,1] = c("Crédits documentaires",
                  "Cautions",
                  "Avals de traite",
                  "Engagements confirmés",
                  "Total hors bilan (hors CES)"
                  )

# noms de colonnes
colnames(HorsBilan) <- c("Encours Hors Bilan par type (Hors CES)",
                         "Montant",
                         "Proportion"
                         )

HorsBilan <- as.data.frame(HorsBilan)
