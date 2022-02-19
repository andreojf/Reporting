##### QUALITE DU PORTEFEUILLE
library(data.table)
library(tidyverse)
# source("CodesCredit/importData.R")

#datapath <- "C:\\Users\\jaouedraogo\\Documents\\R\\fichierTest2.csv"
#data = read.csv2(datapath, na.strings = "")
#str(data)


dataCleaning <- function(data){
  # changement du format des variables sensées être numériques en numérique
  data[, c("EFFET","COMPTE1","CT","MT","LT","IMPAYES","DOUTEUX","TRESORERIE",
         "DOUTEUX_292","DOUTEUX_SIGNATURE", "PROVISION", "PROVISION_512",
         "CREDOC","CONFIRME","CONFIRME_GROUPE", "CAUTION","SIGNATURE",
         "TOTAL_ENG","GARANTIE","GAR_AUTRES","DEPOSIT_ENGAGEM","DEPOSIT_CREDOC" ,
         "GAR_FINANCIERES","GAR_HYPOTHECAIR","GAR_ENTRE","GAR_ETAT_ORGAN",
         "GAR_LETTRE","GAR_NANTISSEMENTS")] <- sapply(data[, c("EFFET","COMPTE1","CT","MT","LT","IMPAYES","DOUTEUX","TRESORERIE",
                                                               "DOUTEUX_292","DOUTEUX_SIGNATURE", "PROVISION", "PROVISION_512",
                                                               "CREDOC","CONFIRME","CONFIRME_GROUPE", "CAUTION","SIGNATURE",
                                                               "TOTAL_ENG","GARANTIE","GAR_AUTRES","DEPOSIT_ENGAGEM","DEPOSIT_CREDOC" ,
                                                               "GAR_FINANCIERES","GAR_HYPOTHECAIR","GAR_ENTRE","GAR_ETAT_ORGAN",
                                                               "GAR_LETTRE","GAR_NANTISSEMENTS")], as.numeric)
  
  
  # structure de la table
  glimpse(data)
  
  # remplacer les valeurs manquantes par 0
  data[, c("EFFET","COMPTE1","CT","MT","LT","IMPAYES","DOUTEUX","TRESORERIE",
           "DOUTEUX_292","DOUTEUX_SIGNATURE", "PROVISION", "PROVISION_512",
           "CREDOC","CONFIRME","CONFIRME_GROUPE", "CAUTION","SIGNATURE",
           "TOTAL_ENG","GARANTIE","GAR_AUTRES","DEPOSIT_ENGAGEM","DEPOSIT_CREDOC" ,
           "GAR_FINANCIERES","GAR_HYPOTHECAIR","GAR_ENTRE","GAR_ETAT_ORGAN",
           "GAR_LETTRE","GAR_NANTISSEMENTS")] <- sapply(data[, c("EFFET","COMPTE1","CT","MT","LT","IMPAYES","DOUTEUX","TRESORERIE",
                                                                 "DOUTEUX_292","DOUTEUX_SIGNATURE", "PROVISION", "PROVISION_512",
                                                                 "CREDOC","CONFIRME","CONFIRME_GROUPE", "CAUTION","SIGNATURE",
                                                                 "TOTAL_ENG","GARANTIE","GAR_AUTRES","DEPOSIT_ENGAGEM","DEPOSIT_CREDOC" ,
                                                                 "GAR_FINANCIERES","GAR_HYPOTHECAIR","GAR_ENTRE","GAR_ETAT_ORGAN",
                                                                 "GAR_LETTRE","GAR_NANTISSEMENTS")], function(x) {
                                                                   x[is.na(x)] <- 0
                                                                   x
                                                                 })
  
  # changer du format date des variables date
  data$DATE_COMPTA <- as.Date(data$DATE_COMPTA, format = "%d/%m/%Y")
  data$DATE_DECL <- as.Date(data$DATE_DECL, format = "%d/%m/%Y")
  
  # Creation de la colonne des restructures
  data <- data %>%
    mutate(Restructures = TRESORERIE - DOUTEUX_292 - EFFET - COMPTE1 - CT - LT - MT - IMPAYES)
  
  return(data)
}


# application de la fonction
final.df <- final.df %>% 
  dataCleaning


## Store the output
write.csv(final.df,"C:/Users/jaouedraogo/Documents/Coris/Reporting/Folder/Inputs.csv", row.names = FALSE)
