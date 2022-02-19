################################################################################
########### INDICATEURS DE RAPPORT SEMESTRIEL ENVOYE A LA COBA #################
################################################################################
library(xlsx)
library(dplyr)
library(plyr)
library(openxlsx)
# 1. IMPORTATION DES DONNEES
filepaths <- list.files(path="C:/Users/jaouedraogo/Documents/Coris/Reporting/Semestriels/Donnees/",
                        recursive=T,
                        pattern=".xlsx",
                        full.names=T)

holding.df <- createDatasetHolding(filepaths)
### detacher le package plyr
detach("package:plyr", unload = TRUE)

holding.df <- holding.df %>%
  dataCleaning


# 2. SITUATION DU PORTEFEUILLE 
evolPortefeuil <- function(df){
  
  df %>%
    group_by(DATE_COMPTA) %>%
    summarise(
      # crédits directs
      ENGAGEMENTS_DIRECTS = sum(-TRESORERIE),
      # crédits indirects
      ENGAGEMENTS_INDIRECTS = sum(-SIGNATURE),
      # Portefeuille Douteux Crédits Directs
      PFT_DT_ENG_DIRECTS = sum(-DOUTEUX_292), 
      # Portefeuille Douteux Eng. Hors bilan
      PFT_DT_ENG_INDIRECTS = sum(-DOUTEUX_SIGNATURE),
      # Créances restructures
      CREDIT_RESTRUCTURES = sum(-Restructures), 
      # Provisions
      PROVISIONS = sum(PROVISION),
      # Portefeuille global
      PORTEFEUILLE_GLOBAL = sum(-TRESORERIE) + sum(-SIGNATURE),
      # Creances en souffrances directes
      CES_DIRECTES = sum(-Restructures) + sum(-DOUTEUX_292),
      # creances en souffrance directes / crédit directs
      CES_DIREC_SUR_CRED_DIREC = (sum(-Restructures) + sum(-DOUTEUX_292)) / sum(-TRESORERIE),
      # creances en souffrance / portefeuille global
      CES_SUR_GLOBAL = (sum(-Restructures) + sum(-DOUTEUX_292) + sum(-DOUTEUX_SIGNATURE)) / (sum(-TRESORERIE) + sum(-SIGNATURE))
    ) %>%
    filter(!is.na(DATE_COMPTA)) %>%
    rename("Crédits Directs" = ENGAGEMENTS_DIRECTS,
           "Engagements hors bilan" = ENGAGEMENTS_INDIRECTS,
           "Portefeuille global" = PORTEFEUILLE_GLOBAL,
           "Crédits Retructurés" = CREDIT_RESTRUCTURES,
           "Portefeuille Douteux Crédits Directs" = PFT_DT_ENG_DIRECTS,
           "Portefeuille Douteux Eng. hors bilan" = PFT_DT_ENG_INDIRECTS,
           "Provisions" = PROVISIONS,
           "Créances restructurées" = CREDIT_RESTRUCTURES,
           "Créances en souffrance directes / Crédits directs" = CES_DIREC_SUR_CRED_DIREC,
           "Créances en souffrance / portefeuille global" = CES_SUR_GLOBAL
        ) %>%
  ungroup %>%
    column_to_rownames("DATE_COMPTA") %>%
    t 
    
}

# 
out <- holding.df %>%
  evolPortefeuil %>%
  as.data.frame

# Ajout des évolutions
out[, "Evol en (Val)"] = out[,3] - out[,2]
out[, "Evol en (%)"] = (out[,3] - out[,2]) / out[,3]
# %>%
write.csv(out, 
           "C:/Users/jaouedraogo/Documents/Coris/Reporting/Semestriels/situation2.csv",
          row.names = T)


###############################################################################
# 50 PLUS GROS ENGAGEMENTS ####################################################
###############################################################################


plusGrosEng <- function(df, arrete){
  
  df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(NOM) %>%
    summarise(
      TRESO = sum(-TRESORERIE) / 1e6,
      SIGNA = sum(-SIGNATURE) / 1e6,
      TOTAL = (sum(-TRESORERIE) + sum(-SIGNATURE)) / 1e6,
      TOTAL_ENG_Brut = sum(-TOTAL_ENG) / 1e6,
      Prov = paste(c(Pays), collapse = "-"),
      Date = paste(c(DATE_COMPTA), collapse = ";")
    ) %>%
    arrange(desc(TOTAL)) %>%
    top_n(n=50, wt=TOTAL) 
  
}

top50 <- holding.df %>%
  plusGrosEng("2021-06-30")

write.csv(top50, 
          "C:/Users/jaouedraogo/Documents/Coris/Reporting/Semestriels/top50.csv",
          row.names = T)



###############################################################################
# RESULTATS DE LA REVISION DU PORTEFEUILLE ####################################
###############################################################################

revision <- function(df, arrete){
  
  df %>%
    filter(DATE_COMPTA == arrete) %>%
    summarise(
      ENGAGEMENTS_DIRECTS = sum(-TRESORERIE),
      SAINS = sum()
      
      
      
      
      
      
      
      ENGAGEMENTS_INDIRECTS = sum(-SIGNATURE),
      PORTEFEUILLE_GLOBAL = sum(-TRESORERIE) + sum(-SIGNATURE),
      CREANCES_IMPAYES = sum(-IMPAYES),
      CREDIT_RESTRUCTURES = sum(-Restructures),
      PFT_DT_ENG_DIRECTS = sum(-DOUTEUX_292),
      PFT_DT_ENG_INDIRECTS = sum(-DOUTEUX_SIGNATURE),
      CREANCES_SOUFFRAN_DIRECTS = sum(-Restructures) + sum(-DOUTEUX_292),
      PROVISIONS = sum(ifelse(Restructures !=0, PROVISION, 0)) + 
        sum(-PROVISION_512) + 
        sum(ifelse(Restructures ==0, PROVISION, 0)),
      PROV_CREDS_RESTRUCTURES = sum(ifelse(Restructures !=0, PROVISION, 0)),
      PROV_DT_INDIRECTS = sum(PROVISION_512),
      PROV_CREANCES_DT_DIRECTS = sum(ifelse(Restructures ==0, PROVISION, 0)),
      CREANC_DIRECTS_SOUFFR_NETTES = (sum(-Restructures) + sum(-DOUTEUX_292)) - 
        sum(ifelse(Restructures !=0, PROVISION, 0)) - 
        sum(ifelse(Restructures ==0, PROVISION, 0)),
      VOL_TOP50 = sum(-TOTAL_ENG[1:50]),
      TAUX_CRED_RESTRUCTURE = CREDIT_RESTRUCTURES / ENGAGEMENTS_DIRECTS,
      TAUX_DOUTEUX_DIRECTS = PFT_DT_ENG_DIRECTS / ENGAGEMENTS_DIRECTS,
      TAUX_CES_DIRECTES = sum(PROV_CREDS_RESTRUCTURES) / CREDIT_RESTRUCTURES,
      TAUX_PROVISIONNEMENT = (PROV_CREANCES_DT_DIRECTS + PROV_CREDS_RESTRUCTURES) / 
        (PFT_DT_ENG_DIRECTS + CREDIT_RESTRUCTURES),
      TAUX_DEGRADATION = (PFT_DT_ENG_DIRECTS + CREDIT_RESTRUCTURES) / ENGAGEMENTS_DIRECTS,
      TAUX_VOL_TOP50_SUR_PFTGLOB = VOL_TOP50 / PORTEFEUILLE_GLOBAL,
      PRETS = ENGAGEMENTS_DIRECTS,
      DEPOTS = depots,
      RATIO_PRETS_DEPOTS = PRETS / DEPOTS
    ) %>%
    rename("Engagements Directs" = ENGAGEMENTS_DIRECTS,
           "Engagements Indirects" = ENGAGEMENTS_INDIRECTS,
           "Portefeuille global" = PORTEFEUILLE_GLOBAL,
           "Dont : - Créances Impayés" = CREANCES_IMPAYES,
           "       - Crédits Retructurés" = CREDIT_RESTRUCTURES,
           "       - Portefeuille Douteux Eng. Directs" = PFT_DT_ENG_DIRECTS,
           "       - Portefeuille Douteux Eng. Indirects" = PFT_DT_ENG_INDIRECTS,
           "       - Créances en souffrance directes" = CREANCES_SOUFFRAN_DIRECTS,
           "Provisions dont: " = PROVISIONS,
           "       - Crédits restructurés" = PROV_CREDS_RESTRUCTURES,
           "       - Douteux indirects" = PROV_DT_INDIRECTS,
           "       - Créances douteuses nettes" = PROV_CREANCES_DT_DIRECTS,
           "Créances directes en souffrance nettes" = CREANC_DIRECTS_SOUFFR_NETTES,
           "Volume des 50 plus gros crédits" = VOL_TOP50,
           "Prêts" = PRETS,
           "Dépôts" = DEPOTS,
           "Crédits restructurés/Engagements directs" = TAUX_CRED_RESTRUCTURE,
           "Douteux Eng. Dir./Engagements Directs" = TAUX_DOUTEUX_DIRECTS,
           "CES directes/Total Engagements" = TAUX_CES_DIRECTES,
           "Taux de provisionnement" = TAUX_PROVISIONNEMENT,
           "Taux de dégradation du portefeuille" = TAUX_DEGRADATION,
           "50 plus gros risques/Portefeuille global" = TAUX_VOL_TOP50_SUR_PFTGLOB, 
           "Ratio Prêts/Dépôts" = RATIO_PRETS_DEPOTS)%>%
    ungroup %>%
    gather(key = "Indicateurs", value = "Montant", -Pays)
  
}


revision.df <- holding.df %>%
  group_by(DATE_COMPTA) %>%
  summarise(
    ENGAGEMENTS_DIRECTS = sum(-TRESORERIE),
    SAINS = sum(-EFFET, -CT, -MT, -LT, -COMPTE1, -IMPAYES),
    CES = sum(-DOUTEUX_292, -Restructures),
    RESTRUCTURES = sum(-Restructures),
    CDL = sum(-DOUTEUX_292),
    PROV_CDL = sum(ifelse(Restructures ==0, PROVISION, 0)),
    PROV_RES = sum(ifelse(Restructures !=0, PROVISION, 0)),
    PROV_CDL2 = sum(-DOUTEUX_292 != 0, PROVISION, 0),
    PROV_CDL3 = sum(PROVISION_512) + sum(ifelse(Restructures ==0, PROVISION, 0)),
    PROVISIONs = sum(PROVISION),
    
    ENCOURS_BRUT_SIGNAT = sum(-SIGNATURE),
    ENCOURS_SAINS_SIGNAT = sum(-SIGNATURE) - sum(-DOUTEUX_SIGNATURE),
    ENCOURS_DOUTEUX_SIGNAT = sum(-DOUTEUX_SIGNATURE),
    PROVISIONS_SIGNAT = sum(PROVISION_512)
  ) %>%
  ungroup %>%
  column_to_rownames("DATE_COMPTA") %>% 
  t %>%
  as.data.frame


write.csv(revision.df, 
          "C:/Users/jaouedraogo/Documents/Coris/Reporting/Semestriels/revision2.csv",
          row.names = T)
