##### QUALITE DU PORTEFEUILLE
library(data.table)
library(tidyverse)
library(dplyr)
source("CodesCredit/preparation.R")


SituationDuPortefeuille <- function(data){
  
  porte_f <- matrix(nrow = 8, ncol = 2)
  porte_f[1,] = c("Encours des effets", abs(sum(data$EFFET)))
  porte_f[2,] = c("Credits Court terme", abs(sum(data$CT)))
  porte_f[3,] = c("Credit Moyen et Long terme", abs(sum(data$MT, data$LT)))
  porte_f[4,] = c("Compte débiteurs", abs(sum(data$COMPTE1)))
  porte_f[5,] = c("Impayes", abs(sum(data$IMPAYES)))
  
  # calcul des restructures
  data <- data %>%
    mutate(Restructures = TRESORERIE - DOUTEUX_292 - EFFET - COMPTE1 - CT - LT - MT - IMPAYES)
  porte_f[6,] = c("Restructures", abs(sum(data$Restructures)))
  
  # calcul des credits particuliers
  part <- data %>%
    filter(!SEGMENT %in% c("GRDE ENTREPRISE","PME-PMI")) %>%
    summarise(va = sum(DOUTEUX_292)) %>%
    pull
  porte_f[7,] = c("Credits Particuliers", abs(part))
  
  # calcul des credits entreprises
  entrp <- data %>%
    filter(SEGMENT %in% c("GRDE ENTREPRISE","PME-PMI")) %>%
    summarise(va = sum(DOUTEUX_292)) %>%
    pull
  porte_f[8,] = c("Credits Entreprises", abs(entrp))
  
  # labels de la table
  colnames(porte_f) <- c("Intitule", "Montant")
  porte_f <- as.data.frame(porte_f)
  porte_f$Montant <- as.numeric(porte_f$Montant)
  
  
  ##################### TOTAUX ##################
  porte_f_tot <- matrix(nrow=3, ncol=2)
  porte_f_tot[1,] <- c("Sain",sum(porte_f[1:5, 2]))
  porte_f_tot[2,] <- c("Restructure",sum(porte_f[6,2]))
  porte_f_tot[3,] <- c("Douteux et Litigieux",sum(porte_f[7:8,2]))
  colnames(porte_f_tot) <- c("Intitule", "Montant")
  porte_f_tot <- as.data.frame(porte_f_tot)
  porte_f_tot$Montant <- as.numeric(porte_f_tot$Montant)

  return(list(porte_f=porte_f, porte_f_tot=porte_f_tot))
}

#pf <- SituationDuPortefeuille(final.df)


################## APPLICATION A NOTRE TABLE ENTIERE 

## selectionner les pays
####################### situation agrégée #################################""
library(knitr)
pfAgrege <- function(df){
  
  pays <- unique(df$Pays)
  pays <- pays[!is.na(pays)]
  
  
  out <- data.frame(Intitule=c(
    "Encours des effets",
    "Credits Court terme",
    "Credit Moyen et Long terme",
    "Compte débiteurs",
    "Impayes",
    "Restructures",
    "Credits Particuliers",
    "Credits Entreprises"
  ))
  
  for (p in pays){
    out <- df %>% 
      filter(Pays == p) %>%
      SituationDuPortefeuille %>%
      .$porte_f %>%
      rename(c("Montant"=p)) %>%
      left_join(out)
  }
  return(out)
}

  

pfAgregeGlobal <- function(df){
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (p in pays){
    out <- df %>% 
      filter(Pays == p) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      mutate(Prop = 100 * round(Montant / sum(Montant), 4)) %>%
      rename(c("Montant"=p, "Prop"=paste("Prop",p))) %>%
      left_join(out, by="Intitule")
  }
  return(out)
}

####### EVOLUTION DES INDICATEURS
## selectionner les date de comptabilisation

pfEvol <- function(df, pays){
  
  dates_compta <- unique(df$DATE_COMPTA)
  dates_compta <- dates_compta[!is.na(dates_compta)]
  dates_compta <- sort(dates_compta, decreasing = TRUE)
  dates_compta_top3 <- dates_compta[1:3]
  dates_compta_top3 <- dates_compta_top3[!is.na(dates_compta_top3)] # dans le cas où il y a moins de 3 arretés.
  dates_compta_top3
  
  out <- data.frame(Intitule=c(
    "Encours des effets",
    "Credits Court terme",
    "Credit Moyen et Long terme",
    "Compte débiteurs",
    "Impayes",
    "Restructures",
    "Credits Particuliers",
    "Credits Entreprises"
  ))
  
  for (date_compta in dates_compta_top3){
    out <- df %>% 
      filter(Pays == pays, DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f %>% 
      rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out)
  }
  names(out) <- NULL
  names(out) <- c("Intitule", format(dates_compta_top3))
  return(out)
}

pfEvolGlobal <- function(df, pays){
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in dates_compta_top3){
    out <- df %>% 
      filter(Pays == pays, DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      mutate(Prop = 100 * round(Montant / sum(Montant), 4)) %>%
      #rename(c("Montant"=p, "Prop"=paste("Prop",p))) %>%
      left_join(pfAllTot, by="Intitule")
  }
  
  names(out) <- NULL
  names(out) <- c("Intitule", format(dates_compta_top3))
}

