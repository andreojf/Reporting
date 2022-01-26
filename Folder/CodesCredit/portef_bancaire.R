##### QUALITE DU PORTEFEUILLE
library(data.table)
library(tidyverse)
library(dplyr)
library(plyr)
library(knitr)
library(gt)
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

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

pfFormatTable <- function(tab, country, params){
  
  tab %>%
    gt %>%
    fmt_number(
      columns = c(params$arrete, params$prev1Arrete, params$prev2Arrete),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = contains("evol"),
      decimals = 1
    ) %>%
    fmt_missing(
      columns = contains("evol"),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    ) %>% 
    text_transform(
      locations = cells_body(
        columns = evol1,
        rows = evol1 > 0),
      fn = function(x) paste(x, up_arrow)
    ) %>% 
    text_transform(
      locations = cells_body(
        columns = evol2,
        rows = evol2 > 0),
      fn = function(x) paste(x, up_arrow)
    ) %>%
    text_transform(
      locations = cells_body(
        columns = evol1,
        rows = evol1 < 0),
      fn = function(x) paste(x, down_arrow)
    ) %>%
    text_transform(
      locations = cells_body(
        columns = evol2,
        rows = evol2 < 0),
      fn = function(x) paste(x, down_arrow)
    ) %>%
    cols_label(
      evol1 = "% T vs T-1", evol2 = "% T vs T-2"
    )
}

pfAgrege <- function(df){
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
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f %>% 
      rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out)
  }
  names(out) <- NULL
  names(out) <- c("Intitule", format(dates_compta_top3))
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  
  #out <- formatTable(out)
  
  return(out)
}
final.df %>%
  pfAgrege %>%
  pfFormatTable(country = country, params = params)

pfAgregeGlobal <- function(df){
  
  dates_compta <- unique(df$DATE_COMPTA)
  dates_compta <- dates_compta[!is.na(dates_compta)]
  dates_compta <- sort(dates_compta, decreasing = TRUE)
  dates_compta_top3 <- dates_compta[1:3]
  dates_compta_top3 <- dates_compta_top3[!is.na(dates_compta_top3)] # dans le cas où il y a moins de 3 arretés.
  dates_compta_top3
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in dates_compta_top3){
    out <- df %>% 
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      #mutate(Prop = Montant / sum(Montant)) %>%
      rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out, by="Intitule")
  }
  
  names(out) <- NULL
  names(out) <- c("Intitule", format(dates_compta_top3))
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  
  #out <- formatTable(out)
  return(out)
}
final.df %>%
  pfAgregeGlobal %>%
  pfFormatTable(country = country, params = params)

####### EVOLUTION DES INDICATEURS

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
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  
  #out <- formatTable(out)
  return(out)
}
final.df %>%
  pfEvol("Benin") %>%
  pfFormatTable(country = country, params = params)

pfEvolGlobal <- function(df, pays){
  
  
  dates_compta <- unique(df$DATE_COMPTA)
  dates_compta <- dates_compta[!is.na(dates_compta)]
  dates_compta <- sort(dates_compta, decreasing = TRUE)
  dates_compta_top3 <- dates_compta[1:3]
  dates_compta_top3 <- dates_compta_top3[!is.na(dates_compta_top3)] # dans le cas où il y a moins de 3 arretés.
  dates_compta_top3
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in dates_compta_top3){
    out <- df %>% 
      filter(Pays == pays, DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      #mutate(Prop = Montant / sum(Montant)) %>%
      rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out, by="Intitule")
  }
  
  names(out) <- NULL
  names(out) <- c("Intitule", format(dates_compta_top3))
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  
  #out <- formatTable(out) 
  
  return(out)
}
final.df %>%
  pfEvolGlobal("Benin") %>%
  pfFormatTable(country = country, params = params)

res <- final.df %>%
  pfAgregeGlobal


#### story
storyPtf <- function(df){
  arrete = "2020-12-31"
  prevarrete = "2020-11-30"
  
  percSain = df[df$Intitule == "Sain",6]
  percRest = df[df$Intitule == "Restructure",6]
  percDtLt = df[df$Intitule == "Douteux et Litigieux",6]
  
  posnegSain = ifelse(percSain >0, "positive","negative")
  posnegRest = ifelse(percRest >0, "positive","negative")
  posnegDtLt = ifelse(percDtLt >0, "positive","negative")
  
    paste0("On constate une évolution ",posnegSain," (", 100 * round(percSain, 4),"%) ",
  "des montants du portefeuille sain cet arreté par rapport à l'arrêté précédent. 
  Sur le périmètre des restructurés, on observe une évolution ",
  posnegRest," (", 100 * round(percRest, 4),"%). Enfin sur les dossiers douteux et litigieux,
  l'évolution est ",posnegDtLt," (", 100 * round(percDtLt, 4),"%) ")
}

storyPtf(res)


