################################ CHANGEMENT PAR RAPPORT A LA VERSION PRECEDENTE #########################
##### : changement de la colonne %T vs T-1 en % M vs M-1 ####
##### : retrait du suffixe M dans le format des colonnes numériques ####################### 







##### QUALITE DU PORTEFEUILLE
library(data.table)
library(tidyverse)
library(dplyr)
library(plyr)
library(knitr)
library(gt)
library(lubridate)
library(flextable)
library(gridExtra)
#source("CodesCredit/preparation.R")

convertDate <- function(x){
  
  if (!(is.Date(x))){
    x = as.Date(x)
  }
  jj = day(x)
  mm = month(x)
  yyyy = year(x)
  
  paste(yyyy,mm,jj,  sep = "-")
}
#class(convertDate(d))


###################################################################################################################
################################## FUNCTION PRODUCTION DES TABLEAUX DE BASES ######################################
###################################################################################################################


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

###################################################################################################################
###################################################################################################################
################################# CODES POUR LE FORMATTING ########################################################
###################################################################################################################

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

pfFormatTablePays <- function(tab, periods){
  
  tab %>%
    gt %>%
    fmt_number(
      columns = c(sapply(periods, convertDate) %>% unname, evo1M, evo2M),
      scale_by = 1 / 1E6,
      #pattern = "{x} M",
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
      evol1 = "M vs M-6 en %", evol2 = "M vs M-12 en %",
      evo1M = "M vs M-6.", evo2M = "M vs M-12"
    )
}


###################################################################################################################
################################# CODES POUR PRODUCTION TEMPLATE PAR PAYS #########################################
###################################################################################################################

####### Situation du portefeuille sur les arretes choisis

pfEvolPays <- function(df, periods){
  
  periods = sort(periods, decreasing = TRUE)
  
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
  
  for (date_compta in periods){
    out <- df %>% 
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f %>% 
      rename(c("Montant"= convertDate(date_compta))) %>%
      left_join(out, by="Intitule")
  }
  
  #print(out)
  # collabels = sapply(periods, convertDate)
  # #print(collabels)
  # names(out) <- NULL
  # names(out) <- c("Intitule", collabels)
  out$evo1M = out[,4] - out[,3]
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evo2M = out[,4] - out[,2]
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  #print(out)
  return(out)
}


# brut.df %>%
#   pfEvolPays(periods = c(
#            "2020-12-31",
#            "2021-01-31",
#            "2020-10-31"
#          )) %>%
#   pfFormatTable(periods = c(
#                   "2020-12-31",
#                   "2021-01-31",
#                   "2020-10-31"
#                 ))

###### courbe d'évolution du portefeuille
library(scales)

# out <- brut.df %>%
#   pfEvolPays(periods = c(
#     "2021-02-28",
#     "2021-01-31",
#     "2020-10-31"
#   )) %>% 
#   select(-evol1, -evol2) %>%
#   gather(key = "periods", value = "Montant", -Intitule) %>%
#   filter(Intitule %in% c("Encours des effets","Restructures"))

graphicEvol <- function(df){
  
  p <- ggplot(df, aes(periods, Montant, group=Intitule))
  p + geom_line(aes(color=Intitule)) +
    geom_point(aes(color=Intitule)) + 
    theme_minimal(base_size = 12, base_family = "Times") + 
    scale_fill_brewer(palette="Spectral") +
    scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
    labs(
      x = "Periodes",
      y = " ",
      title = "Evolution des indicateurs sur les 12 derniers mois",
      subtitle = "Chiffres en Millions",
      color="Indicateurs")
  
}


# graphicEvol(out)

pfEvolGlobalPays <- function(df, periods){
  
  
  periods = sort(periods, decreasing = FALSE)
  
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in periods){
    out <- df %>% 
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      #mutate(Prop = Montant / sum(Montant)) %>%
      #rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out, by="Intitule")
  }
  
  names(out) <- NULL
  collabels = sapply(periods, convertDate)
  names(out) <- c("Intitule", collabels)
  out$evo1M = (out[,4] - out[,3]) 
  out$evol1 = (out[,4] - out[,3]) / out[,3]
  out$evo2M = (out[,4] - out[,2]) 
  out$evol2 = (out[,4] - out[,2]) / out[,2]
  
  #out <- formatTable(out) 
  
  return(out)
}

#### Graphique: évolution sur les 12 mois à compter du mois de décembre de l'année en cours
getDataDC <- function(df, arrete){
  
  names(df)[names(df) == arrete] <- "Value"
  
  df <- df %>%
    mutate(qualite = c(rep("Sains", 5), "Restructurés",rep("Douteux ou litigieux", 2))) %>%
    select(Value, qualite, Intitule) %>%
    group_by(qualite) %>%
    mutate(prop = 100 * round(Value / sum(Value), 4))
  
  mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#00AFBB")
  
  dcSains <- df %>%
    filter(qualite == "Sains") %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    arrange(desc(prop)) %>%
    ggplot(aes(x = 2, y = prop, fill = Intitule)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    scale_fill_manual(values = mycols) +
    theme_minimal()
  
  
  dcDL <- df %>%
    filter(qualite == "Douteux ou litigieux") %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    arrange(desc(prop)) %>%
    ggplot(aes(x = 2, y = prop, fill = Intitule)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    scale_fill_manual(values = mycols) +
    theme_minimal()
  
  
  #library(gridExtra)
  grid.arrange(dcSains, dcDL, 
               ncol=2, nrow =1)
}

### evolution sur les 12 mois des restructurés sains etc...
graphEvol12 <- function(df){
  
  p <- df %>%
    select(-evol1, -evol2, -evo1M, -evo2M) %>%
    gather(key = "Date", value="Value", -Intitule) %>%
    mutate(Value = round(Value / 1e6),
           Date = as.Date(Date))
  
  p %>% ggplot(aes(x = Date, y = Value)) +
    geom_col(aes(fill = Intitule)) +
    scale_x_date(breaks = sort(unique(p$Date)),
                 date_labels = "%m-%Y") +
    scale_fill_manual(values = c("#CC3300", "#E7B800", "#009933")) +
    geom_text(aes(label = Value, y = Value + 0.05),
              position = position_stack(vjust = 0.5),
              size = 3) +
    facet_wrap(~Intitule, scales = "free_y", ncol = 1, ) +
    theme_minimal(base_size = 12) +
    labs(title = "Evolution du portefeuille") +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
}

  



###################################################################################################################
###################################################################################################################


###################################################################################################################
################################# CODES POUR PRODUCTION TEMPLATE PAR POUR LA HOLDING###############################
###################################################################################################################


###################################################################################################################
###################################################################################################################

pfAgrege <- function(df, periods){
  
  periods = sort(periods, decreasing = FALSE)
  
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
  
  for (date_compta in periods){
    out <- df %>% 
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f %>% 
      #rename(c("Montant"=convertDate(date_compta))) %>%
      left_join(out, by="Intitule")
  }
  names(out) <- NULL
  collabels = sapply(periods, convertDate)
  names(out) <- c("Intitule", collabels)
  ncolout = ncol(out)
  #print(ncolout - 2)
  out$evo1 = (out[,ncolout] - out[,(ncolout-1)])
  out$evol1 = (out[,ncolout] - out[,(ncolout-1)]) / out[,(ncolout-1)]
  out$evo2 = (out[,ncolout] - out[,(ncolout-2)]) 
  out$evol2 = (out[,ncolout] - out[,(ncolout-2)]) / out[,(ncolout-2)]
  
  #out <- formatTable(out)
  
  return(out)
}


# 
# final.df %>%
#   pfAgrege(periods = c(
#     "2020-12-31",
#     "2020-11-30",
#     "2020-10-31"
#   )) %>%
# pfFormatTable(country = country, params = params)
  

pfAgregeGlobal <- function(df, periods){
  
  periods = sort(periods, decreasing = FALSE)
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in periods){
    out <- df %>% 
      filter(DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      #mutate(Prop = Montant / sum(Montant)) %>%
      #rename(c("Montant"=as.character(format(date_compta)))) %>%
      left_join(out, by="Intitule")
  }
  
  names(out) <- NULL
  collabels = sapply(periods, convertDate)
  names(out) <- c("Intitule", collabels)
  out$evol1 = (out[,ncolout] - out[,(ncolout-1)]) / out[,(ncolout-1)]
  out$evol2 = (out[,ncolout] - out[,(ncolout-2)]) / out[,(ncolout-2)]
  
  #out <- formatTable(out)
  return(out)
}
# final.df %>%
#   pfAgregeGlobal(
#     periods = c(
#       "2020-12-31",
#       "2020-06-30",
#       "2019-12-31"
#     )
#   ) %>%
#   pfFormatTable(country = country, params = params)


  
  

# final.df %>%
#   pfEvolGlobal("Benin", periods = c(
#                   "2020-12-31",
#                   "2020-11-30",
#                   "2020-10-31"
#                   ) ) %>%
#   pfFormatTable(country = country, params = params)

# res <- final.df %>%
#   pfAgregeGlobal


#### story
storyPtf <- function(df){
  arrete = "2020-12-31"
  prevarrete = "2020-11-30"
  
  percSain = df[df$Intitule == "Sain",5]
  percRest = df[df$Intitule == "Restructure",5]
  percDtLt = df[df$Intitule == "Douteux et Litigieux",5]
  
  posnegSain = ifelse(percSain >0, "positive","negative")
  posnegRest = ifelse(percRest >0, "positive","negative")
  posnegDtLt = ifelse(percDtLt >0, "positive","negative")
  
    paste0("On constate une évolution ",posnegSain," (", 100 * round(percSain, 4),"%) ",
  "des montants du portefeuille sain cet arreté par rapport à l'arrêté précédent. 
  Sur le périmètre des restructurés, on observe une évolution ",
  posnegRest," (", 100 * round(percRest, 4),"%). Enfin sur les dossiers douteux et litigieux,
  l'évolution est ",posnegDtLt," (", 100 * round(percDtLt, 4),"%) ")
}

# storyPtf(res)


# detacher le package plyr
# detach("package:plyr", unload = TRUE)
