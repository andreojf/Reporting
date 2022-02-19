# ### CALCUL par Notation
# library(dplyr)
# #source("preparation.R")
# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE) 
#   detach("package:plyr", unload=TRUE) 
# } 
# library(plyr)
# if ("package:plyr" %in% search()){
#   detach("package:plyr", unload=TRUE) 
# }
library(dplyr)
library(gt)
library(lubridate)

########################################################################
########################### IMPUTS A DEFINIR ########################### 
########################################################################

#d = as.Date("2020-12-31")



# notationParPays <- function(df, pays){
#   
#   out <- df %>%
#     filter(Pays == pays) %>%
#     group_by(NOTATION) %>%
#     summarise(
#       N = length(TRESORERIE),
#       TRESO = abs(sum(TRESORERIE)),
#       SIGNAT = abs(sum(SIGNATURE)),
#       TOTAL = abs(TRESO + SIGNAT))
#   out$Poids <- out$TOTAL / sum(out$TOTAL)
#   out[nrow(out) + 1,] = list("Total",
#                                sum(out$N),
#                                sum(out$TRESO),
#                                sum(out$SIGNAT),
#                                sum(out$TOTAL),
#                                sum(out$Poids))  
#   
#   return(out)
# }


NotationFormatTable <- function(tab){
  
  tab %>%
    gt %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = Poids,
      decimals = 1
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, Poids),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    ) %>%
    cols_label(
      TRESO = "Trésorerie", SIGNAT = "Signature"
    )
}

# final.df %>%
#   notationParPays(pays="Benin") %>%
#   NotationFormatTable




###################################################################################################################
################################# CODES POUR PRODUCTION TEMPLATE PAR PAYS #########################################
###################################################################################################################

# Répartition du portefeuille de chaque pays suivant les classes de risques
notationParPays <- function(df, arrete){
  
  Notation.df <- df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(Pays, NOTATION) %>%
    summarise(
      N = length(TRESORERIE),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(TRESO + SIGNAT),
      CDL = sum(-DOUTEUX_292),
      RESTRUCTURE = sum(-Restructures),
      CES = sum(CDL + RESTRUCTURE),
      TXDEGRADATION = CES / TRESO
      ) %>%
    group_by(Pays) %>%
    mutate(Poids = TRESO / sum(TRESO)) %>%
    ungroup 
  
  
  notation.df.format <- Notation.df %>%
    gt(
      rowname_col = "NOTATION",
      groupname_col = "Pays"
    ) %>%
    fmt_percent(
      columns = c(TXDEGRADATION, Poids),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
      scale_by = 1 / 1E6,
      #pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      groups = TRUE,
      columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    summary_rows(
      groups = TRUE,
      columns = N,
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      #use_sep = TRUE,
      decimals = 0
    ) %>%
    summary_rows(
      groups = TRUE,
      columns = Poids,
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      TRESO = "Trésorerie", SIGNAT = "Signature", RESTRUCTURE = "Restructurés",
      CDL = "Créances Dout. et Lit.", TXDEGRADATION = "Taux de dégradation"
    )
  
  return(list(tab=Notation.df, tabFormat=notation.df.format))
}


###### Matrice de transition de N-1 vers N
# L'objectif de cette analyse est d'analyser les differentes migrations qui ont eu lieu entre deux arrêtés.
# La banque pourra par conséquent sensibiliser les gestionnaires en cas de dégradation significative.

# transition 
transNotation <- function(df, pays, arreteDeb, arreteFin){

  tab <- df %>%
    filter(DATE_COMPTA == arreteDeb) %>%
    select(CLIENT, NOTATION, TRESORERIE) %>% # trésorerie en année N-1
    left_join(
      df %>%
        filter(DATE_COMPTA == arreteFin) %>%
        select(CLIENT, NOTATION)
      , by=c("CLIENT")) %>%
    #rename(NOTATIONAVANT = NOTATION.x, NOTATIONAPRES = NOTATION.y)
    group_by(NOTATION.x, NOTATION.y) %>%
    summarise(N = n(),
              Encours = sum(-TRESORERIE)) %>%
    ungroup
  
  # Gestion des sorties de portefeuille
  tab$NOTATION.y[is.na(tab$NOTATION.y)] <- "Sortie"
  
  # # matrice de transition : Nombre
  tabN <- tab %>%
    select(-Encours) %>%
    spread(key = NOTATION.y, value = N)

  # Matrice de transition : Encours
  tabEnc <- tab %>%
    select(-N) %>%
    spread(key = NOTATION.y, value = Encours)
  
  # si la colonne "Sortie existante"
  if (!("Sortie" %in% names(tabN))){
    tabN$Sortie <- NA
  }
  
  if (!("Sortie" %in% names(tabEnc))){
    tabEnc$Sortie <- NA
  }
  
  # remplacer les valeurs manquantes par 0
  tabN[is.na(tabN)] <- 0
  tabEnc[is.na(tabEnc)] <- 0
  
  # Calcul des taux par lignes
  tabN$TOTAL = rowSums(tabN[2:ncol(tabN)])
  tabEnc$TOTAL = rowSums(tabEnc[2:ncol(tabEnc)])
  
  # EVALUATION DES NIVEAUX DE DEGRADATION, AMELIORATION ET STABILITE : Nombre
  
  ## Extraction de la matrice sans la colonne sortie
  m = tabN[2:(ncol(tabN)-1)]
  
  tabNASD = matrix(nrow = length(unique(df$NOTATION)),
                  ncol = 7, byrow = TRUE) # ASD : Amélioration, Stabilité et Dégradation
  #tabNASD[,1] = unique(df$NOTATION)         # colonne de Notation
  tabNASD[,2] = rowSums(m * upper.tri(m))   # Degradation en nombre
  tabNASD[,3] = rowSums(m * upper.tri(m)) / tabN$TOTAL
  tabNASD[,4] = m[row(m)==col(m)]           # Stabilité en nombre
  tabNASD[,5] = m[row(m)==col(m)] / tabN$TOTAL
  tabNASD[,6] = rowSums(m * lower.tri(m))   # Amélioration en nombre
  tabNASD[,7] = rowSums(m * lower.tri(m)) / tabN$TOTAL
  
  # gestion des entetes
  colnames(tabNASD) <-  c("NOTATION","Degradation", "% Deg.",
                          "Stabilite", "% Stab.",
                          "Amélioration", "% Amélioration")
  
  tabNASD <- as.data.frame(tabNASD)
  tabNASD[,1] = unique(df$NOTATION) 
  
  # EVALUATION DES NIVEAUX DE DEGRADATION, AMELIORATION ET STABILITE : ENCOURS
  
  ## Extraction de la matrice sans la colonne sortie
  m = tabEnc[2:(ncol(tabEnc)-1)]
  
  tabEncASD = matrix(nrow = length(unique(df$NOTATION)),
                   ncol = 7, byrow = TRUE) # ASD : Amélioration, Stabilité et Dégradation
  #tabEncASD[,1] = unique(df$NOTATION)         # colonne de Notation
  tabEncASD[,2] = rowSums(m * upper.tri(m))   # Degradation en nombre
  tabEncASD[,3] = rowSums(m * upper.tri(m)) / tabEnc$TOTAL
  tabEncASD[,4] = m[row(m)==col(m)]           # Stabilité en nombre
  tabEncASD[,5] = m[row(m)==col(m)] / tabEnc$TOTAL
  tabEncASD[,6] = rowSums(m * lower.tri(m))   # Amélioration en nombre
  tabEncASD[,7] = rowSums(m * lower.tri(m)) / tabEnc$TOTAL
  
  # gestion des entetes
  colnames(tabEncASD) <-  c("NOTATION","Degradation", "% Deg.",
                          "Stabilite", "% Stab.",
                          "Amélioration", "% Amélioration")
  
  tabEncASD <- as.data.frame(tabEncASD)
  tabEncASD[,1] = unique(df$NOTATION)
  
  # tabN$TOTAL = rowSums(tabN[2:ncol(tabN)])
  # tabN$Degradation = rowSums(m * upper.tri(m))
  # tabN$Stabilite = m[row(m)==col(m)]
  # tabN$Amelioration = rowSums(m * lower.tri(m))


  # tabProp <- tab
  # tabProp[,2:(ncol(tabProp)-3)] <- tab[,2:(ncol(tab)-3)] / tab$TOTAL
  # tabProp[,(ncol(tab)-2):ncol(tab)] <- tab[,(ncol(tab)-2):ncol(tab)] / tab$TOTAL
  # tabProp[is.na(tabProp)] <- 0

  # names(tab)[1] <- "NOTATION"
  # tabN[is.na(tabN)] <- 0
  # tabN
  # m = tab[2:(ncol(tab)-1)]
  # tab$TOTAL = rowSums(tab[2:ncol(tab)], na.rm = TRUE)
  # tab$Degradation = rowSums(m * upper.tri(m))
  # tab$Stabilite = m[row(m)==col(m)]
  # tab$Amelioration = rowSums(m * lower.tri(m))
  # 
  # 
  # tabProp <- tab
  # tabProp[,2:(ncol(tabProp)-3)] <- tab[,2:(ncol(tab)-3)] / tab$TOTAL
  # tabProp[,(ncol(tab)-2):ncol(tab)] <- tab[,(ncol(tab)-2):ncol(tab)] / tab$TOTAL
  # tabProp[is.na(tabProp)] <- 0
  # 
  # 
  # tabPropGt <- tabProp %>%
  #   gt(
  #     rowname_col = "NOTATION"
  #   ) %>%
  #   fmt_percent(
  #     columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
  #     decimals = 2
  #   ) %>%
  #   fmt_missing(
  #     columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
  #     missing_text = 0
  #   ) %>%
  #   tab_options(
  #     #table.width = pct(100),
  #     table.font.size = px(13)
  #   )  %>% tab_spanner(
  #     label = as.character(arreteFin),
  #     columns = c(contains("0"),"Sortie")
  #   ) %>%
  #   tab_row_group(
  #     label = as.character(arreteDeb),
  #     rows = contains("0")
  #   )

  return(list(tabN=tabN,
              tabEncours=tabEnc,
              tabNASD=tabNASD,
              tabEncoursASD=tabEncASD))
  #, transitionProp = tabProp, transitionPropGt = tabPropGt))
}


#### Gestion des formats
# format transition
formatTabTransition <- function(df, arreteDeb, arreteFin, encours){
  
  out <- df %>%
    gt %>%
    cols_label(
      NOTATION.x = "NOTATION"
    ) %>%
    tab_options(
      table.font.size = px(13)
    )  %>% tab_spanner(
      label = as.character(arreteFin),
      columns = c(contains("0"),"Sortie", "TOTAL")
    ) %>%
    tab_row_group(
      label = as.character(arreteDeb),
      rows = contains("0")
    )
  
  if (encours){
    out <- out %>% fmt_number(
      columns = c(contains("0"),"Sortie", "TOTAL"),
      scale_by = 1 / 1E6,
      #pattern = "{x} M",
      decimals = 0
    )
  }
  
  out
  
}

# format pour les ASD
formatTabASD <- function(df, encours){
  
  out <- df %>%
    gt %>%
    tab_options(
      table.font.size = px(13)
    )  %>% 
    fmt_percent(
      columns = contains("%"),
      decimals = 2
    ) 
  
  
  if (encours){
    out <- out %>% fmt_number(
      columns = c("Degradation","Stabilite","Amélioration"),
      scale_by = 1 / 1E6,
      #pattern = "{x}",
      decimals = 0
    )
  } 

  
  out
  
}




##### d'ou proviennent les nouveaux entrants ?
FocusNouvProd <- function(df, arreteDeb, arreteFin){
  
  ## contrats sur le dernier arrete
  clientsFin <- df %>%
    filter(DATE_COMPTA == arreteFin) %>%
    pull(CLIENT)
  
  
  clientsDeb <- df %>%
    filter(DATE_COMPTA == arreteDeb) %>%
    pull(CLIENT)
  
  
  # clients dans "Fin" et pas dans "Deb"
  NouvClients <- setdiff(clientsFin, clientsDeb)
  
  df %>%
    filter(DATE_COMPTA == arreteFin, 
           CLIENT %in% NouvClients) %>%
    group_by(NOTATION) %>%
    summarise(
      N = length(TRESORERIE),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(TRESO + SIGNAT),
      CDL = sum(-DOUTEUX_292),
      RESTRUCTURE = sum(-Restructures),
      CES = sum(CDL + RESTRUCTURE),
      TXDEGRADATION = CES / TRESO
    ) %>% 
    ungroup %>%
    gt %>%
      fmt_percent(
        columns = c(TXDEGRADATION),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
        scale_by = 1 / 1E3,
        #pattern = "{x} M",
        decimals = 0
      ) %>%
      fmt_missing(
        columns = c(N, TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
        missing_text = ""
      ) %>%
      tab_options(
        #table.width = pct(80),
        table.font.size = px(13)
       ) %>%
      # summary_rows(
      #   groups = TRUE,
      #   columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
      #   fns = list(TOTAL = "sum"),
      #   formatter = fmt_number,
      #   scale_by = 1 / 1E6,
      #   pattern = "{x} M",
      #   decimals = 0
      # ) %>%
      # summary_rows(
      #   groups = TRUE,
      #   columns = N,
      #   fns = list(TOTAL = "sum"),
      #   formatter = fmt_number,
      #   #use_sep = TRUE,
      #   decimals = 0
      # ) %>%
    # %>%
    #   summary_rows(
    #     groups = TRUE,
    #     columns = Poids,
    #     fns = list(TOTAL = "sum"),
    #     formatter = fmt_percent,
    #   )
      cols_label(
        TRESO = "Trésorerie", SIGNAT = "Signature", RESTRUCTURE = "Restructurés",
        CDL = "Créances Dout. et Lit.", TXDEGRADATION = "Taux de dégradation"
      )
}

# report.df %>%
#   FocusNouvProd(arreteFin = "2021-12-31",
#                 arreteDeb = "2021-06-30")

# Croisement avec le secteur d'activité
## Objectif: Comprendre ou se trouve les clients risqués mais aussi d'où proviennent la nouvelle production
## Nous allons selectionner le top 5 des secteurs par classes de risques
NOTATIONPARSECTEUR <- function(df, arrete, n){
  
  # df %>%
  #   filter(DATE_COMPTA==arrete) %>%
  #   group_by(NOTATION, SECTEURBCEAO) %>%
  #   summarise(
  #     TRESO = abs(sum(TRESORERIE))
  #   ) %>% 
  #   spread(key="NOTATION", value = "TRESO")
  
  for (NOTATION in unique(df$NOTATION)) {
    
    out <- df %>%
      filter(DATE_COMPTA == arrete, NOTATION == NOTATION) %>%
      group_by(NOTATION, SECTEURBCEAO) %>%
      summarise(
        N = length(TRESORERIE),
        TRESO = abs(sum(TRESORERIE)),
        SIGNAT = abs(sum(SIGNATURE)),
        TOTAL = abs(TRESO + SIGNAT),
        CDL = sum(-DOUTEUX_292),
        RESTRUCTURE = sum(-Restructures),
        CES = sum(CDL + RESTRUCTURE),
        TXDEGRADATION = CES / TRESO,
      ) %>%
      arrange(desc(TOTAL)) %>%
      top_n(n, wt=TOTAL)
  }
  
  out %>%
    arrange(NOTATION)
}

# out2 <- report.df %>%
#   NOTATIONPARSECTEUR("2021-12-31", 5)
  

#### plotting

plotRiskSecteur <- function(df){
  df %>% 
    #filter(NOTATION == "01 BR-BON RISQUE") %>%
    ggplot(aes(x = reorder(SECTEURBCEAO, -TOTAL), y = TOTAL)) +
    geom_bar(stat="identity", fill="#E7B800") +
    geom_text(aes(label = TOTAL, y = TOTAL + 0.05),
              position = position_stack(vjust = 0.5),
              size = 3) +
    facet_wrap(~NOTATION, scales = "free", ncol = 1) + 
    theme_minimal(base_size = 12) +
    labs(title = "Evolution du portefeuille") +
    theme(legend.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
}


# out2 %>% 
#   plotRiskSecteur


# transTab$transitionPropGt %>%
#   tab_header(
#     title = md(paste("**Matrice de transition de ", params$pays, "**", sep = "")),
#     subtitle = paste("entre", params$prev1Arrete ,"et",params$arrete)
#   )

####################################################################################################
####################################################################################################
####################################################################################################

# report.df %>%
#   notationParPays2("2020-12-31")


#### Vision consolidée

# notationConsolidee <- function(df, arrete){
#   
#   df %>%
#     filter(DATE_COMPTA == arrete) %>%
#     group_by(NOTATION) %>%
#     summarise(
#       N = length(TRESORERIE),
#       TRESO = abs(sum(TRESORERIE)),
#       SIGNAT = abs(sum(SIGNATURE)),
#       TOTAL = abs(TRESO + SIGNAT),
#       CDL = sum(-DOUTEUX_292),
#       RESTRUCTURE = sum(-Restructures),
#       CES = sum(CDL + RESTRUCTURE),
#       TXDEGRADATION = CES / TRESO) %>%
#     mutate(Poids = TRESO / sum(TRESO)) %>%
#     gt(
#       rowname_col = "NOTATION"
#     ) %>%
#     fmt_percent(
#       columns = c(Poids, TXDEGRADATION),
#       decimals = 1
#     ) %>%
#     fmt_number(
#       columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
#       scale_by = 1 / 1E6,
#       pattern = "{x} M",
#       decimals = 0
#     ) %>%
#     fmt_missing(
#       columns = c(N, TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES, TXDEGRADATION, Poids),
#       missing_text = ""
#     ) %>%
#     tab_options(
#       #table.width = pct(80),
#       table.font.size = px(13)
#     )  %>%
#     summary_rows(
#       #groups = TRUE,
#       columns = c(TRESO, SIGNAT, TOTAL, CDL, RESTRUCTURE, CES),
#       fns = list(TOTAL = "sum"),
#       formatter = fmt_number,
#       scale_by = 1 / 1E6,
#       pattern = "{x} M",
#       decimals = 0
#     ) %>%
#     summary_rows(
#       #groups = TRUE,
#       columns = N,
#       fns = list(TOTAL = "sum"),
#       formatter = fmt_number,
#       #use_sep = TRUE,
#       decimals = 0
#     ) %>%
#     summary_rows(
#       #groups = TRUE,
#       columns = Poids,
#       fns = list(TOTAL = "sum"),
#       formatter = fmt_percent,
#     ) %>%
#     cols_label(
#       TRESO = "Trésorerie", SIGNAT = "Signature", RESTRUCTURE = "Restructurés", 
#       CDL = "Créances Dout. et Lit.", TXDEGRADATION = "Taux de dégradation"
#     ) 
#   
#   
# }
# # 
# # final.df %>%
# #   notationConsolidee(arrete = d)
# 
# 
# # 
# # transTab <- final.df %>%
# #   transNotation("Benin", d, 2)
# 
# #transTab$transitionProp
# 
# 
# storyNotation <- function(tabProp, seuilDeg=0.1, seuilStab=0.95, seuilAme=0.1){
#   
#   # appreciation de la dégradation
#   riskclasses = tabProp$NOTATION
#   riskclassdeg = riskclasses[tabProp$Degradation >= seuilDeg]
#   if (is_empty(riskclassdeg)){
#     c1 = "Il n'y a pas de dégradation significative."
#   } else {
#     c1 = paste0("Les classes ",paste0(riskclassdeg, collapse = ", "), " se sont dégradés (>=",seuilDeg*100,"%)")
#   }
#   
#   
#   # appreciation de la stabilité
#   riskclassstab = riskclasses[tabProp$Stabilite >= seuilStab]
#   if (is_empty(riskclassstab)){
#     c2 = "Aucune classe n'est restée stable entre les deux arrêtés."
#   } else {
#     c2 = paste0("Les classes ",paste0(riskclassstab, collapse = ", "), " se sont restés stable (>=",seuilStab*100,"%).")
#   }
#   
#   # appreciation de l'amélioration
#   riskclassame = riskclasses[tabProp$Amelioration >= seuilAme]
#   if (is_empty(riskclassame)){
#     c3 = paste0("On observe aucune une amélioration au seuil de ", seuilAme*100,"%.")
#   } else {
#     c3 = paste0("Les classes ",paste0(riskclassame, collapse = ", "), " se sont ameliorées (>=",seuilAme*100,"%) entre les deux arrêtés.")
#   }
#   
#   # de facon générale
#   m = tabProp[2:(ncol(tabProp)-5)]
#   #print(m)
#   classMig <- c()
#   ans <- c()
#   rownames(m) = colnames(m)
#   m = as.matrix(m)
#   for (i in 1:nrow(m)){
#     for (j in 1:ncol(m)) {
#       if (i!=j){
#         if (m[i,j] >= 0.1){
#           classMig <- c(classMig, rownames(m)[i])
#           ans <- paste(ans, paste(100*m[i,j], "% des dossiers",rownames(m)[i],
#                                   "se retrouvent en",colnames(m)[j]),".")
#         }
#       }
#     }
#   }
#   
#   if (is.null(ans)){
#     c0 = paste("Aucune observation n'est faite sur les transitions")
#   } else {
#     c0 = paste("On note une forte ", sample(c("migration","transition"),1), " des dossiers classés en",
#                paste(unique(classMig),collapse=","),"sur l'arrêté précédent. En effet,", ans)
#   }
#   paste(c0, c1, c2, c3)
#   
# }

#storyNotation(tabProp = transTab$transitionProp)


##### croisement segmentation BCEAO vs NOTATION
# tab <- final.df %>%
#   filter(Pays == "Benin", DATE_COMPTA == "2020-12-31") %>%
#   group_by(NOTATION, SECTEURBCEAO) %>%
#   summarise(
#     CES = - sum(DOUTEUX_292 + Restructures),
#     TxDegradation = 100 * round(CES / sum(-TRESORERIE), 5)
#          ) %>%
#   select(NOTATION, SECTEURBCEAO, TxDegradation) %>%
#   spread(key = NOTATION, value = TxDegradation) %>%
#   left_join(
#     final.df %>%
#       filter(Pays == "Benin", DATE_COMPTA == "2020-12-31") %>%
#       group_by(SECTEURBCEAO) %>%
#       summarise(
#         CES = - sum(DOUTEUX_292 + Restructures),
#         TxDegradation = 100 * round(CES / sum(-TRESORERIE), 5)
#       ) %>%
#       select(SECTEURBCEAO, TxDegradation),
#     
#     by = "SECTEURBCEAO"
#     
#   )

