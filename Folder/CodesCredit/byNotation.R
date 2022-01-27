# ### CALCUL par Notation
# library(dplyr)
# #source("preparation.R")
# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE) 
#   detach("package:plyr", unload=TRUE) 
# } 
# library(plyr)
# detach("package:plyr", unload=TRUE) 
library(dplyr)
library(gt)
library(lubridate)

########################################################################
########################### IMPUTS A DEFINIR ########################### 
########################################################################

d = as.Date("2020-12-31")



notationParPays <- function(df, pays){
  
  out <- df %>%
    filter(Pays == pays) %>%
    group_by(NOTATION) %>%
    summarise(
      N = length(TRESORERIE),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(TRESO + SIGNAT))
  out$Poids <- out$TOTAL / sum(out$TOTAL)
  out[nrow(out) + 1,] = list("Total",
                               sum(out$N),
                               sum(out$TRESO),
                               sum(out$SIGNAT),
                               sum(out$TOTAL),
                               sum(out$Poids))  
  
  return(out)
}


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

# Vision par pays
notationParPays2 <- function(df, arrete){
  
  df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(Pays, NOTATION) %>%
    summarise(
      N = length(TRESORERIE),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(TRESO + SIGNAT)) %>%
    group_by(Pays) %>%
    ungroup %>%
    mutate(Poids = TRESO / sum(TRESO)) %>%
    gt(
      rowname_col = "NOTATION",
      groupname_col = "Pays"
    ) %>% 
    fmt_percent(
      columns = Poids,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      groups = TRUE,
      columns = c(TRESO, SIGNAT, TOTAL),
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
      TRESO = "Trésorerie", SIGNAT = "Signature"
    )
}


final.df %>%
  notationParPays2("2020-12-31")


#### Vision consolidée

notationConsolidee <- function(df, arrete){
  
  df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(NOTATION) %>%
    summarise(
      N = length(TRESORERIE),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(TRESO + SIGNAT)) %>%
    mutate(Poids = TRESO / sum(TRESO)) %>%
    gt(
      rowname_col = "NOTATION"
    ) %>%
    fmt_percent(
      columns = Poids,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, Poids),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      #groups = TRUE,
      columns = c(TRESO, SIGNAT, TOTAL),
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    summary_rows(
      #groups = TRUE,
      columns = N,
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      #use_sep = TRUE,
      decimals = 0
    ) %>%
    summary_rows(
      #groups = TRUE,
      columns = Poids,
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      TRESO = "Trésorerie", SIGNAT = "Signature"
    ) 
  
  
}

#final.df %>%
#  notationConsolidee(arrete = d)


###### Realiser une matrice de transition
# selectionner les 2 derni


transNotation <- function(df, pays, arrete, profondeur){
  
  tab <- df %>%
    filter(Pays == pays, DATE_COMPTA == arrete %m-% months(profondeur)) %>% 
    select(CLIENT, NOTATION) %>%
    left_join(
      final.df %>%
        filter(Pays == pays, DATE_COMPTA == arrete) %>% 
        select(CLIENT, NOTATION)
      , by=c("CLIENT")) %>%
    #rename(NOTATIONAVANT = NOTATION.x, NOTATIONAPRES = NOTATION.y)
    group_by(NOTATION.x, NOTATION.y) %>%
    summarise(N = n()) %>%
    ungroup
  
  tab$NOTATION.y[is.na(tab$NOTATION.y)] <- "Sortie"
  
  tab <- tab %>%
    spread(key = NOTATION.y, value = N)
  
  # si la colonne "Sortie existante"
  
  if (!("Sortie" %in% names(tab))){
    tab$Sortie <- NA
  } 
  
  
  names(tab)[1] <- "NOTATION"
  tab[is.na(tab)] <- 0
  m = tab[2:(ncol(tab)-1)]
  tab$TOTAL = rowSums(tab[2:ncol(tab)], na.rm = TRUE)
  tab$Degradation = rowSums(m * upper.tri(m))
  tab$Stabilite = m[row(m)==col(m)]
  tab$Amelioration = rowSums(m * lower.tri(m))
  
  
  tabProp <- tab
  tabProp[,2:(ncol(tabProp)-3)] <- tab[,2:(ncol(tab)-3)] / tab$TOTAL
  tabProp[,(ncol(tab)-2):ncol(tab)] <- tab[,(ncol(tab)-2):ncol(tab)] / tab$TOTAL
  tabProp[is.na(tabProp)] <- 0
  
  
  tabPropGt <- tabProp %>%
    gt(
      rowname_col = "NOTATION"
    ) %>%
    fmt_percent(
      columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
      decimals = 2
    ) %>%
    fmt_missing(
      columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
      missing_text = 0
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(13)
    )  %>% tab_spanner(
      label = as.character(d),
      columns = c(contains("0"),"Sortie")
    ) %>% 
    tab_row_group(
      label = as.character(arrete %m-% months(profondeur)),
      rows = contains("0")
    )
    
  return(list(transitionN=tab, transitionProp = tabProp, transitionPropGt = tabPropGt))
}

transTab <- final.df %>%
  transNotation("Benin", d, 2)

#transTab$transitionProp


storyNotation <- function(tabProp, seuilDeg=0.1, seuilStab=0.95, seuilAme=0.1){
  
  # appreciation de la dégradation
  riskclasses = tabProp$NOTATION
  riskclassdeg = riskclasses[tabProp$Degradation >= seuilDeg]
  if (is_empty(riskclassdeg)){
    c1 = "Il n'y a pas de dégradation significative."
  } else {
    c1 = paste0("Les classes ",paste0(riskclassdeg, collapse = ", "), " se sont dégradés (>=",seuilDeg*100,"%)")
  }
  
  
  # appreciation de la stabilité
  riskclassstab = riskclasses[tabProp$Stabilite >= seuilStab]
  if (is_empty(riskclassstab)){
    c2 = "Aucune classe n'est restée stable entre les deux arrêtés."
  } else {
    c2 = paste0("Les classes ",paste0(riskclassstab, collapse = ", "), " se sont restés stable (>=",seuilStab*100,"%).")
  }
  
  # appreciation de l'amélioration
  riskclassame = riskclasses[tabProp$Amelioration >= seuilAme]
  if (is_empty(riskclassame)){
    c3 = paste0("On observe aucune une amélioration au seuil de ", seuilAme*100,"%.")
  } else {
    c3 = paste0("Les classes ",paste0(riskclassame, collapse = ", "), " se sont ameliorées (>=",seuilAme*100,"%) entre les deux arrêtés.")
  }
  
  # de facon générale
  m = tabProp[2:(ncol(tabProp)-5)]
  print(m)
  classMig <- c()
  ans <- c()
  rownames(m) = colnames(m)
  m = as.matrix(m)
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)) {
      if (i!=j){
        if (m[i,j] >= 0.1){
          classMig <- c(classMig, rownames(m)[i])
          ans <- paste(ans, paste(100*m[i,j], "% des dossiers",rownames(m)[i],
                                  "se retrouvent en",colnames(m)[j]),".")
        }
      }
    }
  }
  
  if (is.null(ans)){
    c0 = paste("Aucune observation n'est faite sur les transitions")
  } else {
    c0 = paste("On note une forte ", sample(c("migration","transition"),1), " des dossiers classés en",
               paste(unique(classMig),collapse=","),"sur l'arrêté précédent. En effet,", ans)
  }
  paste(c0, c1, c2, c3)
  
}

storyNotation(tabProp = transTab$transitionProp)

