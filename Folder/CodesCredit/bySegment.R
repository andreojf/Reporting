

## 
SegmentParPays <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(Pays, SEGMENT) %>%
    summarise(
      N = n(), # nombre de contrats
      TRESO = abs(sum(TRESORERIE)), # Tresorerie bilan
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(sum(TRESO + SIGNAT)),
      Restructure = abs(sum(Restructures)),
      DOUTEUXDIRECTS = abs(sum(DOUTEUX_292)),
      CES = abs(sum(Restructures + DOUTEUX_292)),
      PROVISIONS_CES = abs(sum(PROVISION)),
      TauxProv = PROVISIONS_CES / CES,
      TauxDegradation = CES / TRESO
    ) %>%
    mutate(PartTotal = TOTAL / sum(TOTAL))
  
  out <- out[complete.cases(out$SEGMENT),]
  
  out %>%
    ungroup %>%
    gt(
      rowname_col = "SEGMENT",
      groupname_col = "Pays"
    ) %>% 
    fmt_percent(
      columns = c(TauxProv, TauxDegradation, PartTotal),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL, Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, 
                  Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES, 
                  TauxProv, TauxDegradation, PartTotal),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(11)
    )  %>%
    summary_rows(
      groups = TRUE,
      columns = c(N, TRESO, SIGNAT, TOTAL, 
                  Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES),
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
      columns = c(PartTotal),
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      TRESO = "Trésorerie", SIGNAT = "Signature"
    )
}

final.df %>%
  SegmentParPays("2020-12-31")



#### vision consolidée
SegmentConsolide <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(SEGMENT) %>%
    summarise(
      N = n(), # nombre de contrats
      TRESO = abs(sum(TRESORERIE)), # Tresorerie bilan
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = abs(sum(TRESO + SIGNAT)),
      Restructure = abs(sum(Restructures)),
      DOUTEUXDIRECTS = abs(sum(DOUTEUX_292)),
      CES = abs(sum(Restructures + DOUTEUX_292)),
      PROVISIONS_CES = abs(sum(PROVISION)),
      TauxProv = PROVISIONS_CES / CES,
      TauxDegradation = CES / TRESO
    ) %>%
    mutate(PartTotal = TOTAL / sum(TOTAL))
  
  out <- out[complete.cases(out$SEGMENT),]
  
  out %>%
    ungroup %>%
    gt(
      #rowname_col = "SEGMENT",
    ) %>% 
    fmt_percent(
      columns = c(TauxProv, TauxDegradation, PartTotal),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL, Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, 
                  Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES, 
                  TauxProv, TauxDegradation, PartTotal),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(11)
    )  %>%
    summary_rows(
      #groups = TRUE,
      columns = c(N, TRESO, SIGNAT, TOTAL, 
                  Restructure, DOUTEUXDIRECTS, CES, PROVISIONS_CES),
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
      columns = c(PartTotal),
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      TRESO = "Trésorerie", SIGNAT = "Signature"
    )
}

final.df %>%
  SegmentConsolide("2020-12-31")



###### MATRICE DE TRANSITION
transSegment <- function(df, pays, arrete, profondeur){
  
  tab <- df %>%
    filter(Pays == pays, DATE_COMPTA == arrete %m-% months(profondeur)) %>% 
    select(CLIENT, SEGMENT) %>%
    left_join(
      final.df %>%
        filter(Pays == pays, DATE_COMPTA == arrete) %>% 
        select(CLIENT, SEGMENT)
      , by=c("CLIENT")) %>%
    #rename(SEGMENTAVANT = SEGMENT.x, SEGMENTAPRES = SEGMENT.y)
    group_by(SEGMENT.x, SEGMENT.y) %>%
    summarise(N = n()) %>%
    ungroup
  
  tab$SEGMENT.y[is.na(tab$SEGMENT.y)] <- "Sortie"
  
  tab <- tab %>%
    spread(key = SEGMENT.y, value = N)
  
  # si la colonne "Sortie existante"
  
  if (!("Sortie" %in% names(tab))){
    tab$Sortie <- NA
  } 
  
  
  names(tab)[1] <- "SEGMENT"
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
      rowname_col = "SEGMENT"
    ) %>%
    # fmt_percent(
    #   columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
    #   decimals = 2
    # ) %>%
    # fmt_missing(
    #   columns = c(contains("0"),"Sortie","Degradation","Stabilite","Amelioration"),
    #   missing_text = 0
    # ) %>%
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
  transSegment("Benin", d, 2)


##### STORY
storySegment <- function(tabProp, seuilDeg=0.1, seuilStab=0.95, seuilAme=0.1){
  
  # appreciation de la dégradation
  riskclasses = tabProp$SEGMENT
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

storySegment(tabProp = transTab$transitionProp)
