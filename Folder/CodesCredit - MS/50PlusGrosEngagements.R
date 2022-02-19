# source("preparation.R")


top50ParPays <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(Pays) %>%
  select(Pays, CLIENT, NOM, GESTIONNAIRE, SEGMENT,
         #SECTEURAMPLITUDE,
         SECTEURBCEAO,DATE_COMPTA, TOTAL_ENG,
         # NOTATION,
         # DATE_DECL, 
         EFFET, COMPTE1,
         CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
         DOUTEUX_SIGNATURE,	PROVISION,	
         PROVISION_512,
         CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE
         ) %>%
    mutate(Poids = TOTAL_ENG / sum(TOTAL_ENG)) %>%
    arrange(TOTAL_ENG) %>%
    top_n(50) %>%
    ungroup
  
  out %>%
    gt(
      groupname_col = "Pays",
      rowname_col = "CLIENT"
    ) %>%
    # fmt_percent(
    #   columns = PART,
    #   decimals = 1
    # ) %>%
    fmt_number(
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(10)
    )  %>%
    summary_rows(
      groups = T,
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0)
    # ) %>%
    # summary_rows(
    #   columns = PART,
    #   fns = list(TOTAL = "sum"),
    #   formatter = fmt_percent,
    # ) %>%
    # cols_label(
    #   CREDITDOCU = "Credits documentaires", CAUTIONS = "Cautions",
    #   ENGAGEMCONFIRME = "Engagements confirmes", AVALSTRAITE = "Avals de traite",
    #   TOTALHORBILAN = "Total hors bilan", PART = "% Total"
    # )
  
  #out[,7:24] <- -1 %*% out[,7:24] 
  
}

final.df %>%
  top50ParPays("2020-12-31")


#### Vision globale
top50Consolidee <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>%
    #group_by(Pays) %>%
    select(Pays, CLIENT, NOM, GESTIONNAIRE, SEGMENT,
           #SECTEURAMPLITUDE,
           SECTEURBCEAO,DATE_COMPTA, TOTAL_ENG,
           # NOTATION,
           # DATE_DECL, 
           EFFET, COMPTE1,
           CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
           DOUTEUX_SIGNATURE,	PROVISION,	
           PROVISION_512,
           CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
           ) %>%
    mutate(Poids = TOTAL_ENG / sum(TOTAL_ENG)) %>%
    arrange(TOTAL_ENG) %>%
    top_n(50) %>%
    ungroup
  
  out %>%
    gt(
      # groupname_col = "Pays",
      # rowname_col = "CLIENT"
    ) %>%
    # fmt_percent(
    #   columns = PART,
    #   decimals = 1
    # ) %>%
    fmt_number(
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(10)
    )  %>%
    summary_rows(
      #groups = T,
      columns = c(EFFET, COMPTE1,
                  CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
                  DOUTEUX_SIGNATURE,	PROVISION,	
                  PROVISION_512,
                  CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
                  TOTAL_ENG),
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0)
  # ) %>%
  # summary_rows(
  #   columns = PART,
  #   fns = list(TOTAL = "sum"),
  #   formatter = fmt_percent,
  # ) %>%
  # cols_label(
  #   CREDITDOCU = "Credits documentaires", CAUTIONS = "Cautions",
  #   ENGAGEMCONFIRME = "Engagements confirmes", AVALSTRAITE = "Avals de traite",
  #   TOTALHORBILAN = "Total hors bilan", PART = "% Total"
  # )
  
  #top50[,7:24] <- top50[,7:24] * -1
  
}

final.df %>%
  top50Consolidee("2020-12-31") 


#### ETUDE DE CONCENTRATION
concentration <- function(df, arrete){
  
  final.df %>%
    filter(DATE_COMPTA == "2020-12-31") %>%
    #group_by(Pays) %>%
    select(TOTAL_ENG, Pays, CLIENT, NOM, GESTIONNAIRE, SEGMENT,
           #SECTEURAMPLITUDE,
           SECTEURBCEAO,DATE_COMPTA, 
           # NOTATION,
           # DATE_DECL, 
           EFFET, COMPTE1,
           CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
           DOUTEUX_SIGNATURE,	PROVISION,	
           PROVISION_512,
           CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE) %>%
    mutate(Poids = TOTAL_ENG / sum(TOTAL_ENG)) %>%
    arrange(TOTAL_ENG) %>%
    top_n(50) %>%
    group_by(CLIENT) %>%
    summarise(
      N = n(),
      Poids = sum(Poids),
      PROV = paste(Pays)) %>%
    arrange(desc(N))
  
}

  