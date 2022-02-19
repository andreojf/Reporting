# 
depots = 1000000000000

synthese <- function(df, arrete, pays){
  
  final.df %>%
    filter(DATE_COMPTA == arrete, Pays == pays) %>%
    group_by(Pays) %>%
    arrange(TOTAL_ENG) %>%
    summarise(
      ENGAGEMENTS_DIRECTS = sum(-TRESORERIE),
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
           "       - Crénaces douteuses nettes" = PROV_CREANCES_DT_DIRECTS,
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

GrandeSynthese <- function(df, pays, arrete, prev1arrete, prev2arrete){
  
  out <- synthese(
    df, 
    prev2arrete,
    "Benin") %>% 
    left_join(
      synthese(df,
               prev1arrete, 
               pays),
      by = c("Pays","Indicateurs")
    ) %>% 
    left_join(
      synthese(df,
               arrete, 
               pays),
      by = c("Pays","Indicateurs")
    ) 
  
  colnames(out)[3:5] = c(prev2arrete, prev1arrete, arrete)
  
  # Calcul des évolutions
  out["evol1"] = (out[arrete] - out[prev2arrete]) / out[prev2arrete]
  out["evol2"] = (out[arrete] - out[prev1arrete]) / out[prev1arrete]
  
  indicateursRatio = c(
    "Crédits restructurés/Engagements directs",
    "Douteux Eng. Dir./Engagements Directs",
    "CES directes/Total Engagements",
    "Taux de provisionnement",
    "Taux de dégradation du portefeuille",
    "50 plus gros risques/Portefeuille global",
    "Ratio Prêts/Dépôts")
    
  outN = out[!(out$Indicateurs %in% indicateursRatio),]
  outRatio = out[out$Indicateurs %in% indicateursRatio,]

  return(list(SynthMontant=outN, SynthRatio=outRatio))
}

synthTab <- final.df %>%
  GrandeSynthese(
  "Benin",
  "2020-12-31",
  "2020-11-30",
  "2020-10-31"
)

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

pfSyntFormatTable <- function(tab, params, ratio=T){
  
  if (!(ratio)){
    tab <- tab %>%
      gt %>%
      fmt_number(
        columns = c(params$arrete, params$prev1Arrete, params$prev2Arrete),
        scale_by = if_else(ratio, 1, 1 / 1E6),
        pattern = if_else(ratio, "", "{x} M"),
        decimals = 0
        )
  }else{
    tab <- tab %>%
      gt %>%
      fmt_percent(
        columns = c(params$arrete, params$prev1Arrete, params$prev2Arrete),
        decimals = 1
      )
  }
  
    tab %>% 
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

synthTab$SynthMontant %>%
  pfSyntFormatTable(params, ratio=F)

synthTab$SynthRatio %>%
  pfSyntFormatTable(params, ratio=T)


##### SYNTHESE AU NIVEAU GROUPE
syntheseGroupe <- function(df, arrete){
  
  final.df %>%
    filter(DATE_COMPTA == arrete) %>%
    arrange(TOTAL_ENG) %>%
    summarise(
      ENGAGEMENTS_DIRECTS = sum(-TRESORERIE),
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
           "       - Crénaces douteuses nettes" = PROV_CREANCES_DT_DIRECTS,
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
    gather(key = "Indicateurs", value = "Montant")
  
}

#
syntheseGroupe(
  final.df, 
  "2020-12-31")

GrandeSyntheseGroupe <- function(df, arrete, prev1arrete, prev2arrete){
  
  out <- syntheseGroupe(
    df, 
    prev2arrete) %>% 
    left_join(
      syntheseGroupe(df,
               prev1arrete),
      by = c("Indicateurs")
    ) %>% 
    left_join(
      syntheseGroupe(df,
               arrete
               ),
      by = c("Indicateurs")
    ) 
  
  colnames(out)[2:4] = c(prev2arrete, prev1arrete, arrete)
  
  
  indicateursRatio = c(
    "Crédits restructurés/Engagements directs",
    "Douteux Eng. Dir./Engagements Directs",
    "CES directes/Total Engagements",
    "Taux de provisionnement",
    "Taux de dégradation du portefeuille",
    "50 plus gros risques/Portefeuille global",
    "Ratio Prêts/Dépôts")
  
  outN = out[!(out$Indicateurs %in% indicateursRatio),]
  outRatio = out[out$Indicateurs %in% indicateursRatio,]
  
  # Calcul des évolutions
  outN["evol1"] = (outN[arrete] - outN[prev2arrete]) / outN[prev2arrete]
  outN["evol2"] = (outN[arrete] - outN[prev1arrete]) / outN[prev1arrete]
  
  # Calcul des évolutions
  outRatio["evol1"] = (outRatio[arrete] - outRatio[prev2arrete]) 
  outRatio["evol2"] = (outRatio[arrete] - outRatio[prev1arrete]) 
  return(list(SynthMontant=outN, SynthRatio=outRatio))
}

synthTabGroup <- final.df %>%
  GrandeSyntheseGroupe(
    "2020-12-31",
    "2020-11-30",
    "2020-10-31"
  )

synthTabGroup$SynthMontant %>%
  pfSyntFormatTable(params, ratio=F)

synthTabGroup$SynthRatio %>%
  pfSyntFormatTable(params, ratio=T)



  
