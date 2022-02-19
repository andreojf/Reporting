# source("preparation.R")

BCEAOparPays <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>% 
    group_by(Pays, SECTEURBCEAO) %>%
    summarise(
      N = n(),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = TRESO + SIGNAT,
      CESBilan = abs(sum(Restructures + DOUTEUX_292)),
      SainBilan = TRESO - CESBilan,
      SainHorsbilan = TOTAL - SainBilan,
      DOUTEUXHorsbilan = abs(sum(DOUTEUX_SIGNATURE)),
      PROVISIONS = abs(sum(PROVISION)),
      TauxProv = PROVISIONS / CESBilan,
      TauxDegradation = CESBilan / TRESO
    ) %>%
    mutate(PartTotal = TOTAL / sum(TOTAL))
  
  # byBCEAO$PartTotal <- byBCEAO$TOTAL / sum(byBCEAO$TOTAL)
  # byBCEAO$TauxProv <- byBCEAO$PROVISIONS_CES / sum(byBCEAO$CES)
  
  
  out <- out[complete.cases(out$SECTEURBCEAO),]
  
  out %>%
    ungroup %>%
    gt(
      rowname_col = "SECTEURBCEAO",
      groupname_col = "Pays"
    ) %>% 
    fmt_percent(
      columns = c(TauxProv, TauxDegradation, PartTotal),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                  DOUTEUXHorsbilan, PROVISIONS),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                    DOUTEUXHorsbilan, PROVISIONS),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      groups = TRUE,
      columns = c(N, TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                  DOUTEUXHorsbilan, PROVISIONS),
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
  BCEAOparPays("2020-12-31")

### vision consolidée
BCEAOConsolidee <- function(df, arrete){
  
  out <- df %>%
    filter(DATE_COMPTA == arrete) %>% 
    group_by(SECTEURBCEAO) %>%
    summarise(
      N = n(),
      TRESO = abs(sum(TRESORERIE)),
      SIGNAT = abs(sum(SIGNATURE)),
      TOTAL = TRESO + SIGNAT,
      CESBilan = abs(sum(Restructures + DOUTEUX_292)),
      SainBilan = TRESO - CESBilan,
      SainHorsbilan = TOTAL - SainBilan,
      DOUTEUXHorsbilan = abs(sum(DOUTEUX_SIGNATURE)),
      PROVISIONS = abs(sum(PROVISION)),
      TauxProv = PROVISIONS / CESBilan,
      TauxDegradation = CESBilan / TRESO
    ) %>%
    mutate(PartTotal = TOTAL / sum(TOTAL))
  
  # byBCEAO$PartTotal <- byBCEAO$TOTAL / sum(byBCEAO$TOTAL)
  # byBCEAO$TauxProv <- byBCEAO$PROVISIONS_CES / sum(byBCEAO$CES)
  
  
  out <- out[complete.cases(out$SECTEURBCEAO),]
  
  out %>%
    ungroup %>%
    gt(
      #rowname_col = "SECTEURBCEAO",
      #groupname_col = "Pays"
    ) %>% 
    fmt_percent(
      columns = c(TauxProv, TauxDegradation, PartTotal),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                  DOUTEUXHorsbilan, PROVISIONS),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                  DOUTEUXHorsbilan, PROVISIONS),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      #groups = TRUE,
      columns = c(N, TRESO, SIGNAT, TOTAL, CESBilan, SainBilan, SainHorsbilan,
                  DOUTEUXHorsbilan, PROVISIONS),
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
  BCEAOConsolidee("2020-12-31")
