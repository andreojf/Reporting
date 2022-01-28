# entree de l'aval de traite
avalTraite = t(data.frame(Benin = 1000000, 
              CI = 5000000, 
              ML = 2500000,
              NG = 1500000,
              SN = 2300000,
              TG = 2000000, row.names = c("AVALSTRAITE"))) %>%
  as.data.frame %>%
  rownames_to_column("Pays")


# attention il faudrait merger ici
HorsBilanparPays <- function(df, arrete, avaltraite){
  
  df %>%
    filter(DATE_COMPTA == arrete) %>%
    group_by(Pays) %>%
    summarise(CREDITDOCU = -1*sum(CREDOC),
              CAUTIONS = -1*sum(CAUTION),
            ENGAGEMCONFIRME = -1*sum(CONFIRME),
              ) %>%
    left_join(avalTraite, by="Pays") %>%
    mutate(TOTALHORBILAN = CREDITDOCU + CAUTIONS + AVALSTRAITE + ENGAGEMCONFIRME,
           PART = TOTALHORBILAN / sum(TOTALHORBILAN)) %>%
    gt %>%
    fmt_percent(
      columns = PART,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(CREDITDOCU, CAUTIONS, ENGAGEMCONFIRME,
                  AVALSTRAITE, TOTALHORBILAN),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(CREDITDOCU, CAUTIONS, ENGAGEMCONFIRME,
                  AVALSTRAITE, TOTALHORBILAN),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      columns = c(CREDITDOCU, CAUTIONS, ENGAGEMCONFIRME,
                  AVALSTRAITE, TOTALHORBILAN),
      fns = list(TOTAL = "sum"),
      formatter = fmt_number,
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    summary_rows(
      columns = PART,
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      CREDITDOCU = "Credits documentaires", CAUTIONS = "Cautions",
      ENGAGEMCONFIRME = "Engagements confirmes", AVALSTRAITE = "Avals de traite",
      TOTALHORBILAN = "Total hors bilan", PART = "% Total"
    )
    
  
}

final.df %>%
  HorsBilanparPays("2020-12-31", avalTraite)
