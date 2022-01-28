# source("preparation.R")

# donner la date d'arrete
arrete = as.Date("31/12/2020", format="%d/%m/%Y")
arrete

# calculer l'anciennete par déclassement
declassementparPays <- function(df, arrete){
  
  final.df %>%
    filter(!is.na(DATE_DECL), DATE_COMPTA == arrete) %>%
    mutate(ageDeCL = -1*interval(arrete, DATE_DECL) %/% years(1),
           classDeCL = cut(ageDeCL, 
                           breaks=c(0, 1, 2, 3, 5, 999), right = FALSE)) %>%
    group_by(Pays, classDeCL) %>%
    summarise(
      N = n(),
      EncoursCDL = abs(sum(DOUTEUX_292)),
      PROVISIONS = abs(sum(PROVISION)),
      TauxProv = PROVISIONS / EncoursCDL
    ) %>%
    mutate(PartN = N / sum(N),
           PartEncours = EncoursCDL / sum(EncoursCDL),
           classDeCL = as.character(classDeCL)) %>%
    gt(
      rowname_col = "classDeCL",
      groupname_col = "Pays"
    ) %>%
    fmt_percent(
      columns = c(TauxProv, PartN),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(EncoursCDL, PROVISIONS),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, EncoursCDL, PROVISIONS, TauxProv),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      groups = TRUE,
      columns = c(N, EncoursCDL, PROVISIONS),
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
      columns = c(PartN, PartEncours),
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      #
    )
  
}


final.df %>%
  declassementparPays(arrete)


#### Vision consolidee
declassementConsolidee <- function(df, arrete){
  
  final.df %>%
    filter(!is.na(DATE_DECL), DATE_COMPTA == arrete) %>%
    mutate(ageDeCL = -1*interval(arrete, DATE_DECL) %/% years(1),
           classDeCL = cut(ageDeCL, 
                           breaks=c(0, 1, 2, 3, 5, 999), right = FALSE)) %>%
    group_by(classDeCL) %>%
    summarise(
      N = n(),
      EncoursCDL = abs(sum(DOUTEUX_292)),
      PROVISIONS = abs(sum(PROVISION)),
      TauxProv = PROVISIONS / EncoursCDL
    ) %>%
    mutate(PartN = N / sum(N),
           PartEncours = EncoursCDL / sum(EncoursCDL),
           classDeCL = as.character(classDeCL)) %>%
    gt(
      #rowname_col = "classDeCL",
      #groupname_col = "Pays"
    ) %>%
    fmt_percent(
      columns = c(TauxProv, PartN, PartEncours),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(EncoursCDL, PROVISIONS),
      scale_by = 1 / 1E6,
      pattern = "{x} M",
      decimals = 0
    ) %>%
    fmt_missing(
      columns = c(N, EncoursCDL, PROVISIONS, TauxProv),
      missing_text = ""
    ) %>%
    tab_options(
      #table.width = pct(80),
      table.font.size = px(13)
    )  %>%
    summary_rows(
      #groups = TRUE,
      columns = c(N, EncoursCDL, PROVISIONS),
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
      columns = c(PartN, PartEncours),
      fns = list(TOTAL = "sum"),
      formatter = fmt_percent,
    ) %>%
    cols_label(
      #
    )
  
}


final.df %>%
  declassementConsolidee(arrete)



