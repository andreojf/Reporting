

## 
SegmentParPays <- function(df){
  
  out <- df %>%
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
  SegmentParPays



#### vision consolidée
SegmentConsolide <- function(df){
  
  out <- df %>%
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
      rowname_col = "SEGMENT",
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
  SegmentConsolide
