# ### CALCUL par Notation
# library(dplyr)
# #source("preparation.R")
# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE) 
#   detach("package:plyr", unload=TRUE) 
# } 
# library(plyr)
detach("package:plyr", unload=TRUE) 
library(dplyr)
library(gt)

# 
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
    mutate(Poids = TRESO / sum(TRESO)) %>%
    gt(
      rowname_col = "NOTATION",
      groupname_col = c("Pays")
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
