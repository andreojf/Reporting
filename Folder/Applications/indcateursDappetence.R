library(tidyverse)

portefeuille <- data.frame(list(
  intitule = c("PORTEFEUILLE D'EFFETS",
                "CREDITS CCT",
                "CREDITS M&LT",
                "COMPTES DEBITEURS",
                "ECHEANCES IMPAYEES",
                "PORTEFEUILLE  RESTRUCTURE",
                "CREDITS PARTICULIERS",
                "CREDITS ENTREPRISES"),
  Montant = c(0,15,0,0,0,0,0,0))
)

r <- portefeuille %>% 
  filter(intitule %in% c("PORTEFEUILLE D'EFFETS",
                         "CREDITS CCT",
                         "CREDITS M&LT",
                         "COMPTES DEBITEURS")) %>%
  summarise(val = sum(Montant))
r$val
