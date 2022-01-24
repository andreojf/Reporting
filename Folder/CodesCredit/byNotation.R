### CALCUL par Notation

source("preparation.R")


# 
bynotation <- data %>%
  group_by(NOTATION) %>%
  summarise(
    N = n(),
    TRESO = abs(sum(TRESORERIE)),
    SIGNAT = abs(sum(SIGNATURE)),
    TOTAL = abs(TRESO + SIGNAT))
bynotation$Poids <- bynotation$TOTAL / sum(bynotation$TOTAL)
bynotation[nrow(bynotation) + 1,] = list("Total",
                                         sum(bynotation$N),
                                         sum(bynotation$TRESO),
                                         sum(bynotation$SIGNAT),
                                         sum(bynotation$TOTAL),
                                         sum(bynotation$Poids))
