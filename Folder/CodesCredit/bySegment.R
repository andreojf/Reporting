source("preparation.R")

## 
bySegment <- data %>%
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
    )

bySegment$PartTotal <- bySegment$TOTAL / sum(bySegment$TOTAL)
#bySegment$TauxProv <- bySegment$PROVISIONS_CES / sum(bySegment$CES)
bySegment[nrow(bySegment) + 1,] = list("Total",
                                      sum(bySegment$N),
                                      sum(bySegment$TRESO),
                                      sum(bySegment$SIGNAT),
                                      sum(bySegment$TOTAL),
                                      sum(bySegment$Restructure),
                                      sum(bySegment$DOUTEUXDIRECTS),
                                      sum(bySegment$CES),
                                      sum(bySegment$PROVISIONS_CES),
                                      sum(bySegment$TauxProv),
                                      sum(bySegment$TauxDegradation),
                                      sum(bySegment$PartTotal)
                                      )
    
