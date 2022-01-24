source("preparation.R")


byBCEAO <- data %>%
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
  )

byBCEAO$PartTotal <- byBCEAO$TOTAL / sum(byBCEAO$TOTAL)
#byBCEAO$TauxProv <- byBCEAO$PROVISIONS_CES / sum(byBCEAO$CES)
byBCEAO[nrow(byBCEAO) + 1,] = list("Total",
                                   
                                   sum(byBCEAO$N),
                                   sum(byBCEAO$TRESO),
                                   sum(byBCEAO$SIGNAT),
                                   sum(byBCEAO$TOTAL),
                                   sum(byBCEAO$CESBilan),
                                   sum(byBCEAO$SainBilan),
                                   sum(byBCEAO$SainHorsbilan),
                                   sum(byBCEAO$DOUTEUXHorsbilan),
                                   sum(byBCEAO$PROVISIONS),
                                   sum(byBCEAO$TauxProv),
                                   sum(byBCEAO$TauxDegradation),
                                   sum(byBCEAO$PartTotal)
)


