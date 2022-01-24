source("preparation.R")

# donner la date d'arrete
arrete = as.Date("31/12/2020", format="%d/%m/%Y")
arrete

# calculer l'anciennete par déclassement
CDLparAge <- data %>%
  filter(!is.na(DATE_DECL)) %>%
  mutate(ageDeCL = floor(as.numeric(difftime(arrete, DATE_DECL)/365)),
         classDeCL = cut(ageDeCL, 
                         breaks=c(0, 1, 2, 3, 5), right = FALSE)) %>%
  group_by(classDeCL) %>%
  summarise(
    N = n(),
    EncoursCDL = abs(sum(DOUTEUX_292)),
    PROVISIONS = abs(sum(PROVISION)),
    TauxProv = PROVISIONS / EncoursCDL
  ) %>%
  mutate(PartN = N / sum(N),
         PartEncours = EncoursCDL / sum(EncoursCDL),
         classDeCL = as.character(classDeCL)) 

CDLparAge[nrow(CDLparAge) + 1,] = list("Total",
                                       sum(CDLparAge$N),
                                       sum(CDLparAge$EncoursCDL),
                                       sum(CDLparAge$PROVISIONS),
                                       sum(CDLparAge$TauxProv),
                                       sum(CDLparAge$PartN),
                                       sum(CDLparAge$PartEncours))  
