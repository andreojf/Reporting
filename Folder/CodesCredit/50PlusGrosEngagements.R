source("preparation.R")


top50 <- data %>%
  select(CLIENT, NOM, GESTIONNAIRE, SEGMENT,
         #SECTEURAMPLITUDE,
         SECTEURBCEAO,DATE_COMPTA, 
         # NOTATION,
         # DATE_DECL, 
         EFFET, COMPTE1,
         CT,	MT,	LT,	IMPAYES,	DOUTEUX,	TRESORERIE,	DOUTEUX_292,
         DOUTEUX_SIGNATURE,	PROVISION,	
         PROVISION_512,
         CREDOC,	CONFIRME,	CONFIRME_GROUPE,	CAUTION,	SIGNATURE,
         TOTAL_ENG) %>%
  mutate(Poids = TOTAL_ENG / sum(TOTAL_ENG)) %>%
  arrange(TOTAL_ENG) %>%
  top_n(50)

top50[,7:24] <- top50[,7:24] * -1

