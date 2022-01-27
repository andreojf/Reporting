library(readxl)
library(tidyverse)
library(plyr)
# recuperer tous les fichiers excel dans le dossier spÃÂ©cifiÃÂ©


importFile <- function(filepath){
  df <- read_excel(path = filepath, 
                   sheet = "Situation Globale Retr",
                   skip = 8)
  df <- df[1:(nrow(df)-1),]
  df <- df %>%
    mutate(Pays = basename(dirname(filepath)))
  return(df)
}

#df <- importFile("FichiersExcel//NG/ENGAGEMENT_PORTEFEUILLE DE CBI NG au 31.12.2020.xlsx")
filepaths <- list.files(path="FichiersExcel/",
                        recursive=T,
                        pattern=".xlsx",
                        full.names=T)
df.list <- lapply(filepaths, importFile)
final.df <- rbind.fill(df.list)


### detacher le package plyr
detach("package:plyr", unload = TRUE)
