library(readxl)
library(tidyverse)
library(plyr)
# recuperer tous les fichiers excel dans le dossier spécifique

# 
# importFile <- function(filepath){
#   df <- read_excel(path = filepath, 
#                    sheet = "Situation Globale Retr",
#                    skip = 8)
#   df <- df[1:(nrow(df)-1),]
#   df <- df %>%
#     mutate(Pays = basename(dirname(filepath)))
#   return(df)
# }
# 
# #df <- importFile("FichiersExcel//NG/ENGAGEMENT_PORTEFEUILLE DE CBI NG au 31.12.2020.xlsx")
# filepaths <- list.files(path="FichiersExcel/",
#                         recursive=T,
#                         pattern=".xlsx",
#                         full.names=T)
# df.list <- lapply(filepaths, importFile)
# final.df <- rbind.fill(df.list)

#########################################################################################
######################## VERSION 1.1 DE L'IMPORTATION ###################################
#########################################################################################


createDatasetHolding <- function(filepaths, ISOPays=NULL){
  
  # afficher les fichiers importés
  # cat("Fichiers importés:\n")
  # for (file in filepaths){
  #   cat(paste(basename(dirname(file)),
  #             ":",
  #             basename(file),
  #             "\n"))
  # 
  # }
  
  # si le pays a été spécifié
  if (!(is.null(ISOPays))){
    cat("Pays spécifié\n-------------------------\n")
    filepaths <- filepaths[grep(ISOPays, filepaths)]
  }
  
  # importer les fichiers concernés
  df.brut.list <- lapply(filepaths, function(filepath){
    cat(basename(dirname(filepath)),
              ":",
              basename(filepath),
              "\n")
    df <- read_excel(path = filepath,
                      sheet = "Brut")
    cat("----: (avant) Nrow:", nrow(df)," Ncol:", ncol(df), "\n")
    df <- df[!is.na(df$CLIENT), ]
    cat("----: (apres) Nrow:", nrow(df)," Ncol:", ncol(df), "\n")
    df <- df %>%
     mutate(Pays = basename(dirname(filepath)))
    return(df)
  })
  final.df.brut <- rbind.fill(df.brut.list)
  final.df.brut
}




a = "C:/Users/jaouedraogo/Documents/Coris/Reporting/Semestriels/Donnees//BJ/DSIO_ENGAGEMENT_PORTEFEUILLE_202106.xlsx"
#########################################################################################
######################## VERSION 2.0 DE L'IMPORTATION ###################################
#########################################################################################

## recuperer tous les chemins contenant des fichiers excels
filepaths <- list.files(path="C:/Users/jaouedraogo/Documents/Coris/Reporting/Folder/FichiersExcel2/",
                        recursive=T,
                        pattern=".xlsx",
                        full.names=T)

createDatasetBrut <- function(filepaths, pays){
  
  # extraire les fichiers contenant le nom du pays concerné
  filepathsPays <- filepaths[grep(pays, filepaths)]
  # 
  # # afficher les noms des fichiers importés
  # cat("Fichiers importes:\n")
  # for (file in filepathsPays){
  #   cat("--:", basename(file), "\n")
  # }
  
  
  # importer les fichiers concernés
  df.brut.list <- lapply(filepathsPays, function(filepath){
    read_excel(path = filepath, 
               sheet = "Brut")
  })
  final.df.brut <- rbind.fill(df.brut.list)
  final.df.brut$Pays <- pays
  final.df.brut
}

# brut.df <- createDatasetBrut(filepaths, "Togo")

### detacher le package plyr
detach("package:plyr", unload = TRUE)

