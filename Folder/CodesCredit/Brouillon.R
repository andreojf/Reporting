pfAgrege <- function(df){
  
  #pays <- unique(df$Pays)
  #pays <- pays[!is.na(pays)]
  
  
  out <- data.frame(Intitule=c(
    "Encours des effets",
    "Credits Court terme",
    "Credit Moyen et Long terme",
    "Compte débiteurs",
    "Impayes",
    "Restructures",
    "Credits Particuliers",
    "Credits Entreprises"
  ))
  
  #for (p in pays){
  out <- df %>% 
    filter(Pays == p) %>%
    SituationDuPortefeuille %>%
    .$porte_f %>%
    rename(c("Montant"=p)) %>%
    left_join(out)
  #}
  out$Total = rowSums(out[, 2:ncol(out)])
  return(out)
}
pfAgregeGlobal <- function(df){
  
  pays <- unique(df$Pays)
  pays <- pays[!is.na(pays)]
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (p in pays){
    out <- df %>% 
      filter(Pays == p) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      mutate(Prop = Montant / sum(Montant)) %>%
      rename(c("Montant"=p, "Prop"=paste("Prop",p))) %>%
      left_join(out, by="Intitule")
  }
  #out$Global = rowSums(out[, 2:ncol(out)])
  return(out)
}
pfEvolGlobal <- function(df, pays){
  
  
  dates_compta <- unique(df$DATE_COMPTA)
  dates_compta <- dates_compta[!is.na(dates_compta)]
  dates_compta <- sort(dates_compta, decreasing = TRUE)
  dates_compta_top3 <- dates_compta[1:3]
  dates_compta_top3 <- dates_compta_top3[!is.na(dates_compta_top3)] # dans le cas où il y a moins de 3 arretés.
  dates_compta_top3
  
  out <- data.frame(Intitule=c(
    "Sain","Restructure","Douteux et Litigieux"
  ))
  
  for (date_compta in dates_compta_top3){
    out <- df %>% 
      filter(Pays == pays, DATE_COMPTA==date_compta) %>%
      SituationDuPortefeuille %>%
      .$porte_f_tot %>%
      mutate(Prop = Montant / sum(Montant)) %>%
      #rename(c("Montant"=p, "Prop"=paste("Prop",p))) %>%
      left_join(out, by="Intitule")
  }
  
  names(out) <- NULL
  names(out)[1] <- "Intitule"
  indper <- seq(3, ncol(out), by=2)
  inddate <- seq(2, ncol(out), by=2)
  names(out)[indper] <- sapply(dates_compta_top3, function(x) paste("% on",x))
  names(out)[inddate] <- sapply(dates_compta_top3, function(x) paste(x))
  return(out)
}



library(grid)
library(ggnewscale)
library(ggtext)
library(tidyverse)
library(shadowtext)
library(patchwork)

# First, define colors.
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"

regions <- c(
  "Sub-Saharan Africa", 
  "Asia and the Pacific", 
  "Latin America and the Caribbean"
)

line_data <- data.frame(
  year = rep(c(2008, 2012, 2016, 2020), 3),
  percent = c(25.5, 21, 22.2, 24, 13.5, 9.5, 7.5, 5.5,10, 9, 7.5, 5.8),
  region = factor(rep(regions, each = 4), levels = regions)
)

# Aesthetics defined in the `ggplot()` call are reused in the 
# `geom_line()` and `geom_point()` calls.
plt1 <- ggplot(line_data, aes(year, percent)) +
  geom_line(aes(color = region), size = 2.4) +
  geom_point(
    aes(fill = region), 
    size = 5, 
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white", 
    stroke = 1 # The width of the border, i.e. stroke.
  ) +
  # Set values for the color and the fill
  scale_color_manual(values = c(BLUE, GREEN, BROWN)) +
  scale_fill_manual(values = c(BLUE, GREEN, BROWN)) + 
  # Do not include any legend
  theme(legend.position = "none") + 
  scale_x_continuous(
    limits = c(2007.5, 2021.5),
    expand = c(0, 0), # The horizontal axis does not extend to either side
    breaks = c(2008, 2012, 2016, 2020),  # Set custom break locations
    labels = c("2008", "12", "16", "20") # And custom labels on those breaks!
  ) +
  scale_y_continuous(
    limits = c(0, 32),
    breaks = seq(0, 30, by = 5), 
    expand = c(0, 0)
  ) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Remove all grid lines
    panel.grid = element_blank(),
    # But add grid lines for the vertical axis, customizing color and size 
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks on the vertical axis by setting their length to 0
    axis.ticks.length.y = unit(0, "mm"), 
    # But keep tick marks on horizontal axis
    axis.ticks.length.x = unit(2, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only the bottom line of the vertical axis is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  ) + 
  new_scale_color() + 
  geom_shadowtext(
    aes(x, y, label = labels, color = color),
    data = line_labels,
    hjust = 0, # Align to the left
    bg.colour = "white", # Shadow color (or background color)
    bg.r = 0.4, # Radius of the background. The higher the value the bigger the shadow.
    family = "Econ Sans Cnd",
    size = 6
  ) + 
  scale_color_identity() # Use the colors in the 'color' variable as they are.

plt1



final.df %>%
  notationParPays(pays="Benin") %>%
  NotationFormatTable


library(plotly)
library(hrbrthemes)
library(viridis)
library(ggplot2)

p <- final.df %>%
  filter(Pays == "Benin") %>%
  group_by(NOTATION, DATE_COMPTA) %>%
  summarise(
    TRESO = abs(sum(TRESORERIE)) / 1e6,
    SIGNAT = abs(sum(SIGNATURE)) / 1e6,
    TOTAL = abs(TRESO + SIGNAT)
  ) %>%
  gather(key = "Indicateurs", value = "Montant", 3:5) %>%
  ggplot(aes(x=as.character(DATE_COMPTA), y=Montant)) +
  geom_line(aes(color = NOTATION), size = 0.75) +
  xlab("(03) derniers arrêtés") +
  ylab("Montant en Millions") +
  geom_point(
    aes(fill = NOTATION), 
    size = 3, 
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white", 
    stroke = 1 # The width of the border, i.e. stroke.
  ) +
  facet_wrap(~Indicateurs) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Evolution des indicateurs par type de notation sur les 3 derniers mois") +
  theme_ipsum()
ggplotly(p, tooltip="text")



### Benin
```{r echo=FALSE, message=FALSE}
country = "Benin"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
country = "Benin"
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```
\
Les effets sont portés par {{CBI}}
\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\

### Côte d'Ivoire
```{r echo=FALSE, message=FALSE}
country = "CI"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\


### Guinée
```{r echo=FALSE, message=FALSE}
country = "Benin"
```

### Mali
```{r echo=FALSE, message=FALSE}
country = "ML"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:
  
  ```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\

### Niger
```{r echo=FALSE, message=FALSE}
country = "NG"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\

### Sénégal
```{r echo=FALSE, message=FALSE}
country = "SN"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:
  
  ```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\

### Togo 
```{r echo=FALSE, message=FALSE}
country = "TG"
```

L'évolution du portefeuille de CBI **`r country`** sur les (03) trois derniers mois.

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvol(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation du portefeuille détaillée de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\
La synthèse suivant les Sains, Restructurés et Douteux et litigieux est reporté dans le tableau suivant:

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfEvolGlobal(pays = country) %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille de CBI ", country, "**", sep = "")),
    subtitle = paste("arreté du",params$arrete)
  ) 
```
\
\


## Vision agrégée

La vision agrégée reprend les mêmes informations que précédemment. Les chiffres sont obtenus en sommant les montants de toutes entités. 
\

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfAgrege %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille du groupe**")),
    subtitle = paste("arreté du",params$arrete)
  )
```

\
\

```{r echo=FALSE, message=FALSE}
final.df %>%
  pfAgregeGlobal %>%
  pfFormatTable(country = country, params = params) %>%
  tab_header(
    title = md(paste("**Situation globale du portefeuille du groupe**")),
    subtitle = paste("arreté du",params$arrete)
  )
```
\
\
**Observations:**
`r final.df %>% pfAgregeGlobal %>% storyPtf `

# Répartition suivant la qualité 


```{r echo=FALSE, message=FALSE}
final.df %>%
  notationParPays(pays="Benin") %>%
  NotationFormatTable
```








p <- final.df %>%
  filter(Pays == "Benin") %>%
  group_by(NOTATION, DATE_COMPTA) %>%
  summarise(
    TRESO = abs(sum(TRESORERIE)) / 1e6,
    SIGNAT = abs(sum(SIGNATURE)) / 1e6,
    TOTAL = abs(TRESO + SIGNAT)
  ) %>%
  gather(key = "Indicateurs", value = "Montant", 3:5) %>%
  ggplot(aes(x=DATE_COMPTA, y=Montant, label=Montant)) +
  geom_line(aes(color = NOTATION), size = 0.75) +
  xlab("(03) derniers arrêtés") +
  ylab("Montant en Millions") +
  geom_text(nudge_y = 10000, size=3) +
  geom_point(
    aes(fill = NOTATION), 
    size = 3, 
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white", 
    stroke = 1 # The width of the border, i.e. stroke.
  ) +
  facet_wrap(~Indicateurs) +
  #scale_fill_viridis(discrete = TRUE) +
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%d-%m-%Y")) +
  theme(legend.position="none") +
  ggtitle("Evolution des indicateurs par type de notation sur les 3 derniers mois") +
  theme_minimal() +
  theme(axis.text.x=element_text(color = "black", size=8, angle=45, vjust=.8, hjust=0.8))
p
ggplotly(p, tooltip="text")
library(scales)
library(patchwork)
p <- final.df %>%
  filter(Pays == "Benin") %>%
  group_by(NOTATION, DATE_COMPTA) %>%
  summarise(
    TRESO = abs(sum(TRESORERIE)) / 1e6,
    SIGNAT = abs(sum(SIGNATURE)) / 1e6,
    TOTAL = abs(TRESO + SIGNAT)
  ) %>%
  gather(key = "Indicateurs", value = "Montant", 3:5) %>%
  filter(Indicateurs == "TRESO") %>%
  ggplot(aes(x=DATE_COMPTA, y=Montant, label=Montant)) +
  geom_point() +
  geom_text(nudge_y = 10000, size=3) +
  geom_line(aes(color=NOTATION)) +
  theme_minimal() +
  labs(title = "Evolution des indicateurs par type de notation sur les 3 derniers mois",
       x = "Arretés",
       y = "Montant en Millions") + 
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%m-%Y")) +
  scale_y_continuous(breaks = c(0, 100, 1000, 10000)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) 


p1 <- final.df %>%
  filter(Pays == "Benin") %>%
  group_by(NOTATION, DATE_COMPTA) %>%
  summarise(
    TRESO = abs(sum(TRESORERIE)) / 1e6,
    SIGNAT = abs(sum(SIGNATURE)) / 1e6,
    TOTAL = abs(TRESO + SIGNAT)
  ) %>%
  gather(key = "Indicateurs", value = "Montant", 3:5) %>%
  filter(Indicateurs == "TRESO") %>%
  ggplot(aes(x=DATE_COMPTA, y=Montant, label=Montant)) +
  geom_point() +
  geom_text(nudge_y = 10000, size=3) +
  geom_line(aes(color=NOTATION)) +
  theme_minimal() +
  labs(title = "Evolution des indicateurs par type de notation sur les 3 derniers mois",
       x = "Arretés",
       y = "Montant en Millions") + 
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%m-%Y")) +
  scale_y_continuous(breaks = c(0, 100, 1000, 10000)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) 


p + p1 + p

# Adding data label
ggplot(data=val, aes(x=course, y=num, group=1, label=num)) +
  geom_line()+
  geom_point()+
  geom_text(nudge_y = 3)




