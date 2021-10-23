library(tidyverse)

btw_kerg <- read.csv("/Users/max/Library/Mobile Documents/com~apple~CloudDocs/AnPraktikum/AP-BTW2021/Raw Data/btw21_kerg.csv", 
                      skip = 2,
                      sep= ";")
btw_struktur <- read.csv("/Users/max/Library/Mobile Documents/com~apple~CloudDocs/AnPraktikum/AP-BTW2021/Raw Data/btw21_strukturdaten.csv",
                         skip= 8,
                         sep= ";")
btw_kerg_dirty <- btw_kerg

# Removing one column with only NA values
any(!is.na(btw_kerg$X.156))
btw_kerg$X.156 <- NULL

# Changing Column name from "gehört zu" to the actual Wahlkreisnummer see https://discord.com/channels/900747217936220200/900747392251465739/901399116343554089 for more info
names(btw_kerg)[3] <- c("Wahlkreisnummer")

# Cleaning column names from multi level to single level headers
parteinamen <- names(btw_kerg)[seq(from= 4, to= length(names(btw_kerg)), by= 4)] # Each Name is followed by three variabels


colnames_cleaned <- vector()
for (i in seq_along(parteinamen)){
 colnames_cleaned <- c(colnames_cleaned,paste(parteinamen[i],".Erst.End", sep = ""),
                              paste(parteinamen[i],".Erst.Vor", sep = ""),
                              paste(parteinamen[i], ".Zweit.End", sep = ""),
                              paste(parteinamen[i], ".Zweit.Vor", sep = ""))
}
colnames(btw_kerg)[4:211] <- colnames_cleaned

# Removing first two rows as multi level headers are no longer needed
btw_kerg <- btw_kerg[3:nrow(btw_kerg),]

# Nur Wahlkreise (ohne Bundesländer und Bund Ergebnis)
btw_kerg_wk <- btw_kerg%>% 
  filter(!is.na(Nr)) %>% 
  filter(!(Wahlkreisnummer %in% 99)) %>% 
  filter(!(Gebiet %in% "Bundesgebiet"))



# Bundesländer
btw_bundeslaender <- btw_kerg %>% 
  filter(Wahlkreisnummer == 99)

# Bundesgebiet
btw_bund <- btw_kerg %>% 
  filter(!is.na(Nr)) %>% 
  filter(!(Wahlkreisnummer %in% 99)) %>% 
  filter(Gebiet %in% "Bundesgebiet")


