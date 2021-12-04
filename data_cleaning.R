# Imports
library(tidyverse)
library(openxlsx)

# Reading in Datasets
btw_kerg <- read.csv("Raw Data/btw21_kerg.csv", 
                     skip = 2,
                     sep= ";", 
                     encoding = "UTF-8")
btw_struktur <- read.csv("Raw Data/btw21_strukturdaten.csv",
                         skip= 8,
                         sep= ";", 
                         encoding = "UTF-8",
                         dec= ",")

btw_kerg2 <- read.csv("Raw Data/btw21_kerg2.csv",
                      skip= 9,
                      sep= ";", 
                      encoding = "UTF-8",
                      dec= ",")

btw_by_brief_urne <- read.xlsx("Raw Data/wahlbezirksergebnisse_2021.xlsx", sheet = 1, startRow = 2) 
btw_by_brief_urne17 <- read.xlsx("Raw Data/wahlbezirksergebnisse_2017.xlsx", sheet = 1, startRow = 2) 

btw_kerg_dirty <- btw_kerg

btw_struktur_dirty <- btw_struktur

# BTW_KERG DATENSATZ -------------------
# Removing one column with only NA values
btw_kerg$X.156 <- NULL

# Changing Column name from "gehört zu" to the actual Wahlkreisnummer see https://discord.com/channels/900747217936220200/900747392251465739/901399116343554089 for more info
names(btw_kerg)[3] <- c("Bundesland.Nr")

# Cleaning column names from multi level to single level headers
colnames(btw_kerg)[seq(from=20, to= 51, by= 4)] <- c("CDU", "SPD", "AFD", "FDP", "LINKE", "GRÜNE",
                                                     "CSU", "FW")
parteinamen <- names(btw_kerg)[seq(from= 4, to= length(names(btw_kerg)), by= 4)] # Each Name is followed by three variabels



colnames_cleaned <- vector()
for (i in seq_along(parteinamen)){
  colnames_cleaned <- c(colnames_cleaned,paste(parteinamen[i],".Erst.End", sep = ""),
                        paste(parteinamen[i],".Erst.Vor", sep = ""),
                        paste(parteinamen[i], ".Zweit.End", sep = ""),
                        paste(parteinamen[i], ".Zweit.Vor", sep = ""))
}
colnames(btw_kerg)[4:211] <- colnames_cleaned

colnames(btw_kerg)[1] <- "Wahlkreis.Nr"

# Changing Character Number columns to correct type numeric
btw_kerg[,3:ncol(btw_kerg)] <- lapply(btw_kerg[3:ncol(btw_kerg)], as.numeric) # NAs are generated because some districts do not have Numbers (="") How should we deal with this?

# Creating an additional Data Frame containing only the main parties, with other parties stored as "Sonstige"

# 52 ist the column where the "non main" parties start
Sonstige.Erst.End <- btw_kerg[seq(from= 52, to= ncol(btw_kerg), by= 4)] %>%
  mutate("Sonstige.Erst.End"= rowSums(.[1:ncol(.)],na.rm= TRUE)) %>% 
  select("Sonstige.Erst.End")

Sonstige.Erst.Vor <- btw_kerg[seq(from= 53, to= ncol(btw_kerg), by=4)] %>%
  mutate("Sonstige.Erst.Vor"= rowSums(.[1:ncol(.)],na.rm= TRUE)) %>% 
  select("Sonstige.Erst.Vor") 

Sonstige.Zweit.End <- btw_kerg[seq(from= 54, to= ncol(btw_kerg), by=4)] %>%
  mutate("Sonstige.Zweit.End"= rowSums(.[1:ncol(.)],na.rm= TRUE)) %>% 
  select("Sonstige.Zweit.End") 

Sonstige.Zweit.Vor <- btw_kerg[seq(from= 55, to= ncol(btw_kerg), by=4)] %>%
  mutate("Sonstige.Zweit.Vor"= rowSums(.[1:ncol(.)],na.rm= TRUE)) %>% 
  select("Sonstige.Zweit.Vor")

btw_kerg_trimmed <- btw_kerg %>% 
  add_column("Sonstige.Zweit.Vor"= Sonstige.Zweit.Vor, .after = colnames(btw_kerg[51])) %>% 
  add_column("Sonstige.Zweit.End"= Sonstige.Zweit.End, .after = colnames(btw_kerg[51])) %>%
  add_column("Sonstige.Erst.Vor"= Sonstige.Erst.Vor, .after = colnames(btw_kerg[51])) %>% 
  add_column("Sonstige.Erst.End"= Sonstige.Erst.End, .after = colnames(btw_kerg[51])) %>% 
  select(1:55)

# Removing first two rows as multi level headers are no longer needed
btw_kerg <- btw_kerg[3:nrow(btw_kerg),]
btw_kerg_trimmed <- btw_kerg_trimmed[3:nrow(btw_kerg_trimmed),]

# Adding useful columns which are not in the set by calculating them
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("Wahlbeteiligung.Erst.End" = (btw_kerg_trimmed[["Wählende.Erst.End"]] / btw_kerg_trimmed[["Wahlberechtigte.Erst.End"]]),
         "Wahlbeteiligung.Zweit.End"= (btw_kerg_trimmed[["Wählende.Zweit.End"]]/btw_kerg_trimmed[["Wahlberechtigte.Zweit.End"]]),
         .after = "Gültige.Stimmen.Zweit.Vor")

#### CDU
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("CDU.Erst.End.Perc"= btw_kerg_trimmed[["CDU.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "CDU.Zweit.End.Perc"= btw_kerg_trimmed[["CDU.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("CDU.Zweit.Vor"))

#### SPD
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("SPD.Erst.End.Perc"= btw_kerg_trimmed[["SPD.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "SPD.Zweit.End.Perc"= btw_kerg_trimmed[["SPD.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("SPD.Zweit.Vor"))

#### AFD
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("AFD.Erst.End.Perc"= btw_kerg_trimmed[["AFD.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "AFD.Zweit.End.Perc"= btw_kerg_trimmed[["AFD.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("AFD.Zweit.Vor"))

#### FDP
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("FDP.Erst.End.Perc"= btw_kerg_trimmed[["FDP.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "FDP.Zweit.End.Perc"= btw_kerg_trimmed[["FDP.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("FDP.Zweit.Vor"))

#### LINKE
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("LINKE.Erst.End.Perc"= btw_kerg_trimmed[["LINKE.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "LINKE.Zweit.End.Perc"= btw_kerg_trimmed[["LINKE.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("LINKE.Zweit.Vor"))

#### GRÜNE
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("GRÜNE.Erst.End.Perc"= btw_kerg_trimmed[["GRÜNE.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "GRÜNE.Zweit.End.Perc"= btw_kerg_trimmed[["GRÜNE.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("GRÜNE.Zweit.Vor"))

#### CSU 
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("CSU.Erst.End.Perc"= btw_kerg_trimmed[["CSU.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "CSU.Zweit.End.Perc"= btw_kerg_trimmed[["CSU.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("CSU.Zweit.Vor"))

#### FW
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("FW.Erst.End.Perc"= btw_kerg_trimmed[["FW.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "FW.Zweit.End.Perc"= btw_kerg_trimmed[["FW.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("FW.Zweit.Vor"))

#### Sonstige
btw_kerg_trimmed <- btw_kerg_trimmed %>% 
  mutate("Sonstige.Erst.End.Perc"= btw_kerg_trimmed[["Sonstige.Erst.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         "Sonstige.Zweit.End.Perc"= btw_kerg_trimmed[["Sonstige.Zweit.End"]]/btw_kerg_trimmed[["Wählende.Erst.End"]],
         .after= c("Sonstige.Zweit.Vor"))


# Nur Wahlkreise (ohne Bundesländer und Bund Ergebnis)
btw_kerg_wk <- btw_kerg%>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(Bundesland.Nr %in% 99)) %>% 
  filter(!(Gebiet %in% "Bundesgebiet"))

btw_kerg_trimmed_wk <- btw_kerg_trimmed%>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(Bundesland.Nr %in% 99)) %>% 
  filter(!(Gebiet %in% "Bundesgebiet"))



# Bundesländer
btw_kerg_bundeslaender <- btw_kerg %>% 
  filter(Bundesland.Nr == 99)

btw_kerg_trimmed_bundeslaender <- btw_kerg_trimmed %>% 
  filter(Bundesland.Nr == 99)

saveRDS(btw_kerg_trimmed_bundeslaender,"btw_kerg_trimmed_bundeslaender.RDS")
# Bundesgebiet
btw_kerg_bund <- btw_kerg %>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(Bundesland.Nr %in% 99)) %>% 
  filter(Gebiet %in% "Bundesgebiet")

btw_kerg_trimmed_bund <- btw_kerg_trimmed %>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(Bundesland.Nr %in% 99)) %>% 
  filter(Gebiet %in% "Bundesgebiet")

saveRDS(btw_kerg_trimmed_bund,"btw_kerg_trimmed_bund.RDS")
# BTW_STRUKTUR DATENSATZ -------------------
footnotes <- btw_struktur$Fußnoten


btw_struktur$Fußnoten <- NULL

btw_struktur[["Fläche.am.31.12.2019..km.."]] <- gsub("\\.","",btw_struktur[["Fläche.am.31.12.2019..km²."]])
btw_struktur[["Fläche.am.31.12.2019..km.."]] <- gsub("\\,",".",btw_struktur[["Fläche.am.31.12.2019..km²."]])
btw_struktur[,c(2,4:ncol(btw_struktur))] <- lapply(btw_struktur[c(2,4:ncol(btw_struktur))], as.numeric)


clean_colnames_btw_struktur <- c("Land",
                                 "Wahlkreis.Nr",
                                 "Wahlkreis.Name",
                                 "Gmd.Anz",
                                 "Flaeche.Km",
                                 "Bvk.Insg",
                                 "Bvk.Dt",
                                 "Bvk.Aus",
                                 "Bvk.Dcht",
                                 "Gbrt.Saldo",
                                 "Wander.Saldo",
                                 "u18.Perctange",
                                 "f18t24.Perctange",
                                 "f25t34.Perctange",
                                 "f35t59.Perctange",
                                 "f60t74.Perctange",
                                 "f75tInf.Perctange",
                                 "Boden.SieduVerk",
                                 "Boden.VegeuGewaes",
                                 "Fertig.Wohnungen",
                                 "Best.Wohnungen",
                                 "Flaech.Wohn",
                                 "WohnFlaech.EW",
                                 "Best.PKW",
                                 "Best.PkwElek",
                                 "Unternehmen.Insg",
                                 "Unternehmen.HW",
                                 "Schulab.berufSch",
                                 "Schulab.allgSch",
                                 "Schulab.ohneHS",
                                 "Schulab.mitHS",
                                 "Schulab.mitMittSA",
                                 "Schulab.mitAllgHS",
                                 "KTG.KinderU3",
                                 "KTG.KinderF3t6",
                                 "Vfg.Einkommen",
                                 "BIP",
                                 "SozPfli.Insg",
                                 "SozPfli.LandW",
                                 "SozPfli.ProdGewerb",
                                 "SozPfli.HandelGwVerk",
                                 "SozPfli.ÖffPrivDienstl",
                                 "SozPfli.Rest",
                                 "SGB2.Empf.Insg",
                                 "SGB2.Empf.nErwHilf",
                                 "SGB2.Empf.Aus",
                                 "ArbeitslosQ",
                                 "ArbeitslosQ.M",
                                 "ArbeitslosQ.W",
                                 "ArbeitslosQ.f15t24",
                                 "ArbeitslosQ.f55t64"
)


colnames(btw_struktur) <- clean_colnames_btw_struktur


btw_struk_wk <- btw_struktur %>% 
  filter(Wahlkreis.Nr<300) %>% 
  arrange(Wahlkreis.Nr)

# Länderebene
btw_struk_laender <- btw_struktur %>% 
  filter(Wahlkreis.Nr>900 & Wahlkreis.Nr<917) %>% 
  arrange(Wahlkreis.Nr)

btw_struk_laender$Wahlkreis.Name <- NULL
colnames(btw_struk_laender)[2] <- "Bundesland.Nr."
btw_struk_laender[["Bundesland.Nr."]] <- 1:16

# Bundesebene
btw_struk_bund <- btw_struktur %>%
  filter(Wahlkreis.Nr>950)

btw_struk_bund$Wahlkreis.Nr <- NULL
btw_struk_bund$Wahlkreis.Name <- NULL


# Creating one big Dataset with key Nr 


btw_data <- left_join(btw_kerg_wk, btw_struk_wk, by= "Wahlkreis.Nr")%>% 
  select(-c("Land", "Wahlkreis.Name"))

btw_trimmed_data <- left_join(btw_kerg_trimmed_wk, btw_struk_wk, by= "Wahlkreis.Nr") %>% 
  select(-c("Land", "Wahlkreis.Name"))

# Plausibilitätschecks
# Endgültige Erststimmen und Gültige Stimme
all.equal(rowSums(btw_data[seq(from=20,to=208, by=4)],na.rm = TRUE), btw_data[["Gültige.Stimmen.Erst.End"]])

# Endgültige Zweitstimmen ung Gültige Stimmen
all.equal(rowSums(btw_data[seq(from=22,to=210, by=4)],na.rm = TRUE), btw_data[["Gültige.Stimmen.Zweit.End"]])

# CSU nur in Bayern
btw_data %>% 
  filter(!is.na(CSU.Erst.End)) %>% 
  select(Bundesland.Nr, Wahlkreis.Nr, Gebiet)

btw_data %>% 
  filter(!is.na(CSU.Erst.End)) %>% 
  select(Bundesland.Nr, Wahlkreis.Nr, Gebiet) %>% 
  nrow()

btw_data %>% 
  filter(!is.na(CSU.Erst.End)) %>% 
  select(Bundesland.Nr)

saveRDS(btw_data, file= "btw_data.RDS")
saveRDS(btw_trimmed_data, file= "btw_trimmed_data.RDS")

# BTW_KERG2 DATENSATZ -------------------

# Removing column with all NAs
all(is.na(btw_kerg2[["Bemerkung"]]))

btw_kerg2[["Bemerkung"]] <- NULL


# Splitte nach Gebietsarten

btw_kerg2_bund <- btw_kerg2 %>%
  filter(Gebietsart == "Bund")

btw_kerg2_land <- btw_kerg2 %>%
  filter(Gebietsart == "Land")

btw_kerg2_wk <- btw_kerg2 %>%
  filter(Gebietsart == "Wahlkreis")

colnames(btw_kerg2_wk)[4] <- "Wahlkreis.Nr"
saveRDS(btw_kerg2_wk, file= "btw_kerg2_wk.RDS")
saveRDS(btw_kerg2_bund, file= "btw_kerg2_bund.RDS")

# Briefwahldaten ----
# Removing and summarizing sontige columns
btw_by_brief_urne <- btw_by_brief_urne %>% 
  mutate(D.Sonstige= rowSums(btw_by_brief_urne[20:71], na.rm = TRUE)) %>% 
  mutate(F.Sonstige= rowSums(btw_by_brief_urne[81:99], na.rm = TRUE)) %>% 
  select(-c(20:71, 81:99))

# Making sure daty type is correct
btw_by_brief_urne[1:ncol(btw_by_brief_urne)] <- lapply(btw_by_brief_urne[1:ncol(btw_by_brief_urne)], as.numeric)

# Aggregation auf Gemeindeebene und Wahlkreisebene
btw_by_brief_urne_wk <- btw_by_brief_urne %>% 
  group_by(schluessel) %>% 
  summarise_each(list(sum))

btw_by_brief_urne_Gde <- btw_by_brief_urne %>% 
  group_by(schluessel.Gde) %>% 
  summarise_each(list(sum))

saveRDS(btw_by_brief_urne_wk, file= "btw_by_brief_urne_wk.RDS")
saveRDS(btw_by_brief_urne_Gde, file= "btw_by_brief_urne_Gde.RDS")

# Analog for 2017 Dataset

btw_by_brief_urne17[1:ncol(btw_by_brief_urne17)] <- lapply(btw_by_brief_urne17[1:ncol(btw_by_brief_urne17)], as.numeric)

btw_by_brief_urne17 <- btw_by_brief_urne17 %>% 
  mutate(D.Sonstige= rowSums(btw_by_brief_urne17[20:48], na.rm = TRUE)) %>% 
  mutate(F.Sonstige= rowSums(btw_by_brief_urne17[58:71], na.rm = TRUE)) %>% 
  select(-c(20:48, 58:71))

btw_by_brief_urne_17_wk <- btw_by_brief_urne17 %>% 
  group_by(schluessel.Wkr) %>% 
  summarise_each(list(sum))

btw_by_brief_urne_17_Gde <- btw_by_brief_urne17 %>% 
  group_by(schluessel.Gde) %>% 
  summarise_each(list(sum))


btw_brief_urne_all <- left_join(btw_by_brief_urne_wk, btw_by_brief_urne_17_wk, by= c("schluessel"="schluessel.Wkr"), suffix= c("_21", "_17"))

saveRDS(btw_by_brief_urne_17_wk, file = "btw_by_brief_urne_17_wk.RDS")
saveRDS(btw_by_brief_urne_17_Gde, file = "btw_by_brief_urne_17_Gde.RDS")
saveRDS(btw_brief_urne_all, file = "btw_brief_urne_all.RDS")
