library(tidyverse)

btw_kerg <- read.csv("https://raw.githubusercontent.com/MaxMLang/AP-BTW2021/main/Raw%20Data/btw21_kerg.csv?token=APSDNIIFGJAG7KMIRFP36UTBPU4TK", 
                      skip = 2,
                      sep= ";")
btw_struktur <- read.csv("https://raw.githubusercontent.com/MaxMLang/AP-BTW2021/main/Raw%20Data/btw21_strukturdaten.csv?token=APSDNIOU55YWJU5VMI7QD73BPU4UY",
                         skip= 8,
                         sep= ";")
btw_kerg_dirty <- btw_kerg
btw_struktur_dirty <- btw_struktur

# BTW_KERG DATENSATZ -------------------
# Removing one column with only NA values
any(!is.na(btw_kerg$X.156))
btw_kerg$X.156 <- NULL

# Changing Column name from "gehört zu" to the actual Wahlkreisnummer see https://discord.com/channels/900747217936220200/900747392251465739/901399116343554089 for more info
names(btw_kerg)[3] <- c("WahlkreisnummerGrob")

# Cleaning column names from multi level to single level headers
parteinamen <- names(btw_kerg)[seq(from= 4, to= length(names(btw_kerg)), by= 4)] # Each Name is followed by three variabels

kern_partein_names_dirty <- c("Christlich.Demokratische.Union.Deutschlands",
                              "Sozialdemokratische.Partei.Deutschlands",
                              "Alternative.für.Deutschland",
                              "Freie.Demokratische.Partei",
                              "DIE.LINKE",
                              "BÜNDNIS.90.DIE.GRÜNEN",
                              "Christlich.Soziale.Union.in.Bayern.e.V.",
                              "FREIE.WÄHLER")

kern_partein_names_clean <- c("CDU",
                              "SPD",
                              "AFD",
                              "FDP",
                              "Die_Linke",
                              "Die_Gruenen",
                              "CSU",
                              "Freie_Waehler")

colnames_cleaned <- vector()
for (i in seq_along(parteinamen)){
 colnames_cleaned <- c(colnames_cleaned,paste(parteinamen[i],".Erst.End", sep = ""),
                              paste(parteinamen[i],".Erst.Vor", sep = ""),
                              paste(parteinamen[i], ".Zweit.End", sep = ""),
                              paste(parteinamen[i], ".Zweit.Vor", sep = ""))
}
colnames(btw_kerg)[4:211] <- colnames_cleaned

colnames(btw_kerg)[1] <- "Wahlkreis.Nr"
# Removing first two rows as multi level headers are no longer needed
btw_kerg <- btw_kerg[3:nrow(btw_kerg),]

# Nur Wahlkreise (ohne Bundesländer und Bund Ergebnis)
btw_kerg_wk <- btw_kerg%>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(WahlkreisnummerGrob %in% 99)) %>% 
  filter(!(Gebiet %in% "Bundesgebiet"))



# Bundesländer
btw_kerg_bundeslaender <- btw_kerg %>% 
  filter(WahlkreisnummerGrob == 99)

# Bundesgebiet
btw_kerg_bund <- btw_kerg %>% 
  filter(!is.na(Wahlkreis.Nr)) %>% 
  filter(!(WahlkreisnummerGrob %in% 99)) %>% 
  filter(Gebiet %in% "Bundesgebiet")

# BTW_STRUKTUR DATENSATZ -------------------
footnotes <- btw_struktur$Fußnoten


btw_struktur$Fußnoten <- NULL

 
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
btw_data <- left_join(btw_kerg_wk, btw_struk_wk, by= "Wahlkreis.Nr")
