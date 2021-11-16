library(tidyverse)
library('patchwork')
## SPD vs CDU

plot1 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc > CDU.Zweit.End.Perc | SPD.Zweit.End.Perc > CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Vfg.Einkommen)) +
  geom_boxplot() +
  ylim(15000, 35000)


plot2 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc < CDU.Zweit.End.Perc | SPD.Zweit.End.Perc < CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Vfg.Einkommen)) +
  geom_boxplot() +
  ylim(15000, 35000)

x <- plot1 + plot2 + plot_annotation(title = "Verfügbares Einkommen")

plot3 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc > CDU.Zweit.End.Perc | SPD.Zweit.End.Perc > CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Bvk.Dcht)) +
  geom_boxplot() +
  ylim(0, 15000)

plot4 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc < CDU.Zweit.End.Perc | SPD.Zweit.End.Perc < CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Bvk.Dcht)) +
  geom_boxplot() +
  ylim(0, 15000)

plot3 + plot4

plot3 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc > CDU.Zweit.End.Perc | SPD.Zweit.End.Perc > CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Bvk.Aus)) +
  geom_boxplot() + 
  ylim(0,35)

plot4 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc < CDU.Zweit.End.Perc | SPD.Zweit.End.Perc < CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Bvk.Aus)) +
  geom_boxplot() + 
  ylim(0,35)

plot3 + plot4

plot3 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc > CDU.Zweit.End.Perc | SPD.Zweit.End.Perc > CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Schulab.mitAllgHS)) +
  geom_boxplot() + 
  ylim(0,60)

plot4 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc < CDU.Zweit.End.Perc | SPD.Zweit.End.Perc < CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = Schulab.mitAllgHS)) +
  geom_boxplot() + 
  ylim(0,60)

plot3 + plot4


plot3 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc > CDU.Zweit.End.Perc | SPD.Zweit.End.Perc > CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = ArbeitslosQ)) +
  geom_boxplot()

plot4 <- btw_trimmed_data %>%
  filter(SPD.Zweit.End.Perc < CDU.Zweit.End.Perc | SPD.Zweit.End.Perc < CSU.Zweit.End.Perc) %>%
  ggplot(aes(y = ArbeitslosQ)) +
  geom_boxplot()

a <- plot3 + plot4

## Erst vs. Zweitstimmen

## Bzw. Map Differenz
Balkendiagramm
btw_kerg_trimmed %>%
  ggplot() +
  geom_bar()


## E
## Altersgruppen Scatterplots große Parteien

btw_trimmed_data %>%
  select(c(CDU.Zweit.End.Perc,
           SPD.Zweit.End.Perc,
           AFD.Zweit.End.Perc,
           FDP.Zweit.End.Perc,
           GRÜNE.Zweit.End.Perc,
           LINKE.Zweit.End.Perc,
           f18t24.Perctange,
           f25t34.Perctange,
           f35t59.Perctange,
           f60t74.Perctange,
           f75tInf.Perctange)
           ) %>%
  pivot_longer(f18t24.Perctange:f75tInf.Perctange, names_to = "Age", values_to = "Percentage") %>%
  ggplot(aes(x = Percentage)) +
  geom_smooth(aes(y = CDU.Zweit.End.Perc), color = "black") +
  geom_smooth(aes(y = SPD.Zweit.End.Perc), color = "red") +
  geom_smooth(aes(y = AFD.Zweit.End.Perc), color = "blue") +
  geom_smooth(aes(y = GRÜNE.Zweit.End.Perc), color = "green") +
  geom_smooth(aes(y = LINKE.Zweit.End.Perc), color = "purple") +
  geom_smooth(aes(y = FDP.Zweit.End.Perc), color = "yellow") +
  facet_wrap("Age")


  
btw_trimmed_data %>%
  select(c(CDU.Zweit.End,
           SPD.Zweit.End,
           AFD.Zweit.End,
           FDP.Zweit.End,
           GRÜNE.Zweit.End,
           LINKE.Zweit.End,
           Schulab.berufSch,
           Schulab.allgSch,
           Schulab.ohneHS,
           Schulab.mitHS,
           Schulab.mitMittSA,
           Schulab.mitAllgHS)) %>%
  pivot_longer(Schulab.berufSch:Schulab.mitAllgHS, names_to = "Abschluss", values_to = "Percentage") %>%
  ggplot(aes(x = Percentage)) +
  geom_smooth(aes(y = CDU.Zweit.End), color = "black") +
  geom_smooth(aes(y = SPD.Zweit.End), color = "red") +
  geom_smooth(aes(y = AFD.Zweit.End), color = "blue") +
  geom_smooth(aes(y = GRÜNE.Zweit.End), color = "green") +
  geom_smooth(aes(y = LINKE.Zweit.End), color = "purple") +
  geom_smooth(aes(y = FDP.Zweit.End), color = "yellow") +
  facet_wrap("Abschluss")

btw_trimmed_data %>%
  select(c(CDU.Zweit.End,
           SPD.Zweit.End,
           AFD.Zweit.End,
           FDP.Zweit.End,
           GRÜNE.Zweit.End,
           LINKE.Zweit.End,
           SozPfli.LandW,
           SozPfli.ProdGewerb,
           SozPfli.HandelGwVerk,
           SozPfli.ÖffPrivDienstl,
           SozPfli.Rest)) %>%
  pivot_longer(SozPfli.LandW:SozPfli.Rest, names_to = "Beschäftigung", values_to = "Percentage") %>%
  ggplot(aes(x = Percentage)) +
  geom_smooth(aes(y = CDU.Zweit.End), color = "black") +
  geom_smooth(aes(y = SPD.Zweit.End), color = "red") +
  geom_smooth(aes(y = AFD.Zweit.End), color = "blue") +
  geom_smooth(aes(y = GRÜNE.Zweit.End), color = "green") +
  geom_smooth(aes(y = LINKE.Zweit.End), color = "purple") +
  geom_smooth(aes(y = FDP.Zweit.End), color = "yellow") +
  facet_wrap("Beschäftigung")

btw_trimmed_data %>%
  ggplot(aes(x = SozPfli.LandW)) +
  geom_point(aes(y = GRÜNE.Zweit.End), color = "green") +
  geom_smooth(aes(y = GRÜNE.Zweit.End), color = "green")

btw_trimmed_data %>%
  ggplot(aes(x = Bvk.Dcht)) +
  geom_point(aes(y = GRÜNE.Zweit.End), color = "green") +
  geom_smooth(aes(y = GRÜNE.Zweit.End), color = "green")


  btw_trimmed_data %>%
  select(c(Sonstige.Zweit.End,
           f18t24.Perctange,
           f25t34.Perctange,
           f35t59.Perctange,
           f60t74.Perctange,
           f75tInf.Perctange)
  ) %>%
  pivot_longer(f18t24.Perctange:f75tInf.Perctange, names_to = "Age", values_to = "Percentage") %>%
  ggplot(aes(x = Percentage)) +
  geom_smooth(aes(y = Sonstige.Zweit.End), color = "black") +
  facet_wrap("Age")

## Altersgruppen Sonstige

  
  
## Arbeitslosengeld

btw_trimmed_data %>%
  ggplot(aes(x = ArbeitslosQ)) +
  geom_smooth(aes(y = CDU.Zweit.End.Perc), color = "black") +
  geom_smooth(aes(y = SPD.Zweit.End.Perc), color = "red") +
  geom_smooth(aes(y = AFD.Zweit.End.Perc), color = "blue") +
  geom_smooth(aes(y = GRÜNE.Zweit.End.Perc), color = "green") +
  geom_smooth(aes(y = LINKE.Zweit.End.Perc), color = "purple") +
  geom_smooth(aes(y = FDP.Zweit.End.Perc), color = "yellow") +
  geom_point(aes(y = CDU.Zweit.End.Perc), color = "black", size = 1) +
  geom_point(aes(y = SPD.Zweit.End.Perc), color = "red", size = 1) +
  geom_point(aes(y = AFD.Zweit.End.Perc), color = "blue", size = 1) +
  geom_point(aes(y = GRÜNE.Zweit.End.Perc), color = "green", size = 1) +
  geom_point(aes(y = LINKE.Zweit.End.Perc), color = "purple", size = 1) +
  geom_point(aes(y = FDP.Zweit.End.Perc), color = "yellow", size = 1)

btw_trimmed_data %>%
  select(c(CDU.Zweit.End.Perc,
           SPD.Zweit.End.Perc,
           AFD.Zweit.End.Perc,
           FDP.Zweit.End.Perc,
           GRÜNE.Zweit.End.Perc,
           LINKE.Zweit.End.Perc,
           ArbeitslosQ.M,
           ArbeitslosQ.W)
  ) %>%
  pivot_longer(c(ArbeitslosQ.M, ArbeitslosQ.W), names_to = "Age", values_to = "Percentage") %>%
  ggplot(aes(x = Percentage)) +
  geom_smooth(aes(y = CDU.Zweit.End.Perc), color = "black") +
  geom_smooth(aes(y = SPD.Zweit.End.Perc), color = "red") +
  geom_smooth(aes(y = AFD.Zweit.End.Perc), color = "blue") +
  geom_smooth(aes(y = GRÜNE.Zweit.End.Perc), color = "green") +
  geom_smooth(aes(y = LINKE.Zweit.End.Perc), color = "purple") +
  geom_smooth(aes(y = FDP.Zweit.End.Perc), color = "yellow") +
  facet_wrap("Age")


## Einkommen und Wahlverhalten

library(tidyverse)

btw_trimmed_data %>%
  ggplot() +
  geom_smooth(aes(x = Vfg.Einkommen, y = LINKE.Zweit.End), color = "red") +
  geom_smooth(aes(x = Vfg.Einkommen, y = FDP.Zweit.End), color = "yellow") +
  geom_point(aes(x = Vfg.Einkommen, y = LINKE.Zweit.End), color = "red") +
  geom_point(aes(x = Vfg.Einkommen, y = FDP.Zweit.End), color = "yellow")

summary(btw_trimmed_data)




## Boxplots

## welche Partei ist am beliebtesten filtern und dann pro Partei Boxplot mit Einkommen in entsprechenden Wahlkreisen
## welche Variablen verändern Wahlergebnis, Vergleich zum Vorjahr

## Wie viele wählen Parteien, die nicht im Bundestag sind

## Welche Partei ist mit welchem Merkmal am stärksten korreliert?
cor_data <- cor(select(btw_trimmed_data, c(CDU.Zweit.End,
                                           SPD.Zweit.End,
                                           AFD.Zweit.End,
                                           FDP.Zweit.End,
                                           GRÜNE.Zweit.End,
                                           LINKE.Zweit.End,
                                           Gmd.Anz,
                                           Flaeche.Km,
                                           Bvk.Insg,
                                           Bvk.Dt,
                                           Bvk.Aus,
                                           Bvk.Dcht,
                                           Gbrt.Saldo,
                                           Wander.Saldo,
                                           u18.Perctange,
                                           f18t24.Perctange,
                                           f25t34.Perctange,
                                           f35t59.Perctange,
                                           f60t74.Perctange,
                                           f75tInf.Perctange,
                                           Boden.SieduVerk,
                                           Boden.VegeuGewaes,
                                           Fertig.Wohnungen,
                                           Best.Wohnungen,
                                           Flaech.Wohn,
                                           WohnFlaech.EW,
                                           Best.PKW,
                                           Best.PkwElek,
                                           Unternehmen.Insg,
                                           Unternehmen.HW,
                                           Schulab.berufSch,
                                           Schulab.allgSch,
                                           Schulab.ohneHS,
                                           Schulab.mitHS,
                                           Schulab.mitMittSA,
                                           Schulab.mitAllgHS,
                                           KTG.KinderU3,
                                           KTG.KinderF3t6,
                                           Vfg.Einkommen,
                                           BIP,
                                           SozPfli.Insg,
                                           SozPfli.LandW,
                                           SozPfli.ProdGewerb,
                                           SozPfli.HandelGwVerk,
                                           SozPfli.ÖffPrivDienstl,
                                           SozPfli.Rest,
                                           SGB2.Empf.Insg,
                                           SGB2.Empf.nErwHilf,
                                           SGB2.Empf.Aus,
                                           ArbeitslosQ,
                                           ArbeitslosQ.M,
                                           ArbeitslosQ.W,
                                           ArbeitslosQ.f15t24,
                                           ArbeitslosQ.f55t64
                                           )))
cor_data <- cor_data[-c(1:6), c(1:6)]

cor_data[[1]]

CSU.max <- max(cor_data[[1]])
## Alle Parteien zueinander korrelieren

CSU.max

cor_data
