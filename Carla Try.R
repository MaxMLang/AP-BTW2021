library(tidyverse)

btw_trimmed_data %>%
  ggplot() +
  geom_smooth(aes(x = Vfg.Einkommen, y = LINKE.Zweit.End), color = "red") +
  geom_smooth(aes(x = Vfg.Einkommen, y = FDP.Zweit.End), color = "yellow") +
  geom_point(aes(x = Vfg.Einkommen, y = LINKE.Zweit.End), color = "red") +
  geom_point(aes(x = Vfg.Einkommen, y = FDP.Zweit.End), color = "yellow")

summary(btw_trimmed_data)


## Welche Partei ist mit welchem Merkmal am stärksten korreliert?
cor_data <- cor(select(btw_trimmed_data, c(CDU.Zweit.End,
                                           SPD.Zweit.End,
                                           AFD.Zweit.End,
                                           FDP.Zweit.End,
                                           GRÜNE.Zweit.End,
                                           LINKE.Zweit.End,
                                           )))
cor_data
## Alle Parteien zueinander korrelieren