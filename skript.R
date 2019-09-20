library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)

valgkretser <- read_delim("valgkretser.csv", 
                          ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()), 
                          locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)

valgkretser2015 <- read_delim("valgkretser2015.csv", 
                              ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()), 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE)%>%
                          filter(Kommunenavn == "Oslo")%>%
                          select(Stemmekretsnavn, Partikode, `Oppslutning prosentvis`)%>%
                          rename("oppslutning15" = `Oppslutning prosentvis`)
  
valgkrets_til_delbydel <- read_excel("valgkrets_til_delbydel.xlsx")
inntekt_oslo <- read_excel("inntekt.xls")

oslo <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% c("A", "FNB", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V"))%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(inntekt_oslo)%>%
  filter(!is.na(Område))%>%
  mutate(konfliktlinje = ifelse(Område == "Sentrum", NA, "Øst-Vest"))%>%
  filter(Område != "Sentrum")

ggplot(oslo, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt (uten sentrum)")


parti <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% c("A", "FNB", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V"))%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(inntekt_oslo)%>%
  filter(!is.na(Område))%>%
  filter(Partikode == "FNB")
# filter(Område == "Sentrum")%>%


ggplot(parti, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  geom_smooth(method='lm')+
  ggtitle("FNBs oppslutning i valgkretser etter inntekt")






sidene <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% c("A", "FNB", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V"))%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(valgkretser2015)%>%
  rename("oppslutning19" = `Oppslutning prosentvis`)%>%
  mutate(Side = fct_relevel(Side, "Venstresida", "Høyresida"))%>%
  group_by(Side, Stemmekretsnavn)%>%
  summarise("Oppslutning 2015" = sum(oppslutning15, na.rm = TRUE), "Oppslutning 2019" = sum(oppslutning19))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(inntekt_oslo)%>%
  mutate(`Oppslutning 2015` = ifelse(`Oppslutning 2015` == 0, NA, `Oppslutning 2015`))%>%
  gather(`Oppslutning 2015`, `Oppslutning 2019`)


ggplot(sidene, aes(x = Snittinntekt, y = `Oppslutning 2015`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Side)+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt 2015")


  









mdg <- oslo%>%
  filter(Partikode == "MDG")%>%
  filter(Område != "Sentrum")




fnb <- oslo%>%
  filter(Partikode == "FNB")

ggplot(fnb, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 5)+
  ggtitle("FNB")


høyre <- oslo%>%
  filter(Partikode == "H")

ggplot(høyre, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 5)+
  ggtitle("Høyre")


ap <- oslo%>%
  filter(Partikode == "A")

ggplot(ap, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 25)+
  ggtitle("AP")


sv <- oslo%>%
  filter(Partikode == "SV")

ggplot(sv, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 15)+
  ggtitle("SV")

rødt <- oslo%>%
  filter(Partikode == "RØDT")

ggplot(rødt, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 15)+
  ggtitle("Rødt")

frp <- oslo%>%
  filter(Partikode == "FRP")

ggplot(frp, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 800000, label.y = 15)+
  ggtitle("FRP")



forhåndsandel <- oslo%>%
  summarise(Forhåndsstemmer = sum(`Antall forhåndsstemmer`), Valgtingstemmer = sum(`Antall valgtingstemmer`), Andel = sum(`Antall forhåndsstemmer`)/sum(`Antall stemmer totalt`)*100)

stemmeandel <- oslo%>%
  filter(Partikode %in% c("A", "FNB", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V"))%>%
  group_by(Partikode)%>%
  summarise(Forhåndsstemmer = sum(`Antall forhåndsstemmer`), Valgtingstemmer = sum(`Antall valgtingstemmer`), Andel = sum(`Antall forhåndsstemmer`)/sum(`Antall stemmer totalt`)*100)%>%
  mutate(andel_forhånd = (Forhåndsstemmer/sum(Forhåndsstemmer)*100), andel_valgting = (Valgtingstemmer/sum(Valgtingstemmer)*100))%>%
  mutate(Diff = andel_valgting - andel_forhånd)
  
  
  