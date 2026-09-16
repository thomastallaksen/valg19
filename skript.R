library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)

# Importerer data

valgkretser <- read_delim("data/valgkretser.csv",
                          ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                          locale = locale(decimal_mark = ","),
                          trim_ws = TRUE)

valgkretser2015 <- read_delim("data/valgkretser2015.csv",
                              ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                              locale = locale(decimal_mark = ","),
                              trim_ws = TRUE)%>%
                          filter(Kommunenavn == "Oslo")%>%
                          select(Stemmekretsnavn, Partikode, `Oppslutning prosentvis`)%>%
                          rename("oppslutning15" = `Oppslutning prosentvis`)

valgkrets_til_delbydel <- read_excel("data/valgkrets_til_delbydel.xlsx")
inntekt_oslo <- read_excel("data/inntekt.xls")

# 2025-data (stortingsvalg). Se README for hvordan disse er hentet og hvordan
# valgkretsene er koblet til delbydeler etter at flere stemmesteder ble
# lagt ned/erstattet siden 2019.
valgkretser2025 <- read_delim("data/valgkretser2025.csv",
                              ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                              locale = locale(decimal_mark = ","),
                              trim_ws = TRUE)

valgkrets_til_delbydel2025 <- read_excel("data/valgkrets_til_delbydel2025.xlsx")
omradedata2025 <- read_excel("data/omradedata2025.xlsx")


# Koden som genererer figuren som sammenlikner partiene

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


# Her kan man generere oversikter over enkeltpartier

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



# Denne koden er brukt til å sammenlikne venstresida og høyresida i 2015 og 2019


sidene <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% c("A", "FNB", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V"))%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(valgkretser2015)%>%
  rename("oppslutning19" = `Oppslutning prosentvis`)%>%
  mutate(Side = fct_relevel(Side, "Venstresida", "Høyresida"))%>%
  group_by(Side, Stemmekretsnavn)%>%
  summarise("Oppslutning 2015" = sum(oppslutning15, na.rm = TRUE), "Oppslutning 2019" = sum(oppslutning19), .groups = "drop")%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(inntekt_oslo)%>%
  mutate(`Oppslutning 2015` = ifelse(`Oppslutning 2015` == 0, NA, `Oppslutning 2015`))


ggplot(sidene, aes(x = Snittinntekt, y = `Oppslutning 2019`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Side)+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt 2019")


# ---------------------------------------------------------------------------
# 2025 (stortingsvalg)
#
# Stemmekretsene er ikke identiske med 2019: en del stemmesteder er lagt ned,
# flyttet eller slått sammen. valgkrets_til_delbydel2025.xlsx er en oppdatert
# kobling som tar høyde for dette (se README). To nye, perifere kretser
# (Maridalen skole og Sørkedalen kirkestue) mangler delbydelsdata og faller
# derfor ut automatisk her, på samme måte som "Oslo rådhus"/Sentrum alltid har
# gjort i 2015/2019-analysen.

partier <- c("A", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V")

oslo2025 <- valgkretser2025%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel2025)%>%
  left_join(omradedata2025)%>%
  filter(!is.na(Område))%>%
  filter(Område != "Sentrum")

ggplot(oslo2025, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt 2025 (uten sentrum)")

ggplot(oslo2025, aes(x = Innvandrerandel, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter innvandrerandel 2025 (uten sentrum)")

ggplot(oslo2025, aes(x = AndelHoyereUtdanning, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter andel med høyere utdanning 2025 (uten sentrum)")


# Venstresida/høyresida over tid (2015, 2019, 2025), på delbydelsnivå siden
# stemmekretsene ikke er direkte sammenlignbare mellom 2019 og 2025.
# Inntekten på x-aksen er samtidig med hvert valg (dvs. ikke justert for
# prisvekst), så figuren viser sammenhengen mellom lokal inntekt og
# oppslutning på det tidspunktet, ikke endring i realinntekt.

venstre_hoyre_2015 <- valgkretser2015%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  group_by(Side, Stemmekretsnavn)%>%
  summarise(Oppslutning = sum(oppslutning15, na.rm = TRUE), .groups = "drop")%>%
  left_join(valgkrets_til_delbydel)%>%
  filter(!is.na(Delbydel))%>%
  group_by(Side, Delbydel)%>%
  summarise(Oppslutning = mean(Oppslutning, na.rm = TRUE), .groups = "drop")%>%
  left_join(inntekt_oslo)%>%
  mutate(År = "2015")

venstre_hoyre_2019 <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  group_by(Side, Stemmekretsnavn)%>%
  summarise(Oppslutning = sum(`Oppslutning prosentvis`, na.rm = TRUE), .groups = "drop")%>%
  left_join(valgkrets_til_delbydel)%>%
  filter(!is.na(Delbydel))%>%
  group_by(Side, Delbydel)%>%
  summarise(Oppslutning = mean(Oppslutning, na.rm = TRUE), .groups = "drop")%>%
  left_join(inntekt_oslo)%>%
  mutate(År = "2019")

venstre_hoyre_2025 <- valgkretser2025%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  group_by(Side, Stemmekretsnavn)%>%
  summarise(Oppslutning = sum(`Oppslutning prosentvis`, na.rm = TRUE), .groups = "drop")%>%
  left_join(valgkrets_til_delbydel2025)%>%
  filter(!is.na(Delbydel))%>%
  group_by(Side, Delbydel)%>%
  summarise(Oppslutning = mean(Oppslutning, na.rm = TRUE), .groups = "drop")%>%
  left_join(omradedata2025)%>%
  select(Side, Delbydel, Oppslutning, Snittinntekt)%>%
  mutate(År = "2025")

venstre_hoyre_alle_ar <- bind_rows(venstre_hoyre_2015, venstre_hoyre_2019, venstre_hoyre_2025)%>%
  ungroup()%>%
  mutate(Side = fct_relevel(Side, "Venstresida", "Høyresida"))

ggplot(venstre_hoyre_alle_ar, aes(x = Snittinntekt, y = Oppslutning))+
  geom_point()+
  facet_grid(Side ~ År, scales = "free_x")+
  geom_smooth(method='lm')+
  ggtitle("Venstresidas og høyresidas oppslutning i delbydeler etter inntekt, 2015–2025")

