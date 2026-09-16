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

# 2021 (stortingsvalg) og 2023 (kommune-/fylkestingsvalg). Samme type
# stemmekretsendringer som for 2025 (se README) - egne koblingsfiler per år.
valgkretser2021 <- read_delim("data/valgkretser2021.csv",
                              ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                              locale = locale(decimal_mark = ","),
                              trim_ws = TRUE)
valgkretser2023 <- read_delim("data/valgkretser2023.csv",
                              ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                              locale = locale(decimal_mark = ","),
                              trim_ws = TRUE)

valgkrets_til_delbydel2021 <- read_excel("data/valgkrets_til_delbydel2021.xlsx")
valgkrets_til_delbydel2023 <- read_excel("data/valgkrets_til_delbydel2023.xlsx")

# Full (ufiltrert) versjon av 2015-tallene, til bruk sammen med
# innvandrerandel/utdanning under (den trimmede `valgkretser2015` over
# brukes fortsatt av den opprinnelige 2015-vs-2019-sammenligningen).
valgkretser2015_full <- read_delim("data/valgkretser2015.csv",
                                   ";", escape_double = FALSE, col_types = cols(`Oppslutning prosentvis` = col_number()),
                                   locale = locale(decimal_mark = ","),
                                   trim_ws = TRUE)%>%
                                filter(Kommunenavn == "Oslo")

# Innvandrerandel og utdanningsnivå per delbydel, hentet fra samme kilde som
# for 2025 (se README), for alle valgår - inkludert 2015 og 2019 som tidligere
# bare hadde inntekt (fra data/inntekt.xls).
omradedata2015 <- read_excel("data/omradedata2015.xlsx")
omradedata2019 <- read_excel("data/omradedata2019.xlsx")
omradedata2021 <- read_excel("data/omradedata2021.xlsx")
omradedata2023 <- read_excel("data/omradedata2023.xlsx")


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

p_inntekt_2019 <- ggplot(oslo, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt (uten sentrum)")
ggsave("figurer/oppslutning_etter_inntekt_2019.png", p_inntekt_2019, width = 10, height = 7, dpi = 150)


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


p_fnb_2019 <- ggplot(parti, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  geom_smooth(method='lm')+
  ggtitle("FNBs oppslutning i valgkretser etter inntekt")
ggsave("figurer/fnb_oppslutning_etter_inntekt_2019.png", p_fnb_2019, width = 7, height = 5, dpi = 150)



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


p_venstre_hoyre_2019 <- ggplot(sidene, aes(x = Snittinntekt, y = `Oppslutning 2019`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Side)+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt 2019")
ggsave("figurer/venstre_hoyre_etter_inntekt_2019.png", p_venstre_hoyre_2019, width = 9, height = 5, dpi = 150)


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

p_inntekt_2025 <- ggplot(oslo2025, aes(x = Snittinntekt, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter inntekt 2025 (uten sentrum)")
ggsave("figurer/oppslutning_etter_inntekt_2025.png", p_inntekt_2025, width = 10, height = 7, dpi = 150)

p_innvandrerandel_2025 <- ggplot(oslo2025, aes(x = Innvandrerandel, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter innvandrerandel 2025 (uten sentrum)")
ggsave("figurer/oppslutning_etter_innvandrerandel_2025.png", p_innvandrerandel_2025, width = 10, height = 7, dpi = 150)

p_utdanning_2025 <- ggplot(oslo2025, aes(x = AndelHoyereUtdanning, y = `Oppslutning prosentvis`))+
  geom_point(aes(colour = Område))+
  facet_wrap(~ Partikode, scales = "free_y")+
  geom_smooth(method='lm')+
  ggtitle("Oppslutning i valgkretser etter andel med høyere utdanning 2025 (uten sentrum)")
ggsave("figurer/oppslutning_etter_utdanning_2025.png", p_utdanning_2025, width = 10, height = 7, dpi = 150)


# ---------------------------------------------------------------------------
# 2021 (stortingsvalg) og 2023 (kommunevalg), samt innvandrerandel/utdanning
# for 2015 og 2019 (som tidligere bare hadde inntekt). Bruker en liten
# hjelpefunksjon siden mønsteret er identisk med 2025-blokken over.

lag_partifigur <- function(data, x_var, x_tittel, filnavn_stub, aar) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = `Oppslutning prosentvis`))+
    geom_point(aes(colour = Område))+
    facet_wrap(~ Partikode, scales = "free_y")+
    geom_smooth(method = "lm")+
    ggtitle(paste0("Oppslutning i valgkretser etter ", x_tittel, " ", aar, " (uten sentrum)"))
  ggsave(paste0("figurer/oppslutning_etter_", filnavn_stub, "_", aar, ".png"), p, width = 10, height = 7, dpi = 150)
  invisible(p)
}

oslo2019_utvidet <- valgkretser%>%
  filter(Kommunenavn == "Oslo")%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(omradedata2019)%>%
  filter(!is.na(Område))%>%
  filter(Område != "Sentrum")

lag_partifigur(oslo2019_utvidet, "Innvandrerandel", "innvandrerandel", "innvandrerandel", 2019)
lag_partifigur(oslo2019_utvidet, "AndelHoyereUtdanning", "andel med høyere utdanning", "utdanning", 2019)

oslo2015_utvidet <- valgkretser2015_full%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel)%>%
  left_join(omradedata2015)%>%
  filter(!is.na(Område))%>%
  filter(Område != "Sentrum")

lag_partifigur(oslo2015_utvidet, "Innvandrerandel", "innvandrerandel", "innvandrerandel", 2015)
lag_partifigur(oslo2015_utvidet, "AndelHoyereUtdanning", "andel med høyere utdanning", "utdanning", 2015)

oslo2021 <- valgkretser2021%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel2021)%>%
  left_join(omradedata2021)%>%
  filter(!is.na(Område))%>%
  filter(Område != "Sentrum")

lag_partifigur(oslo2021, "Snittinntekt", "inntekt", "inntekt", 2021)
lag_partifigur(oslo2021, "Innvandrerandel", "innvandrerandel", "innvandrerandel", 2021)
lag_partifigur(oslo2021, "AndelHoyereUtdanning", "andel med høyere utdanning", "utdanning", 2021)

oslo2023 <- valgkretser2023%>%
  filter(Partikode %in% partier)%>%
  mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
  left_join(valgkrets_til_delbydel2023)%>%
  left_join(omradedata2023)%>%
  filter(!is.na(Område))%>%
  filter(Område != "Sentrum")

lag_partifigur(oslo2023, "Snittinntekt", "inntekt", "inntekt", 2023)
lag_partifigur(oslo2023, "Innvandrerandel", "innvandrerandel", "innvandrerandel", 2023)
lag_partifigur(oslo2023, "AndelHoyereUtdanning", "andel med høyere utdanning", "utdanning", 2023)


# Venstresida/høyresida over tid (2015, 2019, 2021, 2023, 2025), på
# delbydelsnivå siden stemmekretsene ikke er direkte sammenlignbare på tvers
# av alle årene. Inntekten på x-aksen er samtidig med hvert valg (dvs. ikke
# justert for prisvekst), så figuren viser sammenhengen mellom lokal inntekt
# og oppslutning på det tidspunktet, ikke endring i realinntekt.

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

lag_venstre_hoyre_delbydel <- function(valgkretser_df, mapping_df, omradedata_df, aar) {
  valgkretser_df%>%
    filter(Partikode %in% partier)%>%
    mutate(Side = ifelse(Partikode %in% c("A", "MDG", "RØDT", "SP", "SV"), "Venstresida", "Høyresida"))%>%
    group_by(Side, Stemmekretsnavn)%>%
    summarise(Oppslutning = sum(`Oppslutning prosentvis`, na.rm = TRUE), .groups = "drop")%>%
    left_join(mapping_df)%>%
    filter(!is.na(Delbydel))%>%
    group_by(Side, Delbydel)%>%
    summarise(Oppslutning = mean(Oppslutning, na.rm = TRUE), .groups = "drop")%>%
    left_join(omradedata_df)%>%
    select(Side, Delbydel, Oppslutning, Snittinntekt)%>%
    mutate(År = aar)
}

venstre_hoyre_2021 <- lag_venstre_hoyre_delbydel(valgkretser2021, valgkrets_til_delbydel2021, omradedata2021, "2021")
venstre_hoyre_2023 <- lag_venstre_hoyre_delbydel(valgkretser2023, valgkrets_til_delbydel2023, omradedata2023, "2023")
venstre_hoyre_2025 <- lag_venstre_hoyre_delbydel(valgkretser2025, valgkrets_til_delbydel2025, omradedata2025, "2025")

venstre_hoyre_alle_ar <- bind_rows(venstre_hoyre_2015, venstre_hoyre_2019, venstre_hoyre_2021, venstre_hoyre_2023, venstre_hoyre_2025)%>%
  ungroup()%>%
  mutate(Side = fct_relevel(Side, "Venstresida", "Høyresida"))

p_venstre_hoyre_2015_2025 <- ggplot(venstre_hoyre_alle_ar, aes(x = Snittinntekt, y = Oppslutning))+
  geom_point()+
  facet_grid(Side ~ År, scales = "free_x")+
  geom_smooth(method='lm')+
  ggtitle("Venstresidas og høyresidas oppslutning i delbydeler etter inntekt, 2015–2025")
ggsave("figurer/venstre_hoyre_etter_inntekt_2015_2019_2021_2023_2025.png", p_venstre_hoyre_2015_2025, width = 14, height = 6, dpi = 150)

