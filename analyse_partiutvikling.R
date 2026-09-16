library(tidyverse)
library(readxl)
library(broom)

# Statistisk analyse av hvordan sammenhengen mellom partienes oppslutning og
# levekårsvariablene (inntekt, innvandrerandel, utdanningsnivå) har utviklet
# seg over tid. Avgrenset til lokalvalgene 2015, 2019 og 2023, samt
# stortingsvalget 2017 - dvs. IKKE 2021/2025, som allerede er dekket andre
# steder i prosjektet. I motsetning til de andre figurene i skript.R holdes
# Sentrum ikke utenfor her: alle bydeler er med.

partier <- c("A", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V")
venstre <- c("A", "MDG", "RØDT", "SP", "SV")

les_valgkretsfil <- function(sti) {
  read_delim(sti, ";", escape_double = FALSE,
             col_types = cols(`Oppslutning prosentvis` = col_number()),
             locale = locale(decimal_mark = ","), trim_ws = TRUE)
}

valgkretser2015 <- les_valgkretsfil("data/valgkretser2015.csv")%>% filter(Kommunenavn == "Oslo")
valgkretser2017 <- les_valgkretsfil("data/valgkretser2017.csv")
valgkretser2019 <- les_valgkretsfil("data/valgkretser.csv")%>% filter(Kommunenavn == "Oslo")
valgkretser2023 <- les_valgkretsfil("data/valgkretser2023.csv")

mapping2015 <- read_excel("data/valgkrets_til_delbydel.xlsx")
mapping2017 <- read_excel("data/valgkrets_til_delbydel2017.xlsx")
mapping2019 <- read_excel("data/valgkrets_til_delbydel.xlsx")
mapping2023 <- read_excel("data/valgkrets_til_delbydel2023.xlsx")

omrade2015 <- read_excel("data/omradedata2015.xlsx")
omrade2017 <- read_excel("data/omradedata2017.xlsx")
omrade2019 <- read_excel("data/omradedata2019.xlsx")
omrade2023 <- read_excel("data/omradedata2023.xlsx")

koble <- function(valg, mapping, omrade, aar) {
  valg%>%
    filter(Partikode %in% partier)%>%
    mutate(Side = ifelse(Partikode %in% venstre, "Venstresida", "Høyresida"))%>%
    left_join(mapping, by = "Stemmekretsnavn")%>%
    left_join(omrade, by = "Delbydel")%>%
    filter(!is.na(Område))%>%  # dropper kretser uten delbydelsdata (Maridalen/Sørkedalen o.l.)
    mutate(År = aar)
}

alle_data <- bind_rows(
  koble(valgkretser2015, mapping2015, omrade2015, 2015),
  koble(valgkretser2017, mapping2017, omrade2017, 2017),
  koble(valgkretser2019, mapping2019, omrade2019, 2019),
  koble(valgkretser2023, mapping2023, omrade2023, 2023)
)

variabler <- c(Inntekt = "Snittinntekt", Innvandrerandel = "Innvandrerandel", Utdanning = "AndelHoyereUtdanning")

# ---------------------------------------------------------------------------
# 1) Regresjon per parti og valgår: Oppslutning ~ variabel (lineær), krets som
#    observasjonsenhet. Gir ett stigningstall (helning) og én R^2 per
#    parti/år/variabel-kombinasjon.

kjor_regresjoner <- function(data, x_kolonne, variabelnavn) {
  data%>%
    filter(!is.na(.data[[x_kolonne]]))%>%
    mutate(x_lokal = .data[[x_kolonne]])%>%
    group_by(Partikode, Side, År)%>%
    group_modify(~ {
      modell <- lm(`Oppslutning prosentvis` ~ x_lokal, data = .x)
      bind_cols(broom::tidy(modell)[2, c("estimate", "std.error", "p.value")],
                r.squared = broom::glance(modell)$r.squared,
                n = nrow(.x))
    })%>%
    ungroup()%>%
    mutate(Variabel = variabelnavn)
}

regresjoner <- map_dfr(names(variabler), ~ kjor_regresjoner(alle_data, variabler[[.x]], .x))

write_csv(regresjoner, "data/regresjonsresultater_partiutvikling.csv")

cat("\n=== Regresjonsresultater (helning = endring i oppslutning per enhet av variabelen) ===\n")
print(regresjoner%>%arrange(Variabel, Partikode, År), n = Inf)

# ---------------------------------------------------------------------------
# 2) Figur: hvordan helningen (styrken og retningen på sammenhengen) utvikler
#    seg fra valg til valg, per parti og variabel.

p_utvikling <- ggplot(regresjoner, aes(x = factor(År), y = estimate, group = Partikode, colour = Partikode))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60")+
  geom_line()+
  geom_point()+
  facet_wrap(~ Variabel, scales = "free_y")+
  labs(x = "Valgår", y = "Regresjonskoeffisient (helning)",
       title = "Utvikling i sammenhengen mellom oppslutning og levekårsvariabler",
       subtitle = "Lokalvalgene 2015/2019/2023 og stortingsvalget 2017. Alle bydeler, kretsnivå.")
ggsave("figurer/utvikling_regresjonskoeffisienter.png", p_utvikling, width = 12, height = 7, dpi = 150)

# ---------------------------------------------------------------------------
# 3) Har sammenhengen endret seg signifikant over tid? Tester med et
#    interaksjonsledd (variabel × valgår, valgår som kontinuerlig) per parti
#    og variabel. Et signifikant interaksjonsledd betyr at helningen fra (1)
#    ikke er stabil, men har en trend gjennom perioden.

test_interaksjon <- function(data, x_kolonne, variabelnavn) {
  data%>%
    filter(!is.na(.data[[x_kolonne]]))%>%
    mutate(x_lokal = .data[[x_kolonne]])%>%
    group_by(Partikode)%>%
    group_modify(~ {
      modell <- lm(`Oppslutning prosentvis` ~ x_lokal * År, data = .x)
      broom::tidy(modell)%>%
        filter(str_detect(term, ":"))%>%
        select(estimate, std.error, p.value)
    })%>%
    ungroup()%>%
    mutate(Variabel = variabelnavn)
}

interaksjoner <- map_dfr(names(variabler), ~ test_interaksjon(alle_data, variabler[[.x]], .x))%>%
  rename(endring_i_helning_per_ar = estimate)%>%
  arrange(p.value)

write_csv(interaksjoner, "data/regresjonsresultater_interaksjon.csv")

cat("\n=== Har sammenhengen med oppslutning endret seg signifikant 2015-2023? ===\n")
cat("(sortert etter p-verdi - lavest først = sterkest evidens for endring)\n")
print(interaksjoner, n = Inf)

cat("\nSignifikante endringer (p < 0.05):\n")
print(interaksjoner%>%filter(p.value < 0.05), n = Inf)
