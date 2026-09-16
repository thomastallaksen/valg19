library(tidyverse)
library(readxl)
library(broom)
library(RColorBrewer)

# Statistisk analyse av hvordan sammenhengen mellom partienes oppslutning og
# levekårsvariablene (inntekt, innvandrerandel, utdanningsnivå) har utviklet
# seg over tid. Bruker alle valgår prosjektet har data for: lokalvalgene
# 2015, 2019 og 2023, og stortingsvalgene 2021 og 2025. I motsetning til de
# andre figurene i skript.R holdes Sentrum ikke utenfor her: alle bydeler er
# med.

partier <- c("A", "FRP", "H", "KRF", "MDG", "RØDT", "SP", "SV", "V")
venstre <- c("A", "MDG", "RØDT", "SP", "SV")

# Partiene i politisk rekkefølge fra venstre til høyre, brukt til både
# fargelegging og sortering av legender under. brewer.pal(9, "RdBu") går fra
# mørk rød til mørk blå med hvitt i midten, som gir RØDT den mørkeste røde
# fargen, FRP den mørkeste blå, og gradvis lysere farger mot midten
# (Sp/KrF) - se ?RColorBrewer::brewer.pal.
parti_rekkefolge <- c("RØDT", "SV", "MDG", "A", "SP", "KRF", "V", "H", "FRP")
parti_farger <- setNames(brewer.pal(9, "RdBu"), parti_rekkefolge)

les_valgkretsfil <- function(sti) {
  read_delim(sti, ";", escape_double = FALSE,
             col_types = cols(`Oppslutning prosentvis` = col_number()),
             locale = locale(decimal_mark = ","), trim_ws = TRUE)
}

valgkretser2015 <- les_valgkretsfil("data/valgkretser2015.csv")%>% filter(Kommunenavn == "Oslo")
valgkretser2019 <- les_valgkretsfil("data/valgkretser.csv")%>% filter(Kommunenavn == "Oslo")
valgkretser2021 <- les_valgkretsfil("data/valgkretser2021.csv")
valgkretser2023 <- les_valgkretsfil("data/valgkretser2023.csv")
valgkretser2025 <- les_valgkretsfil("data/valgkretser2025.csv")

mapping2015 <- read_excel("data/valgkrets_til_delbydel.xlsx")
mapping2019 <- read_excel("data/valgkrets_til_delbydel.xlsx")
mapping2021 <- read_excel("data/valgkrets_til_delbydel2021.xlsx")
mapping2023 <- read_excel("data/valgkrets_til_delbydel2023.xlsx")
mapping2025 <- read_excel("data/valgkrets_til_delbydel2025.xlsx")

omrade2015 <- read_excel("data/omradedata2015.xlsx")
omrade2019 <- read_excel("data/omradedata2019.xlsx")
omrade2021 <- read_excel("data/omradedata2021.xlsx")
omrade2023 <- read_excel("data/omradedata2023.xlsx")
omrade2025 <- read_excel("data/omradedata2025.xlsx")

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
  koble(valgkretser2019, mapping2019, omrade2019, 2019),
  koble(valgkretser2021, mapping2021, omrade2021, 2021),
  koble(valgkretser2023, mapping2023, omrade2023, 2023),
  koble(valgkretser2025, mapping2025, omrade2025, 2025)
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

p_utvikling <- ggplot(regresjoner%>%mutate(Partikode = fct_relevel(Partikode, parti_rekkefolge)),
                       aes(x = factor(År), y = estimate, group = Partikode, colour = Partikode))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60")+
  geom_line()+
  geom_point()+
  scale_colour_manual(values = parti_farger)+
  facet_wrap(~ Variabel, scales = "free_y")+
  labs(x = "Valgår", y = "Regresjonskoeffisient (helning)", colour = "Parti",
       title = "Utvikling i sammenhengen mellom oppslutning og levekårsvariabler",
       subtitle = "Lokalvalgene 2015/2019/2023 og stortingsvalgene 2021/2025. Alle bydeler, kretsnivå.")
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

cat("\n=== Har sammenhengen med oppslutning endret seg signifikant 2015-2025? ===\n")
cat("(sortert etter p-verdi - lavest først = sterkest evidens for endring)\n")
print(interaksjoner, n = Inf)

cat("\nSignifikante endringer (p < 0.05):\n")
print(interaksjoner%>%filter(p.value < 0.05), n = Inf)

# ---------------------------------------------------------------------------
# 4) Kontroll: inntekt, innvandrerandel og utdanning korrelerer med hverandre
#    på delbydelsnivå, så de enkle regresjonene i (1) kan blande sammen
#    effektene - en tilsynelatende "utdanningseffekt" kan egentlig skyldes at
#    delbydeler med lavt utdanningsnivå også har høy innvandrerandel (eller
#    omvendt). Her kjøres én regresjon med alle tre variablene samtidig, som
#    gir partielle (kontrollerte) sammenhenger - "hva forklarer denne
#    variabelen alene, når de to andre holdes fast".

cat("\n=== Korrelasjon mellom levekårsvariablene (delbydelsnivå, alle fire år samlet) ===\n")
korrelasjon <- alle_data%>%
  distinct(Delbydel, År, Snittinntekt, Innvandrerandel, AndelHoyereUtdanning)%>%
  select(Snittinntekt, Innvandrerandel, AndelHoyereUtdanning)%>%
  cor(use = "pairwise.complete.obs")
print(round(korrelasjon, 2))

kjor_multivariat <- function(data) {
  data%>%
    filter(!is.na(Snittinntekt), !is.na(Innvandrerandel), !is.na(AndelHoyereUtdanning))%>%
    group_by(Partikode, Side, År)%>%
    group_modify(~ {
      modell <- lm(`Oppslutning prosentvis` ~ Snittinntekt + Innvandrerandel + AndelHoyereUtdanning, data = .x)
      broom::tidy(modell)%>%
        filter(term != "(Intercept)")%>%
        mutate(r.squared = broom::glance(modell)$r.squared, n = nrow(.x))
    })%>%
    ungroup()%>%
    rename(Variabel = term)%>%
    mutate(Variabel = recode(Variabel,
                              "Snittinntekt" = "Inntekt",
                              "Innvandrerandel" = "Innvandrerandel",
                              "AndelHoyereUtdanning" = "Utdanning"))
}

multivariat <- kjor_multivariat(alle_data)
write_csv(multivariat, "data/regresjonsresultater_multivariat.csv")

cat("\n=== Kontrollert regresjon: Oppslutning ~ inntekt + innvandrerandel + utdanning samtidig ===\n")
print(multivariat%>%arrange(Variabel, Partikode, År)%>%select(-r.squared, -n), n = Inf)

# Sammenligning enkel vs. kontrollert - "andel_forklart_av_andre" viser hvor
# mye av den enkle sammenhengen som forsvinner når de to andre variablene
# holdes fast (nær 1 = nesten hele den tilsynelatende effekten var egentlig
# de andre variablene; negativ/over 1 = fortegnet har snudd).
sammenligning <- regresjoner%>%
  select(Partikode, Side, År, Variabel, enkel = estimate)%>%
  inner_join(multivariat%>%select(Partikode, Side, År, Variabel, kontrollert = estimate),
             by = c("Partikode", "Side", "År", "Variabel"))%>%
  mutate(andel_forklart_av_andre = round(1 - kontrollert/enkel, 2))

write_csv(sammenligning, "data/regresjonsresultater_sammenligning.csv")

cat("\n=== SV og utdanning: enkel vs. kontrollert sammenheng ===\n")
print(sammenligning%>%filter(Partikode == "SV", Variabel == "Utdanning"))

cat("\n=== Alle partier, alle variabler: enkel vs. kontrollert ===\n")
print(sammenligning%>%arrange(Variabel, Partikode, År), n = Inf)

p_kontrollert <- ggplot(sammenligning%>%
                           mutate(Partikode = fct_relevel(Partikode, parti_rekkefolge))%>%
                           pivot_longer(c(enkel, kontrollert), names_to = "Type", values_to = "Helning")%>%
                           mutate(Type = fct_relevel(Type, "enkel", "kontrollert")),
                         aes(x = factor(År), y = Helning, colour = Partikode, linetype = Type,
                             group = interaction(Partikode, Type)))+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey70")+
  geom_line()+
  geom_point(size = 1)+
  scale_colour_manual(values = parti_farger)+
  scale_linetype_manual(values = c(enkel = "solid", kontrollert = "22"))+
  facet_wrap(~ Variabel, scales = "free_y")+
  labs(x = "Valgår", y = "Regresjonskoeffisient", colour = "Parti", linetype = "",
       title = "Enkel vs. kontrollert sammenheng med oppslutning",
       subtitle = "Heltrukket = enkel regresjon (kun denne variabelen). Stiplet = kontrollert for de to andre variablene samtidig.")
ggsave("figurer/utvikling_regresjon_kontrollert_vs_enkel.png", p_kontrollert, width = 13, height = 7, dpi = 150)

# ---------------------------------------------------------------------------
# 5) SVs oppslutning i hver enkelt valgkrets, år for år - samme type figur
#    som oslo/oslo2025-plottene i skript.R, men for ett parti og alle fem
#    valgår i stedet for alle partier i ett valgår.

sv_data <- alle_data%>%filter(Partikode == "SV")

lag_sv_figur <- function(data, x_var, x_tittel, filnavn_stub) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = `Oppslutning prosentvis`))+
    geom_point(aes(colour = Område))+
    geom_smooth(method = "lm")+
    facet_wrap(~ År)+
    labs(x = x_tittel, y = "SVs oppslutning (%)",
         title = paste0("SVs oppslutning i valgkretsene etter ", x_tittel),
         subtitle = "2015, 2019, 2021, 2023 og 2025. Alle bydeler.")
  ggsave(paste0("figurer/sv_oppslutning_etter_", filnavn_stub, ".png"), p, width = 10, height = 7, dpi = 150)
  invisible(p)
}

lag_sv_figur(sv_data, "Snittinntekt", "inntekt", "inntekt")
lag_sv_figur(sv_data, "Innvandrerandel", "innvandrerandel", "innvandrerandel")
lag_sv_figur(sv_data, "AndelHoyereUtdanning", "andel med høyere utdanning", "utdanning")
