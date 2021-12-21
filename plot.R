# inspiration: from
# https://twitter.com/VictimOfMaths/status/1472968769353965568
# https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDCasesLTLAPhasePlot.R

library(tibble)
library(readr)
library(forcats)
library(tidyr)
library(stringr)
library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(scales)
library(plotly)
library(lubridate)
library(curl)
library(readxl)

filename <- paste0(today(), "_Fallzahlen_Inzidenz.xlsx")
urlRKI <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Inzidenz_aktualisiert.xlsx?__blob=publicationFile"
curl_download(url = urlRKI, destfile = filename, quiet = FALSE, mode = "wb")

DateOfInterest <- today() - days(1)

# Bundesländer ----------------------------------------------------------------------------------------------------------------------------------------------

df.raw.BL <- read_xlsx(filename, sheet = 5, skip = 1)

df.long <- df.raw.BL %>%
  pivot_longer(-MeldeLandkreisBundesland, names_to = "Datum", values_to = "Inzidenz")

df.plot <- df.long %>%
  mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y")) %>%
  transmute(BL = MeldeLandkreisBundesland, Datum, Inzidenz) %>%
  group_by(BL) %>%
    arrange(Datum) %>%
    mutate(Inzidenz7DaysAgo = lag(Inzidenz, 7)) %>%
    mutate(Change = Inzidenz - Inzidenz7DaysAgo) %>%
  ungroup() %>%
  filter(Datum >= DateOfInterest - days(7), Datum <= DateOfInterest) %>%
  mutate(DaysSince = as.integer(DateOfInterest - Datum)) %>%
  mutate(Highlight = ifelse(BL == "Gesamt", "y", "n"))

p.BL <- df.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_vline(xintercept = 0, color = "grey20") +
  geom_path(aes(x = Inzidenz, y = Change, group = BL, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
  geom_point(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change, fill = Highlight), shape = 21, size = 3) +
  geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change, label = BL), color = "grey40") +
  scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
  scale_fill_manual(values = c("y" = "red", "n" = "grey50"), guide = "none") +
  scale_y_continuous(name = "Differenz zur 7-Tage-Inzidenz vor einer Woche") +
  theme_bw() +
  theme(axis.line = element_blank(), axis.ticks = element_blank()) +
  labs(
    title = "Entwicklung 7-Tage-Inzidenzen der Bundesländer",
    subtitle = paste0("Datum: ", DateOfInterest),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
  )

ggsave(p.BL, filename = paste0(DateOfInterest, "_Entwicklung_Inzidenz_BL.png"), width = 10)


# Landkreise ----------------------------------------------------------------------------------------------------------------------------------------------

df.raw.LK <- read_xlsx(filename, sheet = 7, skip = 1)

df.long <- df.raw.LK %>%
  filter(!is.na(MeldeLandkreis)) %>%
  pivot_longer(cols = !c("IdMeldeLandkreis", "MeldeLandkreis"), names_to = "Datum", values_to = "Inzidenz")

df.plot <- df.long %>%
  mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y")) %>%
  transmute(LK = MeldeLandkreis, Datum, Inzidenz) %>%
  group_by(LK) %>%
    arrange(Datum) %>%
    mutate(Inzidenz7DaysAgo = lag(Inzidenz, 7)) %>%
    mutate(Change = Inzidenz - Inzidenz7DaysAgo) %>%
  ungroup() %>%
  filter(Datum >= DateOfInterest - days(7), Datum <= DateOfInterest) %>%
  mutate(DaysSince = as.integer(DateOfInterest - Datum))

p.LK <- df.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_vline(xintercept = 0, color = "grey20") +
  geom_path(aes(x = Inzidenz, y = Change, group = LK, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
  geom_point(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change), color = "deepskyblue4", alpha = 0.8) +
  geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change, label = LK), size = 2.3) +
  scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
  scale_y_continuous(name = "Differenz zur 7-Tage-Inzidenz vor einer Woche") +
  theme_bw() +
  theme(axis.line = element_blank(), axis.ticks = element_blank()) +
  labs(
    title = "Entwicklung 7-Tage-Inzidenzen der Landkreise",
    subtitle = paste0("Datum: ", DateOfInterest),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
  )

ggsave(p.LK, filename = paste0(DateOfInterest, "_Entwicklung_Inzidenz_LK.png"), width = 10)
