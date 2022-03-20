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

df.ID_BL <- data.frame(BL_ID = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"),
                       Bundesland = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Württemberg", "Bayern", "Saarland", "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen")
)


filename <- paste0(today(), "_Fallzahlen_Inzidenz.xlsx")
urlRKI <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Inzidenz_aktualisiert.xlsx?__blob=publicationFile"
curl_download(url = urlRKI, destfile = filename, quiet = FALSE, mode = "wb")

DateOfInterest <- today() - days(1)

# Bundesländer ----------------------------------------------------------------------------------------------------------------------------------------------

df.raw.BL <- read_xlsx(filename, sheet = 5, skip = 1)

df.long <- df.raw.BL %>%
  pivot_longer(-MeldeLandkreisBundesland, names_to = "Datum", values_to = "Inzidenz") %>%
  mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y"))

max_available_date <- max(df.long$Datum)
DateOfInterest <- min(DateOfInterest, max_available_date)

df.plot <- df.long %>%
  transmute(BL = MeldeLandkreisBundesland, Datum, Inzidenz) %>%
  group_by(BL) %>%
    arrange(Datum) %>%
    mutate(Inzidenz7DaysAgo = lag(Inzidenz, 7)) %>%
    mutate(Change = Inzidenz - Inzidenz7DaysAgo,
           ChangePerc = Change / Inzidenz7DaysAgo * 100) %>%
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

p.BL.perc <- df.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_vline(xintercept = 0, color = "grey20") +
  geom_path(aes(x = Inzidenz, y = ChangePerc, group = BL, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
  geom_point(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = ChangePerc, fill = Highlight), shape = 21, size = 3) +
  geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = ChangePerc, label = BL), color = "grey40") +
  scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
  scale_fill_manual(values = c("y" = "red", "n" = "grey50"), guide = "none") +
  scale_y_continuous(name = "Unterschied zur 7-Tage-Inzidenz vor einer Woche in Prozent") +
  theme_bw() +
  theme(axis.line = element_blank(), axis.ticks = element_blank()) +
  labs(
    title = "Prozentuale Entwicklung 7-Tage-Inzidenzen der Bundesländer",
    subtitle = paste0("Datum: ", DateOfInterest),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
  )

ggsave(p.BL.perc, filename = paste0(DateOfInterest, "_Entwicklung_Inzidenz_Prozent_BL.png"), width = 10)


# Landkreise ----------------------------------------------------------------------------------------------------------------------------------------------

df.raw.LK <- read_xlsx(filename, sheet = 7, skip = 1)

df.long <- df.raw.LK %>%
  filter(!is.na(MeldeLandkreis)) %>%
  pivot_longer(cols = !c("IdMeldeLandkreis", "MeldeLandkreis"), names_to = "Datum", values_to = "Inzidenz") %>%
  mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y"))

max_available_date <- max(df.long$Datum)
DateOfInterest <- min(DateOfInterest, max_available_date)

df.plot <- df.long %>%
  mutate(BL_ID = substr(IdMeldeLandkreis,1,2)) %>%
  left_join(df.ID_BL, by="BL_ID") %>%
  transmute(LK = MeldeLandkreis, Bundesland, Datum, Inzidenz) %>%
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
  geom_point(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change, fill = Bundesland), shape = 21, color = "grey50", alpha = 0.8) +
  geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = Change, label = LK), size = 2.3, min.segment.length = Inf) +
  scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
  scale_y_continuous(name = "Differenz zur 7-Tage-Inzidenz vor einer Woche") +
  guides(fill = guide_legend(ncol = 6, title = element_blank())) +
  theme_bw() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
  labs(
    title = "Entwicklung 7-Tage-Inzidenzen der Landkreise",
    subtitle = paste0("Datum: ", DateOfInterest),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
  )

ggsave(p.LK, filename = paste0(DateOfInterest, "_Entwicklung_Inzidenz_LK.png"), width = 10)

p.LK.perc <- df.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_vline(xintercept = 0, color = "grey20") +
  geom_path(aes(x = Inzidenz, y = ChangePerc, group = LK, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
  geom_point(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = ChangePerc, fill = Bundesland), shape = 21, color = "grey50", alpha = 0.8) +
  geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest), aes(x = Inzidenz, y = ChangePerc, label = LK), size = 2.3) +
  scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
  scale_y_continuous(name = "Unterschied zur 7-Tage-Inzidenz vor einer Woche in Prozent") +
  guides(fill = guide_legend(ncol = 6, title = element_blank())) +
  theme_bw() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
  labs(
    title = "Prozentuale Entwicklung 7-Tage-Inzidenzen der Landkreise",
    subtitle = paste0("Datum: ", DateOfInterest),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
  )

ggsave(p.LK.perc, filename = paste0(DateOfInterest, "_Entwicklung_Inzidenz_Prozent_LK.png"), width = 10)

# Kumulierte Todesfälle -----------------------------------------------------------------------------------------------------------------------------------------------

filename <- paste0(today(), "_Fallzahlen_Todesfälle.xlsx")
urlRKI <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Gesamtuebersicht.xlsx?__blob=publicationFile"
curl_download(url = urlRKI, destfile = filename, quiet = FALSE, mode = "wb")

df.raw <- read_xlsx(filename, sheet = 1, skip = 3, col_names = c("date", "n_cases", "diff_cases", "n_deaths", "diff_deaths", "perc_deaths", "n_non_deaths"))

last_date <- tail(df.plot,1)$date

df.plot <- df.raw %>%
  replace_na(replace = list(n_deaths = 0)) %>%
  mutate(date = as.Date(date)) %>%
  select(date, n_deaths)

p.deaths <- df.plot %>%
  ggplot(aes(x = date, y = n_deaths)) +
  geom_line(color = "orange", size = 1.2) +
  theme_bw() +
  labs(
    title = "Anzahl Todesfälle (kumuliert)",
    subtitle = paste0("Datum: ", last_date),
    caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Gesamtuebersicht.html"
  ) +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels = scales::label_number(), breaks = seq(0,1e6,25000)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y")

ggsave(p.deaths, filename = paste0(last_date, "_kumulierte_Todesfälle.png"), width = 10)

