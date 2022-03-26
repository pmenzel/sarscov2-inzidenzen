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
library(plotly)
library(lubridate)
library(curl)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)

# global vars ----------------------------------------------------------------------------------------------------------------------------------------------------------

urlRKI <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Inzidenz_aktualisiert.xlsx?__blob=publicationFile"
filename <- "Fallzahlen_Inzidenz_aktualisiert.xlsx"

df.ID_BL <- data.frame(BL_ID = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"),
                       Bundesland = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Baden-Württemberg", "Bayern", "Saarland", "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen")
)


# UI ----------------------------------------------------------------------------------------------------------------------------------------------------------

header <- dashboardHeader(title = "SARS-CoV-2 Inzidenzen")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Inzidenzen", startExpanded = TRUE,
      menuSubItem("Bundesländer", tabName = "tab-BL"),
      menuSubItem("Landkreise", tabName = "tab-LK")
    ),
    menuItem("Todesfälle",
      menuSubItem("kumulierte Todesfälle", tabName = "tab-deaths")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab-BL", fluidRow(box(width = "12", plotOutput("plotBL", width = "1000px", height = "800px") %>% withSpinner(color = "#0dc5c1")),
                                         box(width = "12", plotOutput("plotBLperc", width = "1000px", height = "800px") %>% withSpinner(color = "#0dc5c1")))),
    tabItem(tabName = "tab-LK", fluidRow(box(width = "12", plotOutput("plotLK", width = "1000px", height = "800px") %>% withSpinner(color = "#0dc5c1")),
                                         box(width = "12", plotOutput("plotLKperc", width = "1000px", height = "800px") %>% withSpinner(color = "#0dc5c1")))),
    tabItem(tabName = "tab-deaths", fluidRow(box(width = "12", plotOutput("plotDeaths", width = "1000px", height = "800px") %>% withSpinner(color = "#0dc5c1")))                                         )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Server ----------------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Inzidenzen ----------------------------------------------------------------------------------------------------------------------------------------------

  DateOfInterest <- reactiveVal({
    today() - days(1)
  })

  # Bundesländer ----------------------------------------------------------------------------------------------------------------------------------------------

  df.plot.BL <- reactive({
    # reload every 8 hours
    invalidateLater(8* 60 * 60* 1000)

    if(!file.exists(filename) || now() - file.info(filename)$mtime > hours(8)) {
      message(paste(now(), "Downloading to file", filename))
      curl_download(url = urlRKI, destfile = filename, quiet = FALSE, mode = "wb")
    }

    df.raw.BL <- read_xlsx(filename, sheet = 4, skip = 1)

    df.long <- df.raw.BL %>%
      pivot_longer(-MeldeLandkreisBundesland, names_to = "Datum", values_to = "Inzidenz") %>%
      mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y"))

    max_available_date <- max(df.long$Datum)
    DateOfInterest(min(DateOfInterest(), max_available_date))

    df.long %>%
      transmute(BL = MeldeLandkreisBundesland, Datum, Inzidenz) %>%
      group_by(BL) %>%
        arrange(Datum) %>%
        mutate(Inzidenz7DaysAgo = lag(Inzidenz, 7)) %>%
        mutate(Change = Inzidenz - Inzidenz7DaysAgo,
               ChangePerc = Change / Inzidenz7DaysAgo * 100) %>%
      ungroup() %>%
      filter(Datum >= DateOfInterest() - days(7), Datum <= DateOfInterest()) %>%
      mutate(DaysSince = as.integer(DateOfInterest() - Datum)) %>%
      mutate(Highlight = ifelse(BL == "Gesamt", "y", "n"))
  })


  output$plotBL <- renderPlot({
    df.plot <- df.plot.BL()

    df.plot %>%
      ggplot() +
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = 0, color = "grey20") +
      geom_path(aes(x = Inzidenz, y = Change, group = BL, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
      geom_point(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = Change, fill = Highlight), shape = 21, size = 3) +
      geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = Change, label = BL), color = "grey40") +
      scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
      scale_fill_manual(values = c("y" = "red", "n" = "grey50"), guide = "none") +
      scale_y_continuous(name = "Differenz zur 7-Tage-Inzidenz vor einer Woche") +
      theme_bw(base_size = 15) +
      theme(axis.line = element_blank(), axis.ticks = element_blank()) +
      labs(
        title = "Entwicklung 7-Tage-Inzidenzen der Bundesländer",
        subtitle = paste0("Datum: ", DateOfInterest()),
        caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
      )
  })

  output$plotBLperc <- renderPlot({
    df.plot <- df.plot.BL()

    df.plot %>%
      ggplot() +
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = 0, color = "grey20") +
      geom_path(aes(x = Inzidenz, y = ChangePerc, group = BL, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
      geom_point(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = ChangePerc, fill = Highlight), shape = 21, size = 3) +
      geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = ChangePerc, label = BL), color = "grey40") +
      scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
      scale_fill_manual(values = c("y" = "red", "n" = "grey50"), guide = "none") +
      scale_y_continuous(name = "Unterschied zur 7-Tage-Inzidenz vor einer Woche in Prozent") +
      theme_bw(base_size = 15) +
      theme(axis.line = element_blank(), axis.ticks = element_blank()) +
      labs(
        title = "Prozentuale Entwicklung 7-Tage-Inzidenzen der Bundesländer",
        subtitle = paste0("Datum: ", DateOfInterest()),
        caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
      )
  })

  # Landkreise ----------------------------------------------------------------------------------------------------------------------------------------------

  df.plot.LK <- reactive({
    # reload every 8 hours
    invalidateLater(8* 60 * 60* 1000)

    if(!file.exists(filename) || now() - file.info(filename)$mtime > hours(8)) {
      message(paste(now(), "Downloading to file", filename))
      curl_download(url = urlRKI, destfile = filename, quiet = FALSE, mode = "wb")
    }

    df.raw.LK <- read_xlsx(filename, sheet = 6, skip = 1)

    df.long <- df.raw.LK %>%
      filter(!is.na(MeldeLandkreis)) %>%
      pivot_longer(cols = !c("IdMeldeLandkreis", "MeldeLandkreis"), names_to = "Datum", values_to = "Inzidenz") %>%
      mutate(Datum = readr::parse_date(Datum, "%d.%m.%Y"))

    max_available_date <- max(df.long$Datum)
    DateOfInterest(min(DateOfInterest(), max_available_date))

    df.long %>%
      mutate(BL_ID = substr(IdMeldeLandkreis,1,2)) %>%
      left_join(df.ID_BL, by="BL_ID") %>%
      transmute(LK = MeldeLandkreis, Bundesland, Datum, Inzidenz) %>%
      group_by(LK) %>%
        arrange(Datum) %>%
        mutate(Inzidenz7DaysAgo = lag(Inzidenz, 7)) %>%
        mutate(Change = Inzidenz - Inzidenz7DaysAgo,
               ChangePerc = Change / Inzidenz7DaysAgo * 100) %>%
      ungroup() %>%
      filter(Datum >= DateOfInterest() - days(7), Datum <= DateOfInterest()) %>%
      mutate(DaysSince = as.integer(DateOfInterest() - Datum))
  })

  output$plotLK <- renderPlot({
    df.plot <- df.plot.LK()

    df.plot %>%
      ggplot() +
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = 0, color = "grey20") +
      geom_path(aes(x = Inzidenz, y = Change, group = LK, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
      geom_point(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = Change, fill = Bundesland), shape = 21, color = "grey50", alpha = 0.8) +
      geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = Change, label = LK)) +
      scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
      scale_y_continuous(name = "Differenz zur 7-Tage-Inzidenz vor einer Woche") +
      theme_bw() +
      guides(fill = guide_legend(ncol = 6, title = element_blank())) +
      theme_bw() +
      theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
      labs(
        title = "Entwicklung 7-Tage-Inzidenzen der Landkreise",
        subtitle = paste0("Datum: ", DateOfInterest()),
        caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
      )
  })

  output$plotLKperc <- renderPlot({
    df.plot <- df.plot.LK()

    df.plot %>%
      ggplot() +
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = 0, color = "grey20") +
      geom_path(aes(x = Inzidenz, y = ChangePerc, group = LK, alpha = 7 - DaysSince), color = "grey50", show.legend = FALSE) +
      geom_point(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = ChangePerc, fill = Bundesland), shape = 21, color = "grey50", alpha = 0.8) +
      geom_text_repel(data = df.plot %>% filter(Datum == DateOfInterest()), aes(x = Inzidenz, y = ChangePerc, label = LK)) +
      scale_x_continuous(name = "7-Tage-Inzidenz (Fälle in der letzten Woche je 100k Einwohner)") +
      scale_y_continuous(name = "Unterschied zur 7-Tage-Inzidenz vor einer Woche in Prozent") +
      theme_bw() +
      guides(fill = guide_legend(ncol = 6, title = element_blank())) +
      theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom") +
      labs(
        title = "Prozentuale Entwicklung 7-Tage-Inzidenzen der Landkreise",
        subtitle = paste0("Datum: ", DateOfInterest()),
        caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Inzidenz-Tabellen.html"
      )
  })

  # Todesfälle ----------------------------------------------------------------------------------------------------------------------------------------------

  urlRKI_deaths <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Gesamtuebersicht.xlsx?__blob=publicationFile"
  filename_deaths <- "Fallzahlen_Todesfälle.xlsx"

  df.plot.deaths <- reactive({
    # reload every 8 hours
    invalidateLater(8* 60 * 60* 1000)

    if(!file.exists(filename_deaths) || now() - file.info(filename_deaths)$mtime > hours(8)) {
      message(paste(now(), "Downloading to file", filename_deaths))
      curl_download(url = urlRKI_deaths, destfile = filename_deaths, quiet = FALSE, mode = "wb")
    }

    df.raw <- read_xlsx(filename_deaths, sheet = 1, skip = 3, col_names = c("date", "n_cases", "diff_cases", "n_deaths", "diff_deaths", "perc_deaths", "n_non_deaths"))

    df.raw %>%
      replace_na(replace = list(n_deaths = 0)) %>%
      mutate(date = as.Date(date)) %>%
      select(date, n_deaths)

  })

  output$plotDeaths <- renderPlot({
    df.plot <- df.plot.deaths()

    df.plot %>%
      ggplot(aes(x = date, y = n_deaths)) +
      geom_line(color = "orange", size = 1.2) +
      theme_bw(base_size = 15) +
      labs(
        title = "Anzahl Todesfälle (kumuliert)",
        subtitle = paste0("Datum: ", tail(df.plot,1)$date),
        caption = "Daten von https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Gesamtuebersicht.html"
      ) +
      xlab("") +
      ylab("") +
      scale_y_continuous(labels = scales::label_number(), breaks = seq(0,1e6,25000)) +
      scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y")
  })

}

shinyApp(ui = ui, server = server)
