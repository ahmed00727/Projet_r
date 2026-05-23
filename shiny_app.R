# ============================================================================
# SCRIPT 04 - DASHBOARD SHINY
# Dataset : movies_clean.csv
# Auteur  : Ahmed bechir 
# Matricule : C34639
# ============================================================================
# Pour lancer l'application : shiny::runApp("app/app.R")
# ============================================================================

library(shiny)
library(shinydashboard)  # layout dashboard professionnel
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)              # tableaux interactifs
library(rnaturalearth)
library(rnaturalearthdata)

# ── Chargement des données (une seule fois au démarrage) ─────────────────────
movies <- read_csv("movies_clean.csv") %>%
  mutate(
    genre   = as.factor(genre),
    country = as.factor(country),
    rating  = as.factor(rating),
    budget_m = budget / 1e6,
    gross_m  = gross  / 1e6,
    profit_m = (gross - budget) / 1e6
  )

# Valeurs pour les filtres
all_genres    <- c("Tous", levels(movies$genre))
all_ratings   <- c("Tous", levels(movies$rating))
all_countries <- c("Tous", levels(movies$country))
year_range    <- range(movies$year, na.rm = TRUE)

# ── UI (Interface Utilisateur) ────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  # En-tête
  dashboardHeader(
    title = "🎬 Movies Analytics",
    titleWidth = 220
  ),

  # Barre latérale (filtres)
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("📊 Vue d'ensemble",  tabName = "overview",  icon = icon("chart-bar")),
      menuItem("🎭 Analyse Genres",  tabName = "genres",    icon = icon("film")),
      menuItem("🌍 Cartographie",    tabName = "carte",     icon = icon("globe")),
      menuItem("🔍 Explorer",        tabName = "explorer",  icon = icon("table"))
    ),

    hr(),
    h5("  Filtres", style = "color: #ccc; margin-left: 15px;"),

    # Filtre année (slider)
    sliderInput("year_range",
                label   = "Période :",
                min     = year_range[1],
                max     = year_range[2],
                value   = year_range,
                step    = 1,
                sep     = ""),

    # Filtre genre
    selectInput("genre_filter",
                label   = "Genre :",
                choices = all_genres,
                selected = "Tous"),

    # Filtre rating
    selectInput("rating_filter",
                label   = "Classification :",
                choices = all_ratings,
                selected = "Tous"),

    # Filtre score minimum
    sliderInput("score_min",
                label   = "Score minimum :",
                min     = 1, max = 10,
                value   = 1, step = 0.5),

    # Bouton reset
    actionButton("reset_filters", "↺ Réinitialiser",
                 class = "btn-warning btn-sm",
                 style = "margin: 10px 15px;")
  ),

  # Corps principal (onglets)
  dashboardBody(
    tabItems(

      # ── ONGLET 1 : Vue d'ensemble ──────────────────────────────────────────
      tabItem(
        tabName = "overview",

        # Ligne de KPI (indicateurs clés)
        fluidRow(
          valueBoxOutput("kpi_films",    width = 3),
          valueBoxOutput("kpi_score",    width = 3),
          valueBoxOutput("kpi_budget",   width = 3),
          valueBoxOutput("kpi_gross",    width = 3)
        ),

        # Ligne de graphiques
        fluidRow(
          box(
            title  = "Évolution du nombre de films par année",
            width  = 8, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_evol_annuelle", height = "300px")
          ),
          box(
            title  = "Distribution des scores",
            width  = 4, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_distrib_score", height = "300px")
          )
        ),

        fluidRow(
          box(
            title  = "Budget vs Recettes",
            width  = 6, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_budget_gross", height = "320px")
          ),
          box(
            title  = "Score moyen par rating",
            width  = 6, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_score_rating", height = "320px")
          )
        )
      ),

      # ── ONGLET 2 : Analyse par genre ──────────────────────────────────────
      tabItem(
        tabName = "genres",

        fluidRow(
          box(
            title  = "Score moyen par genre",
            width  = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_genre_score", height = "350px")
          ),
          box(
            title  = "ROI moyen par genre",
            width  = 6, status = "success", solidHeader = TRUE,
            plotlyOutput("plot_genre_roi", height = "350px")
          )
        ),

        fluidRow(
          box(
            title  = "Évolution temporelle par genre",
            width  = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_genre_evol", height = "350px")
          )
        )
      ),

      # ── ONGLET 3 : Cartographie ────────────────────────────────────────────
      tabItem(
        tabName = "carte",

        fluidRow(
          box(
            title  = "Carte mondiale de la production",
            width  = 12, status = "primary", solidHeader = TRUE,
            leafletOutput("carte_monde", height = "500px")
          )
        ),

        fluidRow(
          box(
            title  = "Top 15 pays producteurs",
            width  = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_top_pays", height = "350px")
          ),
          box(
            title  = "Score moyen par pays (top 10)",
            width  = 6, status = "warning", solidHeader = TRUE,
            plotlyOutput("plot_score_pays", height = "350px")
          )
        )
      ),

      # ── ONGLET 4 : Explorateur de données ─────────────────────────────────
      tabItem(
        tabName = "explorer",

        fluidRow(
          box(
            title  = "Rechercher un film",
            width  = 12, status = "primary", solidHeader = TRUE,
            textInput("search_film", "Titre du film :", placeholder = "ex: Avengers"),
            DTOutput("table_films")
          )
        ),

        fluidRow(
          box(
            title  = "Télécharger les données filtrées",
            width  = 4, status = "info", solidHeader = TRUE,
            downloadButton("download_csv", "💾 Télécharger CSV",
                           class = "btn-info btn-block")
          )
        )
      )
    )
  )
)

# ── SERVER (Logique) ──────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Données réactives (filtrées selon les inputs)
  movies_filtered <- reactive({
    df <- movies %>%
      filter(
        year  >= input$year_range[1],
        year  <= input$year_range[2],
        score >= input$score_min
      )

    if (input$genre_filter  != "Tous")
      df <- df %>% filter(genre  == input$genre_filter)

    if (input$rating_filter != "Tous")
      df <- df %>% filter(rating == input$rating_filter)

    df
  })

  # Réinitialisation des filtres
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "year_range", value = year_range)
    updateSelectInput(session, "genre_filter",  selected = "Tous")
    updateSelectInput(session, "rating_filter", selected = "Tous")
    updateSliderInput(session, "score_min", value = 1)
  })

  # ── KPI ──────────────────────────────────────────────────────────────────
  output$kpi_films <- renderValueBox({
    valueBox(
      value    = format(nrow(movies_filtered()), big.mark = " "),
      subtitle = "Films sélectionnés",
      icon     = icon("film"),
      color    = "blue"
    )
  })

  output$kpi_score <- renderValueBox({
    valueBox(
      value    = round(mean(movies_filtered()$score, na.rm = TRUE), 2),
      subtitle = "Score IMDb moyen",
      icon     = icon("star"),
      color    = "yellow"
    )
  })

  output$kpi_budget <- renderValueBox({
    valueBox(
      value    = paste0("$", round(mean(movies_filtered()$budget_m, na.rm = TRUE), 0), "M"),
      subtitle = "Budget moyen",
      icon     = icon("dollar-sign"),
      color    = "red"
    )
  })

  output$kpi_gross <- renderValueBox({
    valueBox(
      value    = paste0("$", round(mean(movies_filtered()$gross_m, na.rm = TRUE), 0), "M"),
      subtitle = "Recettes moyennes",
      icon     = icon("chart-line"),
      color    = "green"
    )
  })

  # ── Graphiques ONGLET 1 ───────────────────────────────────────────────────

  output$plot_evol_annuelle <- renderPlotly({
    movies_filtered() %>%
      count(year) %>%
      plot_ly(x = ~year, y = ~n, type = "scatter", mode = "lines+markers",
              line = list(color = "#2196F3"),
              marker = list(color = "#2196F3")) %>%
      layout(xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre de films"),
             hovermode = "x")
  })

  output$plot_distrib_score <- renderPlotly({
    plot_ly(movies_filtered(), x = ~score,
            type = "histogram",
            marker = list(color = "#4CAF50", line = list(color = "white", width = 1))) %>%
      layout(xaxis = list(title = "Score IMDb"),
             yaxis = list(title = "Fréquence"))
  })

  output$plot_budget_gross <- renderPlotly({
    movies_filtered() %>%
      filter(!is.na(budget_m), !is.na(gross_m)) %>%
      plot_ly(x = ~budget_m, y = ~gross_m,
              type = "scatter", mode = "markers",
              color = ~genre,
              text  = ~paste0(name, "<br>Score: ", score),
              hoverinfo = "text",
              marker = list(opacity = 0.5, size = 6)) %>%
      layout(xaxis = list(title = "Budget (M$)", type = "log"),
             yaxis = list(title = "Recettes (M$)", type = "log"))
  })

  output$plot_score_rating <- renderPlotly({
    movies_filtered() %>%
      group_by(rating) %>%
      summarise(score_moy = mean(score, na.rm = TRUE), n = n()) %>%
      plot_ly(x = ~rating, y = ~score_moy,
              type = "bar",
              text = ~paste0("n=", n),
              textposition = "outside",
              marker = list(color = "#FF6B35")) %>%
      layout(xaxis = list(title = "Classification"),
             yaxis = list(title = "Score moyen", range = c(4, 8)))
  })

  # ── Graphiques ONGLET 2 ───────────────────────────────────────────────────

  output$plot_genre_score <- renderPlotly({
    movies_filtered() %>%
      group_by(genre) %>%
      summarise(score_moy = mean(score, na.rm = TRUE), n = n()) %>%
      filter(n >= 5) %>%
      arrange(score_moy) %>%
      plot_ly(x = ~score_moy, y = ~reorder(genre, score_moy),
              type = "bar", orientation = "h",
              marker = list(color = ~score_moy,
                            colorscale = "Viridis")) %>%
      layout(xaxis = list(title = "Score moyen"),
             yaxis = list(title = ""))
  })

  output$plot_genre_roi <- renderPlotly({
    movies_filtered() %>%
      filter(!is.na(roi)) %>%
      group_by(genre) %>%
      summarise(roi_moy = mean(roi, na.rm = TRUE), n = n()) %>%
      filter(n >= 5) %>%
      arrange(roi_moy) %>%
      plot_ly(x = ~roi_moy, y = ~reorder(genre, roi_moy),
              type = "bar", orientation = "h",
              marker = list(color = ifelse(
                (movies_filtered() %>%
                   filter(!is.na(roi)) %>%
                   group_by(genre) %>%
                   summarise(roi_moy = mean(roi, na.rm = TRUE), n = n()) %>%
                   filter(n >= 5) %>%
                   arrange(roi_moy))$roi_moy > 0,
                "#4CAF50", "#F44336"
              ))) %>%
      layout(xaxis = list(title = "ROI moyen (%)"),
             yaxis = list(title = ""))
  })

  output$plot_genre_evol <- renderPlotly({
    top5 <- movies_filtered() %>%
      count(genre) %>% top_n(5, n) %>% pull(genre)

    movies_filtered() %>%
      filter(genre %in% top5) %>%
      group_by(year, genre) %>%
      summarise(n = n(), .groups = "drop") %>%
      plot_ly(x = ~year, y = ~n, color = ~genre,
              type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Année"),
             yaxis = list(title = "Nombre de films"),
             hovermode = "x unified")
  })

  # ── Cartographie ONGLET 3 ─────────────────────────────────────────────────

  output$carte_monde <- renderLeaflet({
    coords_pays <- tibble(
      country = c("United States", "United Kingdom", "France",
                  "Canada", "Germany", "Australia", "Japan",
                  "India", "Italy", "Spain", "China", "Mexico",
                  "Brazil", "Ireland", "South Korea"),
      lat = c(37.1, 55.4, 46.2, 56.1, 51.2, -25.3, 36.2,
              20.6, 41.9, 40.5, 35.9, 23.6, -14.2, 53.4, 35.9),
      lng = c(-95.7, -3.4, 2.2, -106.3, 10.5, 133.8, 138.3,
              78.9, 12.6, -3.7, 104.2, -102.6, -51.9, -8.2, 127.8)
    )

    carte_data <- movies_filtered() %>%
      group_by(country) %>%
      summarise(
        n_films   = n(),
        score_moy = round(mean(score, na.rm = TRUE), 2),
        gross_moy = round(mean(gross_m, na.rm = TRUE), 1)
      ) %>%
      inner_join(coords_pays, by = "country")

    pal <- colorNumeric("Blues", domain = log10(carte_data$n_films))

    leaflet(carte_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~log10(n_films) * 10,
        fillColor = ~pal(log10(n_films)),
        fillOpacity = 0.8,
        stroke = TRUE, weight = 1, color = "white",
        popup = ~paste0(
          "<b>", country, "</b><br>",
          "🎬 Films : ", n_films, "<br>",
          "⭐ Score : ", score_moy, "<br>",
          "📈 Recettes moy : $", gross_moy, "M"
        )
      )
  })

  output$plot_top_pays <- renderPlotly({
    movies_filtered() %>%
      count(country) %>%
      top_n(15, n) %>%
      arrange(n) %>%
      plot_ly(x = ~n, y = ~reorder(country, n),
              type = "bar", orientation = "h",
              marker = list(color = "#1976D2")) %>%
      layout(xaxis = list(title = "Nombre de films"),
             yaxis = list(title = ""))
  })

  output$plot_score_pays <- renderPlotly({
    movies_filtered() %>%
      group_by(country) %>%
      summarise(score_moy = mean(score, na.rm = TRUE), n = n()) %>%
      filter(n >= 20) %>%
      top_n(10, score_moy) %>%
      arrange(score_moy) %>%
      plot_ly(x = ~score_moy, y = ~reorder(country, score_moy),
              type = "bar", orientation = "h",
              marker = list(color = "#FF6B35")) %>%
      layout(xaxis = list(title = "Score moyen"),
             yaxis = list(title = ""))
  })

  # ── Table ONGLET 4 ────────────────────────────────────────────────────────

  output$table_films <- renderDT({
    df <- movies_filtered()

    if (nchar(input$search_film) > 0) {
      df <- df %>% filter(str_detect(tolower(name),
                                     tolower(input$search_film)))
    }

    df %>%
      select(name, year, genre, rating, score, budget_m,
             gross_m, profit_m, director, country) %>%
      rename(
        "Titre"      = name,
        "Année"      = year,
        "Genre"      = genre,
        "Rating"     = rating,
        "Score"      = score,
        "Budget(M$)" = budget_m,
        "Recettes(M$)" = gross_m,
        "Profit(M$)" = profit_m,
        "Réalisateur" = director,
        "Pays"       = country
      ) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "Bfrtip"
        ),
        rownames = FALSE,
        filter   = "top"  # filtres en haut de chaque colonne
      ) %>%
      formatRound(c("Score", "Budget(M$)", "Recettes(M$)", "Profit(M$)"), digits = 1)
  })

  # Téléchargement CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("movies_filtre_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(movies_filtered(), file)
    }
  )
}

# ── Lancement de l'application ────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
