# ============================================================================
# titre     : ANALYSE COMPLETE DU DATASET IMDB MOVIES----------------
# Dataset   : movies.csv (7668 films, 1980-2020)
# Auteur    : Ahmed bechir 
# Matricule : C34639
# ============================================================================

# ── 1. Chargement des bibliothèques ─────────────────────────────────────────
library(tidyverse)   # manipulation et visualisation des données
library(naniar)      # visualisation des valeurs manquantes
library(mice)        # imputation des valeurs manquantes
library(pastecs)     # statistiques descriptives avancées
library(lubridate)   # manipulation des dates
library(plotly)      # graphiques interactifs
library(leaflet)     # carte interactive
library(sf)          # données spatiales (shapefiles)
library(rnaturalearth)     # frontières des pays du monde
library(rnaturalearthdata) # données géographiques
library(naniar)
library(ggplot2)
library(dplyr)
# ── 2. Importation des données ───────────────────────────────────────────────
movies <- read_csv("movies.csv")

# Vérification rapide
dim(movies)          # dimensions : 7668 lignes, 15 colonnes
str(movies)          # structure des variables
head(movies, 5)      # 5 premières lignes
summary(movies)      # résumé statistique global


# ── 3. Nettoyage des types de variables ─────────────────────────────────────

movies <- movies %>%
  mutate(
    rating  = as.factor(rating),
    genre   = as.factor(genre),
    country = as.factor(country),
    released_clean = mdy(str_extract(released, "^[^(]+")),
    release_month  = month(released_clean, label = TRUE)
  )

cat("✔ Types de variables corrigés\n")


# ── 4. Analyse des valeurs manquantes ────────────────────────────────────────

cat("\n=== VALEURS MANQUANTES ===\n")

# Nombre de NA par colonne
na_counts <- colSums(is.na(movies))
print(na_counts)

# Pourcentage de NA par colonne
na_pct <- round(na_counts / nrow(movies) * 100, 1)
print(na_pct)

# Résumé sous forme de tableau
na_summary <- data.frame(
  variable       = names(na_counts),
  na_count       = na_counts,
  na_percentage  = na_pct
) %>%
  arrange(desc(na_percentage)) %>%
  filter(na_count > 0)

print(na_summary)
# → budget : 28.3% manquant (colonne la plus problématique)
# → gross  :  2.5% manquant
# → rating :  1.0% manquant

# Visualisation des patterns de NA (carte des manquants)

miss_data <- miss_var_summary(movies) %>%
  mutate(pct_miss = as.numeric(pct_miss))

# Graphique en barres
p <- ggplot(
  miss_data,
  aes(
    x = reorder(variable, pct_miss),
    y = pct_miss
  )
) +
  geom_col(fill = "steelblue") +
  
  # Affichage du pourcentage
  geom_text(
    aes(label = paste0(round(pct_miss, 1), "%")),
    hjust = -0.1,
    size = 4
  ) +
  
  labs(
    title = "Pourcentage de valeurs manquantes par variable",
    x = "Variable",
    y = "% manquant"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  coord_flip()
print(p)
ggsave(
  "graphique_manquants.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)



# ── 5. Stratégie de traitement des NA ────────────────────────────────────────

# STRATÉGIE CHOISIE :
# - budget (28%) → imputation par la médiane par genre
#   (un film d'Action a un budget différent d'un film Dramatique)
# - gross  (2.5%) → suppression des lignes (peu d'impact)
# - rating (1.0%) → catégorie "Unknown"
# - autres NA (<1%) → suppression des lignes

# 5.1 Imputation du budget par médiane de genre
mediane_globale <- median(movies$budget, na.rm = TRUE)

movies <- movies %>%
  group_by(genre) %>%
  mutate(mediane_genre = median(budget, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    budget = case_when(
      !is.na(budget)        ~ budget,
      !is.na(mediane_genre) ~ mediane_genre,
      TRUE                  ~ mediane_globale
    )
  ) %>%
  select(-mediane_genre)

movies <- movies %>%
  mutate(
    profit = ifelse(!is.na(gross),
                    (gross - budget) / 1e6, NA),
    roi    = ifelse(!is.na(gross) & budget > 0,
                    (gross - budget) / budget * 100, NA)
  )
cat("NA restants :", sum(is.na(movies$budget)), "\n")
#Calcul roi et profit APRÈS imputation
movies <- movies %>%
  mutate(
    profit = ifelse(!is.na(gross),
                    (gross - budget) / 1e6, NA),
    roi    = ifelse(!is.na(gross) & budget > 0,
                    (gross - budget) / budget * 100, NA)
  )

#Remplacer company NA
movies <- movies %>%
  mutate(company = replace_na(company, "Unknown"))

# 5.2 Rating manquant → "Unknown"
movies <- movies %>%
  mutate(rating = fct_na_value_to_level(rating, level = "Unknown"))

# 5.3 Suppression des lignes avec NA restants (gross, etc.)
movies_clean <- movies %>%
  filter(!is.na(gross), !is.na(score), !is.na(runtime))

cat("Dimensions après nettoyage :", nrow(movies_clean), "x", ncol(movies_clean), "\n")
# On passe de 7668 à ~7470 films (perte minime)
movies_clean <- movies_clean %>%
  filter(!is.na(writer), !is.na(star), !is.na(country))
movies_clean <- movies_clean %>%
  mutate(company = replace_na(company, "Unknown"))
#Supprimer les lignes avec writer / star / country NA
movies_clean <- movies_clean %>%
  filter(!is.na(writer), !is.na(star), !is.na(country))


# Vérification : plus aucun NA critique
cat("NA restants dans budget :", sum(is.na(movies_clean$budget)), "\n")
cat("NA restants dans gross  :", sum(is.na(movies_clean$gross)), "\n")
cat("NA restants dans gross  :", sum(is.na(movies_clean$roi)), "\n")
na_counts <- colSums(is.na(movies_clean))
print(na_counts)
write_csv(movies_clean, "movies_clean.csv")
# ── 6. Statistiques descriptives ─────────────────────────────────────────────

cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")

# Variables numériques clés
vars_num <- movies_clean %>%
  select(score, votes, budget, gross, runtime, roi)

# stat.desc() du package pastecs : détaillé (min, max, moyenne, médiane, écart-type...)
stat.desc(vars_num) %>% round(2)

# Résumé rapide avec tidyverse
movies_clean %>%
  summarise(
    score_moy   = mean(score,   na.rm = TRUE),
    score_med   = median(score, na.rm = TRUE),
    score_sd    = sd(score,     na.rm = TRUE),
    budget_moy  = mean(budget,  na.rm = TRUE) / 1e6,   # en millions
    gross_moy   = mean(gross,   na.rm = TRUE) / 1e6,
    runtime_moy = mean(runtime, na.rm = TRUE)
  )
# → score moyen ~6.4, budget moyen ~30M$, durée moyenne ~109 min

# Statistiques par genre
movies_clean %>%
  group_by(genre) %>%
  summarise(
    n_films     = n(),
    score_moy   = round(mean(score), 2),
    budget_moy  = round(mean(budget, na.rm = TRUE) / 1e6, 1),
    gross_moy   = round(mean(gross,  na.rm = TRUE) / 1e6, 1),
    roi_moy     = round(mean(roi,    na.rm = TRUE), 1)
  ) %>%
  arrange(desc(n_films)) %>%
  print(n = 20)


# ── 7. Détection des valeurs aberrantes (outliers) ───────────────────────────



# Fonction générique de détection par la méthode IQR
detect_outliers <- function(x, var_name) {
  Q1  <- quantile(x, 0.25, na.rm = TRUE)
  Q3  <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  n_out <- sum(x < lower | x > upper, na.rm = TRUE)
  cat(sprintf("%-10s | Q1=%.0f | Q3=%.0f | seuils=[%.0f, %.0f] | outliers=%d\n",
              var_name, Q1, Q3, lower, upper, n_out))
}

detect_outliers(movies_clean$score,   "score")
detect_outliers(movies_clean$budget,  "budget")
detect_outliers(movies_clean$gross,   "gross")
detect_outliers(movies_clean$runtime, "runtime")

# Identifier les films avec budget ou recettes extrêmes
movies_clean %>%
  filter(budget > quantile(budget, 0.99, na.rm = TRUE)) %>%
  select(name, year, genre, budget, gross) %>%
  arrange(desc(budget))
# → Films à très gros budget (Star wars, Avengers, etc.)

# ── 8. Sauvegarde du dataset nettoyé ─────────────────────────────────────────
write_csv(movies_clean, "movies_clean.csv")
cat("\n✔ Dataset nettoyé sauvegardé : movies_clean.csv\n")
cat("  Dimensions finales :", nrow(movies_clean), "x", ncol(movies_clean), "\n")


# ============================================================================

# -  ANALYSES STATISTIQUES AVANCÉES

# ============================================================================


movies <- read_csv("movies_clean.csv") 





# ── 2. Matrice de corrélation ────────────────────── ───────────────────────────

# Sélection des variables numériques
num_vars <- movies %>%
  select(score, votes, budget, gross, runtime, roi) %>%
  drop_na()

# Calcul de la matrice de corrélation (méthode Pearson)
cor_matrix <- cor(num_vars, method = "pearson")

# Affichage numérique
print(round(cor_matrix, 2))


# Visualisation avec corrplot
corrplot(
  cor_matrix,
  method  = "color",       # carrés colorés
  type    = "upper",       # triangle supérieur seulement
  addCoef.col = "black",   # afficher les coefficients
  tl.col  = "black",
  tl.srt  = 45,
  title   = "Corrélations entre variables numériques",
  mar     = c(0, 0, 2, 0)
)
# INTERPRÉTATION :
# - budget ↔ gross : 0.74 → les gros budgets rapportent plus
# - score  ↔ votes : 0.40 → films populaires légèrement mieux notés
# - roi    ↔ budget: négatif → les petits budgets ont parfois meilleur ROI


# ── 3. Évolution temporelle ───────────────────────────────────────────────────

# 3.1 Nombre de films produits par année
films_par_an <- movies %>%
  count(year) %>%
  rename(n_films = n)

ggplot(films_par_an, aes(x = year, y = n_films)) +
  geom_line(color = "#1565C0", linewidth = 1.3) +
  geom_point(color = "#0D47A1", fill = "#BBDEFB", shape = 21, size = 3, stroke = 1) +
  geom_smooth(method = "loess", color = "#E53935", linewidth = 1.2, linetype = "dashed", se = FALSE) +
  labs(
    title = "Évolution du nombre de films produits",
    subtitle = "Analyse des productions cinématographiques entre 1980 et 2020",
    x = "Année",
    y = "Nombre de films",
    caption = "Source : Dataset Movies"
  ) +
  scale_x_continuous(
    breaks = seq(min(films_par_an$year), max(films_par_an$year), by = 5)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#0D47A1", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "gray50", face = "italic")
  )

# 3.2 Score moyen par année
score_par_an <- movies %>%
  group_by(year) %>%
  summarise(
    score_moy = mean(score, na.rm = TRUE),
    score_sd  = sd(score, na.rm = TRUE),
    n         = n()
  )

ggplot(score_par_an, aes(x = year, y = score_moy)) +
  geom_ribbon(aes(ymin = score_moy - score_sd,
                  ymax = score_moy + score_sd),
              alpha = 0.15, fill = "#4CAF50") +
  geom_line(color = "#4CAF50", linewidth = 1) +
  geom_point(color = "#4CAF50", fill = "#BBDEFB", shape = 21, size = 3, stroke = 1)+
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(
    title    = "Évolution du score moyen par année (1980-2020)",
    subtitle = "Zone verte = ± 1 écart-type | Ligne rouge = tendance linéaire",
    x = "Année", y = "Score moyen IMDb"
  ) +
  ylim(4, 9) +
  theme_minimal()

# 3.3 Budget moyen par décennie
movies %>%
  mutate(decennie = paste0(floor(year / 10) * 10, "s")) %>%
  group_by(decennie) %>%
  summarise(
    budget_moy = mean(budget, na.rm = TRUE) / 1e6,
    gross_moy  = mean(gross,  na.rm = TRUE) / 1e6,
    n          = n()
  ) %>%
  pivot_longer(cols = c(budget_moy, gross_moy),
               names_to = "type", values_to = "valeur") %>%
  ggplot(aes(x = decennie, y = valeur, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("budget_moy" = "#FF6B35",
                               "gross_moy"  = "#4CAF50"),
                    labels = c("Budget moyen (M$)", "Recettes moyennes (M$)")) +
  labs(
    title = "Budget vs Recettes moyens par décennie",
    x = "Décennie", y = "Montant (millions $)", fill = ""
  ) +
  theme_minimal()


# ── 4. Analyse par genre ──────────────────────────────────────────────────────

# 4.1 Score moyen par genre (top 10 genres)
top_genres <- movies %>%
  count(genre) %>%
  top_n(10, n) %>%
  pull(genre)

movies %>%
  filter(genre %in% top_genres) %>%
  group_by(genre) %>%
  summarise(score_moy = mean(score, na.rm = TRUE)) %>%
  arrange(score_moy) %>%
  mutate(genre = factor(genre, levels = genre)) %>%
  ggplot(aes(x = score_moy, y = genre)) +
  geom_segment(aes(x = 0, xend = score_moy, y = genre, yend = genre),
               color = "#E3F2FD", linewidth = 2) +
  geom_point(size = 5, color = "#0D47A1") +
  geom_text(aes(label = round(score_moy, 2)),
            hjust = -0.3, size = 4, color = "#0D47A1", fontface = "bold") +
  labs(
    title = "IMDB Score par Genre",
    subtitle = "Genres classés par score moyen",
    x = "Score moyen",
    y = "Genre"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#0D47A1"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# 4.2 Rentabilité par genre (ROI moyen)
movies %>%
  filter(genre %in% top_genres, !is.na(roi)) %>%
  group_by(genre) %>%
  summarise(roi_moy = mean(roi, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, roi_moy), y = roi_moy, fill = roi_moy > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  labs(
    title = "ROI moyen par genre (Retour sur Investissement)",
    subtitle = "Vert = rentable | Rouge = perte",
    x = "Genre", y = "ROI moyen (%)"
  ) +
  theme_minimal()


# ── 5. Top réalisateurs ───────────────────────────────────────────────────────

# Top 15 réalisateurs par score moyen (avec au moins 5 films)
movies %>%
  group_by(director) %>%
  summarise(
    n_films = n(),
    score_moy = mean(score, na.rm = TRUE)
  ) %>%
  filter(n_films >= 5) %>%
  top_n(15, score_moy) %>%
  arrange(score_moy) %>%
  mutate(director = factor(director, levels = director)) %>%
  ggplot(aes(x = score_moy, y = director)) +
  geom_point(size = 4, color = "#1565C0") +
  geom_segment(aes(x = 0, xend = score_moy, y = director, yend = director),
               color = "#BBDEFB", linewidth = 1) +
  geom_text(aes(label = round(score_moy, 2)),
            hjust = -0.3, size = 3.5, color = "#0D47A1") +
  labs(
    title = "Top 15 réalisateurs IMDb",
    subtitle = "Classement par score moyen (min 5 films)",
    x = "Score moyen",
    y = "Réalisateur"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


# ── 6. Analyse de la saisonnalité ─────────────────────────────────────────────

# Films sortis par mois (saisonnalité des sorties)
movies %>%
  filter(!is.na(release_month)) %>%
  count(release_month) %>%
  mutate(release_month = as.character(release_month)) %>%
  ggplot(aes(x = reorder(release_month, n), y = n, fill = n)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  scale_fill_gradient(low = "#BBDEFB", high = "#1565C0") +
  labs(
    title = "Nombre de films sortis par mois",
    x = "Mois",
    y = "Nombre de films"
  ) +
  theme_minimal(base_size = 13)

# Score et recettes moyens par mois de sortie
movies %>%
  filter(!is.na(release_month)) %>%
  group_by(release_month) %>%
  summarise(
    gross_moy = mean(gross, na.rm = TRUE) / 1e6,
    score_moy = mean(score, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = release_month, y = gross_moy, group = 1)) +
  geom_line(color = "#E91E63", linewidth = 1.2) +
  geom_point(aes(size = score_moy), color = "#E91E63") +
  labs(
    title    = "Recettes moyennes par mois de sortie",
    subtitle = "Taille des points = score IMDb moyen",
    x = "Mois", y = "Recettes moyennes (M$)", size = "Score"
  ) +
  theme_minimal()


# ── 7. Analyse du rating ──────────────────────────────────────────────────────

# Distribution des ratings
movies %>%
  count(rating) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = reorder(rating, n), y = n, fill = rating)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Distribution des classifications (rating)",
    x = "Classification", y = "Nombre de films"
  ) +
  theme_minimal()

# Score par rating
movies %>%
  ggplot(aes(x = rating, y = score, fill = rating)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  labs(
    title    = "Distribution des scores par classification",
    subtitle = "Les films NC-17 ont souvent des scores plus faibles",
    x = "Classification", y = "Score IMDb"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cat("\n✔ Script 02 terminé — toutes les analyses statistiques générées\n")

# ============================================================================

# - VISUALISATION ET CARTOGRAPHIE

# ============================================================================

# ──  Graphiques statiques avec ggplot2 ──────────────────────────────────────

#  Distribution des scores (histogramme + densité)
p1 <- ggplot(movies, aes(x = score)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "#1976D2", alpha = 0.7, color = "white") +
  geom_density(color = "#E91E63", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(score)), color = "red",
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(movies$score) + 0.3,
           y = 0.55, label = paste("Moy =", round(mean(movies$score), 2)),
           color = "red", size = 3.5) +
  labs(
    title    = "Distribution des scores IMDb",
    subtitle = "La majorité des films est notée entre 5.5 et 7.5",
    x = "Score IMDb", y = "Densité"
  ) +
  theme_minimal()

print(p1)
ggsave("output/01_distribution_scores.png", p1, width = 8, height = 5, dpi = 150)

#  Budget vs Recettes (scatter plot avec coloration par genre)
top5_genres <- movies %>% count(genre) %>% top_n(5, n) %>% pull(genre)

p2 <- movies %>%
  filter(genre %in% top5_genres) %>%
  mutate(
    budget_m = budget / 1e6,
    gross_m  = gross  / 1e6
  ) %>%
  ggplot(aes(x = budget_m, y = gross_m, color = genre, text = name)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black") +  # ligne de break-even
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Budget vs Recettes (échelle logarithmique)",
    subtitle = "Au-dessus de la diagonale = film rentable",
    x = "Budget (M$)", y = "Recettes (M$)", color = "Genre"
  ) +
  theme_minimal()

print(p2)
ggsave("output/02_budget_vs_recettes.png", p2, width = 9, height = 6, dpi = 150)

# Boxplot des scores par genre (top 10)
top10_genres <- movies %>% count(genre) %>% top_n(10, n) %>% pull(genre)

p3 <- movies %>%
  filter(genre %in% top10_genres) %>%
  ggplot(aes(x = reorder(genre, score, median),
             y = score, fill = genre)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2, outlier.size = 0.8) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title    = "Distribution des scores IMDb par genre",
    subtitle = "Médiane, quartiles et valeurs aberrantes",
    x = "Genre", y = "Score IMDb"
  ) +
  theme_minimal()

print(p3)
ggsave("output/03_scores_par_genre.png", p3, width = 8, height = 6, dpi = 150)

#  Heatmap : score moyen par genre et décennie
p4 <- movies %>%
  filter(genre %in% top10_genres) %>%
  mutate(decennie = paste0(floor(year / 10) * 10, "s")) %>%
  group_by(genre, decennie) %>%
  summarise(score_moy = mean(score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = decennie, y = genre, fill = score_moy)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(score_moy, 1)), color = "black", size = 3) +
  scale_fill_gradient2(low = "#F44336", mid = "#FFC107", high = "#4CAF50",
                       midpoint = 6.5, name = "Score\nmoyen") +
  labs(
    title    = "Score moyen par genre et par décennie",
    subtitle = "Vert = bien noté | Rouge = moins bien noté",
    x = "Décennie", y = "Genre"
  ) +
  theme_minimal()

print(p4)
ggsave("output/04_heatmap_genre_decennie.png", p4, width = 9, height = 6, dpi = 150)


# ──  Graphiques interactifs avec plotly ─────────────────────────────────────

#  Scatter interactif : budget vs recettes (survol = nom du film)
scatter_interactif <- movies %>%
  filter(!is.na(budget), !is.na(gross)) %>%
  mutate(
    budget_m = round(budget / 1e6, 1),
    gross_m  = round(gross  / 1e6, 1),
    profit_m = round((gross - budget) / 1e6, 1)
  ) %>%
  plot_ly(
    x    = ~budget_m,
    y    = ~gross_m,
    type = "scatter",
    mode = "markers",
    color = ~genre,
    size  = ~score,
    text  = ~paste0(
      "<b>", name, "</b><br>",
      "Année : ", year, "<br>",
      "Genre : ", genre, "<br>",
      "Budget : $", budget_m, "M<br>",
      "Recettes : $", gross_m, "M<br>",
      "Profit : $", profit_m, "M<br>",
      "Score : ", score
    ),
    hoverinfo = "text",
    marker = list(opacity = 0.6)
  ) %>%
  layout(
    title  = "Budget vs Recettes (interactif)",
    xaxis  = list(title = "Budget (M$)", type = "log"),
    yaxis  = list(title = "Recettes (M$)", type = "log"),
    legend = list(title = list(text = "Genre"))
  )

# Sauvegarder en HTML interactif
htmlwidgets::saveWidget(scatter_interactif,
                        "output/05_scatter_interactif.html",
                        selfcontained = TRUE)

#  Évolution temporelle interactive
evol_data <- movies %>%
  group_by(year) %>%
  summarise(
    n_films    = n(),
    score_moy  = round(mean(score, na.rm = TRUE), 2),
    budget_moy = round(mean(budget, na.rm = TRUE) / 1e6, 1),
    gross_moy  = round(mean(gross,  na.rm = TRUE) / 1e6, 1)
  )

evol_interactif <- plot_ly(evol_data, x = ~year) %>%
  add_lines(y = ~n_films,    name = "Nb films",          yaxis = "y2",
            line = list(color = "#2196F3")) %>%
  add_lines(y = ~score_moy,  name = "Score moyen",
            line = list(color = "#4CAF50")) %>%
  add_lines(y = ~budget_moy, name = "Budget moy (M$)",
            line = list(color = "#FF6B35", dash = "dash")) %>%
  layout(
    title  = "Évolution annuelle : films, scores et budgets",
    xaxis  = list(title = "Année"),
    yaxis  = list(title = "Score / Budget (M$)"),
    yaxis2 = list(title = "Nombre de films", overlaying = "y",
                  side = "right"),
    hovermode = "x unified"
  )

htmlwidgets::saveWidget(evol_interactif,
                        "output/06_evolution_temporelle.html",
                        selfcontained = TRUE)

# ──TOP 10 DES PAYS PRODUCTEURS DE FILMS ─────────────────────────────────────────────


top_pays <- movies %>%
  count(country, sort = TRUE) %>%
  slice(1:10)

ggplot(top_pays,
       aes(x = reorder(country, n),
           y = n,
           fill = n)) +
  
  # Barres
  geom_col(width = 0.7,
           show.legend = FALSE) +
  
  # Valeurs sur les barres
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.15,
            size = 4.5,
            fontface = "bold",
            color = "#0D47A1") +
  
  # Orientation horizontale
  coord_flip() +
  
  # Dégradé élégant
  scale_fill_gradient(
    low  = "#BBDEFB",
    high = "#0D47A1"
  ) +
  
  # Titres
  labs(
    title = "Top 10 des pays producteurs de films",
    subtitle = "Production cinématographique mondiale (1980–2020)",
    x = "",
    y = "Nombre de films",
    caption = "Source : Movies Dataset"
  ) +
  
  # Axe Y plus espacé
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  # Style général
  theme_minimal(base_size = 15) +
  
  theme(
    
    # Titre principal
    plot.title = element_text(
      face = "bold",
      size = 22,
      color = "#0D47A1",
      hjust = 0.5
    ),
    
    # Sous-titre
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    
    # Axes
    axis.title = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text.y = element_text(
      face = "bold",
      color = "#212121",
      size = 12
    ),
    
    axis.text.x = element_text(
      color = "#424242"
    ),
    
    # Supprimer grilles inutiles
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Caption
    plot.caption = element_text(
      color = "gray50",
      face = "italic"
    )
  )

# ──  Cartographie avec leaflet ─────────────────────────────────────────────

#  Préparer les données par pays
# Harmonisation des noms de pays pour jointure avec carte mondiale
pays_stats <- movies %>%
  mutate(
    # Uniformiser les noms (rnaturalearth utilise des noms en anglais)
    country = recode(country,
                     "United States"    = "United States of America",
                     "United Kingdom"   = "United Kingdom",
                     "South Korea"      = "South Korea",
                     "Hong Kong"        = "China"  # simplification
    )
  ) %>%
  group_by(country) %>%
  summarise(
    n_films    = n(),
    score_moy  = round(mean(score, na.rm = TRUE), 2),
    budget_moy = round(mean(budget, na.rm = TRUE) / 1e6, 1),
    gross_moy  = round(mean(gross,  na.rm = TRUE) / 1e6, 1)
  )

#  Charger la carte mondiale (polygones des pays)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Jointure entre carte et statistiques
world_movies <- world %>%
  left_join(pays_stats, by = c("name_long" = "country"))

#  Carte statique (ggplot2 + sf)
p_carte <- ggplot(world_movies) +
  geom_sf(aes(fill = n_films), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1",
                      na.value = "grey85",
                      name = "Nombre\nde films",
                      trans = "log10") +
  labs(
    title    = "Production cinématographique mondiale",
    subtitle = "Nombre de films dans le dataset (1980-2020)",
    caption  = "Source : IMDb Movies dataset"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

print(p_carte)
ggsave("output/07_carte_mondiale.png", p_carte, width = 12, height = 7, dpi = 150)

#  Carte interactive leaflet (avec infobulles au survol)
# Coordonnées approximatives des capitales/centres des principaux pays
coords_pays <- tibble(
  country    = c("United States of America", "United Kingdom", "France",
                 "Canada", "Germany", "Australia", "Japan",
                 "India", "Italy", "Spain"),
  lat        = c(37.1, 55.4, 46.2, 56.1, 51.2, -25.3, 36.2,
                 20.6, 41.9, 40.5),
  lng        = c(-95.7, -3.4, 2.2, -106.3, 10.5, 133.8, 138.3,
                 78.9, 12.6, -3.7)
)

carte_data <- pays_stats %>%
  inner_join(coords_pays, by = "country")

# Palette de couleurs selon nombre de films
pal <- colorNumeric(palette = "Blues",
                    domain  = log10(carte_data$n_films))

carte_leaflet <- leaflet(carte_data) %>%
  addTiles() %>%  # fond de carte OpenStreetMap
  addCircleMarkers(
    lng    = ~lng,
    lat    = ~lat,
    radius = ~log10(n_films) * 8,
    
    fillColor   = ~pal(log10(n_films)),  # couleur intérieure
    fillOpacity = 0.8,
    
    color  = "white",   # bordure
    stroke = TRUE,
    weight = 1,
    
    popup  = ~paste0(
      "<b>", country, "</b><br>",
      "🎬 Films : ",        n_films,    "<br>",
      "⭐ Score moyen : ",  score_moy,  "<br>",
      "💰 Budget moy : $",  budget_moy, "M<br>",
      "📈 Recettes moy : $", gross_moy, "M"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal,
    values   = ~log10(n_films),
    title    = "log10(Nb films)",
    opacity  = 0.8
  ) %>%
  addControl(
    "<b>Production mondiale de films (1980-2020)</b>",
    position = "topright"
  )

# Sauvegarder la carte interactive
htmlwidgets::saveWidget(carte_leaflet,
                        "output/08_carte_leaflet.html",
                        selfcontained = TRUE)



