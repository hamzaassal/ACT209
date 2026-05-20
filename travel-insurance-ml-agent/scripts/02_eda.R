# 02_eda.R
# Analyse exploratoire du risque de sinistre dans le dataset Travel Insurance.

required_packages <- c("dplyr", "ggplot2", "readr", "tidyr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(here)

dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

data_path <- here("data", "processed", "data_clean.rds")
if (!file.exists(data_path)) {
  stop("Fichier introuvable : data/processed/data_clean.rds. Executez scripts/01_import_data.R.")
}

data_clean <- readRDS(data_path)
target_col <- "claim_status"

if (!target_col %in% names(data_clean)) {
  stop("Variable cible introuvable : claim_status")
}

dimensions <- tibble(n_rows = nrow(data_clean), n_cols = ncol(data_clean))

structure_summary <- tibble(
  variable = names(data_clean),
  class = vapply(data_clean, function(x) paste(class(x), collapse = "/"), character(1)),
  missing_count = vapply(data_clean, function(x) sum(is.na(x)), integer(1)),
  distinct_count = vapply(data_clean, dplyr::n_distinct, integer(1))
)

numeric_summary <- data_clean %>%
  select(where(is.numeric)) %>%
  summarise(
    across(
      everything(),
      list(
        min = ~ min(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")

target_distribution <- data_clean %>%
  count(claim_status) %>%
  mutate(share = n / sum(n))

write_csv(dimensions, here("reports", "eda_dimensions.csv"))
write_csv(structure_summary, here("reports", "eda_structure.csv"))
write_csv(numeric_summary, here("reports", "eda_numeric_summary.csv"))
write_csv(target_distribution, here("reports", "target_distribution.csv"))

print(dimensions)
print(structure_summary)
print(target_distribution)

p_target <- ggplot(data_clean, aes(x = claim_status, fill = claim_status)) +
  geom_bar(width = 0.7) +
  scale_fill_manual(values = c(no = "#2b6cb0", yes = "#c53030")) +
  labs(
    title = "Distribution de la cible : survenance de sinistre",
    x = "Sinistre observe",
    y = "Nombre de contrats"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("reports", "figures", "target_distribution.png"), p_target, width = 7, height = 5)

numeric_vars <- data_clean %>% select(where(is.numeric)) %>% names()

if (length(numeric_vars) > 0) {
  numeric_long <- data_clean %>%
    select(all_of(c(target_col, numeric_vars))) %>%
    pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "value")

  p_numeric <- ggplot(numeric_long, aes(x = claim_status, y = value, fill = claim_status)) +
    geom_boxplot(alpha = 0.85, outlier.alpha = 0.15) +
    facet_wrap(~ variable, scales = "free_y") +
    scale_fill_manual(values = c(no = "#2b6cb0", yes = "#c53030")) +
    labs(title = "Variables numeriques selon la survenance de sinistre", x = "Sinistre", y = "Valeur") +
    theme_minimal() +
    theme(legend.position = "none")

  ggsave(here("reports", "figures", "numeric_by_target.png"), p_numeric, width = 10, height = 6)
}

categorical_vars <- data_clean %>%
  select(where(~ is.character(.x) || is.factor(.x) || is.logical(.x))) %>%
  names() %>%
  setdiff(target_col)

if (length(categorical_vars) > 0) {
  categorical_long <- data_clean %>%
    mutate(across(all_of(categorical_vars), ~ ifelse(is.na(.x), "missing", as.character(.x)))) %>%
    select(all_of(c(target_col, categorical_vars))) %>%
    pivot_longer(cols = all_of(categorical_vars), names_to = "variable", values_to = "category") %>%
    group_by(variable) %>%
    mutate(category = ifelse(category %in% names(sort(table(category), decreasing = TRUE))[1:10], category, "other")) %>%
    ungroup() %>%
    count(variable, category, claim_status) %>%
    group_by(variable, category) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  write_csv(categorical_long, here("reports", "eda_categorical_by_target.csv"))

  p_categorical <- ggplot(categorical_long, aes(x = category, y = share, fill = claim_status)) +
    geom_col(position = "fill") +
    facet_wrap(~ variable, scales = "free_x") +
    scale_fill_manual(values = c(no = "#2b6cb0", yes = "#c53030")) +
    labs(title = "Variables categorielles selon la survenance de sinistre", x = "Modalite", y = "Part", fill = "Sinistre") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")

  ggsave(here("reports", "figures", "categorical_by_target.png"), p_categorical, width = 12, height = 8)
}

if (length(numeric_vars) >= 2) {
  corr_matrix <- data_clean %>% select(all_of(numeric_vars)) %>% cor(use = "pairwise.complete.obs")
  corr_long <- as.data.frame(as.table(corr_matrix)) %>%
    as_tibble() %>%
    rename(variable_1 = Var1, variable_2 = Var2, correlation = Freq)

  write_csv(corr_long, here("reports", "correlation_matrix.csv"))

  p_corr <- ggplot(corr_long, aes(variable_1, variable_2, fill = correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Matrice de correlation des variables numeriques", x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(here("reports", "figures", "correlation_matrix.png"), p_corr, width = 8, height = 7)
}

message("EDA terminee. Resultats sauvegardes dans reports/ et reports/figures/.")
