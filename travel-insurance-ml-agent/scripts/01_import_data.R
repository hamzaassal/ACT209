# 01_import_data.R
# Import et nettoyage minimal du dataset Kaggle Travel Insurance.
# Objectif du projet : predire la survenance d'un sinistre.
# Cible standardisee : claim_status, avec niveaux no / yes.

required_packages <- c("readr", "dplyr", "janitor", "tidyr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les par exemple avec : install.packages(c('",
    paste(missing_packages, collapse = "','"), "'))"
  )
}

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(here)

set.seed(123)

dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)

csv_files <- list.files(here("data", "raw"), pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

if (length(csv_files) == 0) {
  stop(
    "Aucun fichier CSV trouve dans data/raw/.\n",
    "Le dataset Kaggle doit etre ajoute manuellement ; aucun telechargement automatique n'est effectue."
  )
}

# Si plusieurs CSV existent, on privilegie un nom contenant travel et insurance.
candidate <- csv_files[grepl("travel.*insurance|insurance.*travel", basename(csv_files), ignore.case = TRUE)]
csv_path <- if (length(candidate) > 0) sort(candidate)[1] else sort(csv_files)[1]
message("Fichier CSV utilise : ", basename(csv_path))

data_raw <- read_csv(csv_path, show_col_types = FALSE) %>%
  clean_names()

# Le fichier attendu peut contenir "Claim Status". Le fichier local actuel
# contient "Claim". Dans les deux cas, on standardise le nom en claim_status.
if (!"claim_status" %in% names(data_raw)) {
  if ("claim" %in% names(data_raw)) {
    message("Colonne 'claim' detectee : renommage en 'claim_status'.")
    data_raw <- data_raw %>% rename(claim_status = claim)
  } else {
    stop(
      "Variable cible introuvable. Attendu : claim_status ou claim apres clean_names().\n",
      "Colonnes disponibles : ", paste(names(data_raw), collapse = ", ")
    )
  }
}

target_values <- unique(na.omit(as.character(data_raw$claim_status)))
if (length(target_values) != 2) {
  stop("La cible claim_status doit etre binaire. Modalites detectees : ", paste(target_values, collapse = ", "))
}

missing_summary <- data_raw %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))

duplicate_count <- sum(duplicated(data_raw))

message("Dimensions initiales : ", nrow(data_raw), " lignes x ", ncol(data_raw), " colonnes")
message("Doublons exacts detectes : ", duplicate_count)
print(missing_summary)

data_clean <- data_raw %>%
  distinct() %>%
  mutate(
    claim_status = case_when(
      claim_status %in% c(1, "1", "Yes", "yes", "YES", TRUE) ~ "yes",
      claim_status %in% c(0, "0", "No", "no", "NO", FALSE) ~ "no",
      TRUE ~ tolower(as.character(claim_status))
    )
  )

if (!setequal(unique(data_clean$claim_status), c("no", "yes"))) {
  stop("La cible nettoyee doit contenir exactement les modalites no et yes.")
}

saveRDS(data_clean, here("data", "processed", "data_clean.rds"))
write_csv(missing_summary, here("data", "processed", "missing_summary.csv"))

message("Base nettoyee sauvegardee : data/processed/data_clean.rds")
