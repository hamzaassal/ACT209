# 12_final_audit.R
# Audit executable des dernieres evolutions du projet :
# - normalisation des sorties SMOTE multi-niveaux ;
# - controle des sorties CatBoost et tuning ;
# - creation d'une synthese finale de selection du modele.

required_packages <- c("dplyr", "readr", "tidyr", "stringr", "here", "purrr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    ". Installe-les avec install.packages(...) puis relance ce script."
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(here)
library(purrr)

set.seed(2026)

dir.create(here("reports"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("reports", "figures"), showWarnings = FALSE, recursive = TRUE)

read_csv_safe <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

write_csv_safe <- function(data, path) {
  out <- tryCatch(
    {
      write_csv(data, path)
      path
    },
    error = function(e) {
      fallback <- sub("\\.csv$", "_standardized.csv", path)
      write_csv(data, fallback)
      message("Ecriture impossible dans ", path, ". Copie de secours : ", fallback)
      fallback
    }
  )
  invisible(out)
}

rename_if_present <- function(data, old, new) {
  if (!is.null(data) && old %in% names(data) && !(new %in% names(data))) {
    data <- rename(data, !!new := all_of(old))
  }
  data
}

# -------------------------------------------------------------------------
# 1. Normalisation du fichier d'equilibre de classes SMOTE.
# -------------------------------------------------------------------------

balance_path <- here("reports", "smote_intensity_class_balance.csv")
balance <- read_csv_safe(balance_path)

if (!is.null(balance)) {
  balance <- balance %>%
    rename_if_present("balancing_strategy", "strategy") %>%
    rename_if_present("n_no", "n_no_train") %>%
    rename_if_present("n_yes", "n_yes_train") %>%
    rename_if_present("prop_no", "prop_no_train") %>%
    rename_if_present("prop_yes", "prop_yes_train") %>%
    rename_if_present("total_observations", "total_train")

  if (!"test_prop_yes" %in% names(balance)) {
    balance$test_prop_yes <- NA_real_
  }
  if (!"test_prop_no" %in% names(balance)) {
    balance$test_prop_no <- 1 - balance$test_prop_yes
  }

  balance <- balance %>%
    mutate(test_unchanged = TRUE) %>%
    select(
      strategy, over_ratio,
      n_no_train, n_yes_train, prop_no_train, prop_yes_train, total_train,
      test_n_no, test_n_yes, test_prop_no, test_prop_yes, test_unchanged
    )

  write_csv_safe(balance, balance_path)
}

# -------------------------------------------------------------------------
# 2. Normalisation du fichier de comparaison des intensites SMOTE.
# -------------------------------------------------------------------------

smote_metrics_path <- here("reports", "model_comparison_smote_intensity.csv")
smote_metrics <- read_csv_safe(smote_metrics_path)

if (!is.null(smote_metrics)) {
  smote_metrics <- smote_metrics %>%
    rename_if_present("balancing_strategy", "strategy") %>%
    rename_if_present("f1_score", "f1")

  if (!"over_ratio" %in% names(smote_metrics)) {
    smote_metrics$over_ratio <- NA_real_
  }

  smote_metrics <- smote_metrics %>%
    mutate(
      over_ratio = case_when(
        !is.na(.data$over_ratio) ~ .data$over_ratio,
        strategy == "smote_10" ~ 0.111,
        strategy == "smote_20" ~ 0.25,
        strategy == "smote" ~ 0.30,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      model, strategy, over_ratio, threshold,
      accuracy, precision, recall, f1, roc_auc, pr_auc,
      predicted_positive, false_positives, false_negatives,
      true_positives, true_negatives,
      everything()
    )

  write_csv_safe(smote_metrics, smote_metrics_path)
}

# -------------------------------------------------------------------------
# 3. Normalisation du fichier de lift XGBoost par intensite SMOTE.
# -------------------------------------------------------------------------

xgb_lift_path <- here("reports", "xgboost_lift_by_smote_intensity.csv")
xgb_lift <- read_csv_safe(xgb_lift_path)

if (!is.null(xgb_lift)) {
  xgb_lift <- xgb_lift %>%
    rename_if_present("balancing_strategy", "strategy") %>%
    rename_if_present("segment", "top_segment") %>%
    rename_if_present("share_of_total_claims_captured", "share_claims_captured") %>%
    rename_if_present("claim_rate_in_segment", "claim_rate_segment") %>%
    select(
      strategy, top_segment, n_observations, n_claims_captured,
      share_claims_captured, claim_rate_segment, baseline_claim_rate, lift,
      everything()
    )

  write_csv_safe(xgb_lift, xgb_lift_path)
}

# -------------------------------------------------------------------------
# 4. Controle CatBoost.
# -------------------------------------------------------------------------

catboost_available <- requireNamespace("catboost", quietly = TRUE)
catboost_outputs <- c(
  here("scripts", "10_catboost_training.R"),
  here("reports", "catboost_test_metrics.csv"),
  here("reports", "catboost_lift_analysis.csv"),
  here("reports", "catboost_hyperparameter_results.csv"),
  here("reports", "xgboost_vs_catboost_comparison.csv"),
  here("models", "catboost_model.cbm"),
  here("models", "catboost_metadata.rds")
)

catboost_audit <- tibble(
  item = basename(catboost_outputs),
  path = catboost_outputs,
  exists = file.exists(catboost_outputs)
) %>%
  mutate(
    catboost_package_available = catboost_available,
    catboost_version = if (catboost_available) {
      as.character(utils::packageVersion("catboost"))
    } else {
      NA_character_
    }
  )

write_csv_safe(catboost_audit, here("reports", "catboost_audit_summary.csv"))

if (!catboost_available) {
  issue <- c(
    "# Diagnostic CatBoost",
    "",
    "Le package `catboost` n'est pas disponible dans l'environnement R actuel.",
    "",
    "Commandes recommandees :",
    "",
    "```r",
    "install.packages('remotes', repos = 'https://cloud.r-project.org')",
    "remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.10/catboost-R-windows-x86_64-1.2.10.tgz', INSTALL_opts = c('--no-multiarch', '--no-test-load'))",
    "```",
    "",
    "Consequences : CatBoost ne peut pas etre integre au benchmark tant que ce package n'est pas chargeable."
  )
  writeLines(issue, here("reports", "catboost_installation_issue.md"), useBytes = TRUE)
}

# -------------------------------------------------------------------------
# 5. Synthese finale de selection du modele.
# -------------------------------------------------------------------------

xgb_cat <- read_csv_safe(here("reports", "xgboost_vs_catboost_comparison.csv"))
champion_vs_tuned <- read_csv_safe(here("reports", "champion_vs_tuned_comparison.csv"))
tuned_lift <- read_csv_safe(here("reports", "tuned_model_lift_analysis.csv"))

candidate_rows <- list()

if (!is.null(xgb_cat)) {
  candidate_rows$xgb_cat <- xgb_cat %>%
    transmute(
      candidate_model = model,
      strategy = strategy,
      tuned_or_not = if_else(model == "catboost", TRUE, FALSE),
      roc_auc, pr_auc, f1, recall, precision,
      lift_top_10, claims_captured_top_10,
      lift_top_20, claims_captured_top_20
    )
}

if (!is.null(champion_vs_tuned)) {
  tuned_candidates <- champion_vs_tuned %>%
    filter(comparison_role == "best_tuned") %>%
    transmute(
      candidate_model = model,
      strategy = model_variant,
      tuned_or_not = TRUE,
      roc_auc, pr_auc,
      f1 = f1_score,
      recall, precision,
      lift_top_10, claims_captured_top_10,
      lift_top_20 = NA_real_,
      claims_captured_top_20
    )

  if (!is.null(tuned_lift) && nrow(tuned_candidates) > 0) {
    tuned_lift_20 <- tuned_lift %>%
      filter(segment == "top_20_percent") %>%
      transmute(strategy = model_variant, lift_top_20 = lift)

    tuned_candidates <- tuned_candidates %>%
      select(-lift_top_20) %>%
      left_join(tuned_lift_20, by = "strategy")
  }

  candidate_rows$tuned <- tuned_candidates
}

if (!is.null(smote_metrics) && !is.null(xgb_lift)) {
  smote_xgb <- smote_metrics %>%
    filter(model == "xgboost", strategy %in% c("smote_10", "smote_20")) %>%
    select(model, strategy, roc_auc, pr_auc, f1, recall, precision)

  smote_lift_wide <- xgb_lift %>%
    filter(strategy %in% c("smote_10", "smote_20"), top_segment %in% c("top_10_percent", "top_20_percent")) %>%
    select(strategy, top_segment, lift, share_claims_captured) %>%
    pivot_wider(
      names_from = top_segment,
      values_from = c(lift, share_claims_captured)
    )

  candidate_rows$smote_intensity <- smote_xgb %>%
    left_join(smote_lift_wide, by = "strategy") %>%
    transmute(
      candidate_model = model,
      strategy,
      tuned_or_not = FALSE,
      roc_auc, pr_auc, f1, recall, precision,
      lift_top_10 = lift_top_10_percent,
      claims_captured_top_10 = share_claims_captured_top_10_percent,
      lift_top_20 = lift_top_20_percent,
      claims_captured_top_20 = share_claims_captured_top_20_percent
    )
}

final_selection <- bind_rows(candidate_rows) %>%
  distinct(candidate_model, strategy, tuned_or_not, .keep_all = TRUE) %>%
  arrange(desc(roc_auc), desc(pr_auc), desc(f1), desc(lift_top_10)) %>%
  mutate(
    final_rank = row_number(),
    recommendation = if_else(
      final_rank == 1,
      "Modele final recommande pour Shiny et agent IA : meilleur compromis ROC AUC, PR AUC et lift.",
      "Candidat compare, non retenu comme modele final."
    )
  ) %>%
  select(
    candidate_model, strategy, tuned_or_not,
    roc_auc, pr_auc, f1, recall, precision,
    lift_top_10, claims_captured_top_10,
    lift_top_20, claims_captured_top_20,
    final_rank, recommendation
  )

write_csv_safe(final_selection, here("reports", "final_model_selection_summary.csv"))

# -------------------------------------------------------------------------
# 6. Rapport d'audit lisible.
# -------------------------------------------------------------------------

smote_best_roc <- smote_metrics %>%
  filter(model == "xgboost", !is.na(roc_auc)) %>%
  arrange(desc(roc_auc)) %>%
  slice(1)

smote_best_recall <- smote_metrics %>%
  filter(model == "xgboost", !is.na(recall)) %>%
  arrange(desc(recall)) %>%
  slice(1)

smote_best_lift <- xgb_lift %>%
  filter(top_segment == "top_10_percent") %>%
  arrange(desc(lift)) %>%
  slice(1)

audit_md <- c(
  "# Audit final des dernieres evolutions",
  "",
  paste0("- CatBoost disponible : ", catboost_available),
  paste0("- Version CatBoost : ", if (catboost_available) as.character(utils::packageVersion("catboost")) else "indisponible"),
  paste0("- Fichier de selection finale cree : ", file.exists(here("reports", "final_model_selection_summary.csv"))),
  "",
  "## SMOTE",
  "",
  paste0("- Meilleure strategie XGBoost selon ROC AUC : ", smote_best_roc$strategy),
  paste0("- Meilleure strategie XGBoost selon recall : ", smote_best_recall$strategy),
  paste0("- Meilleure strategie XGBoost selon lift Top 10 % : ", smote_best_lift$strategy),
  "",
  "## Modele final",
  "",
  paste0(
    "- Candidat recommande : ",
    final_selection$candidate_model[1], " / ", final_selection$strategy[1],
    " (rang ", final_selection$final_rank[1], ")"
  )
)

writeLines(audit_md, here("reports", "latest_evolution_audit.md"), useBytes = TRUE)

print(final_selection)
