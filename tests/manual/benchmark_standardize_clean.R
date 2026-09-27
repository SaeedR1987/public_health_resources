#!/usr/bin/env Rscript

#' Manual Performance Benchmark: Data$standardize() / Data$clean()
#'
#' @description
#' Standalone, dependency-light benchmark for the `Data` class
#' standardize/clean workflow (`validate()`, `standardize()`, `clean()`,
#' including cleaning-log application). It is intended to be run manually,
#' BEFORE and AFTER any performance-related code changes, so the two result
#' CSVs can be diffed to confirm an improvement (or catch a regression).
#'
#' Usage:
#'   Rscript tests/manual/benchmark_standardize_clean.R
#'   # or from an interactive session:
#'   source("tests/manual/benchmark_standardize_clean.R")
#'
#' Output:
#'   tests/manual/output/benchmark_standardize_clean_<timestamp>.csv
#'   A "latest" copy is also written to:
#'   tests/manual/output/benchmark_standardize_clean_latest.csv
#'
#' To compare a "before" and "after" run:
#'   1. Run this script on the baseline code, rename the *_latest.csv to
#'      *_before.csv (or just keep the timestamped file).
#'   2. Make your performance changes.
#'   3. Run this script again and compare the two CSVs (e.g. with
#'      `diff`, or by reading both into R and computing % change per stage).

rm(list = ls())

devtools::load_all()
source("dev/generate_household_samples.R")

# CONFIGURATION ####

# Dataset sizes to benchmark. Increase the largest value(s) to make any
# super-linear (e.g. O(n^2)) behavior more visible.
sample_sizes <- c(200, 1000, 4000, 8000)

# Fraction of rows to include in the synthetic cleaning log, to exercise
# Data$clean() -> private$..apply_cleaning_changes().
cleaning_log_fraction <- 0.10

# Number of timing repetitions per stage/size (median is reported).
n_reps <- 3

# WHERE TO WRITE RESULTS ####

output_dir <- "tests/manual/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(
  output_dir,
  paste0("benchmark_standardize_clean_", timestamp, ".csv")
)
latest_file <- file.path(
  output_dir,
  "benchmark_standardize_clean_latest.csv"
)

# HELPERS ####

#' Time an expression, returning elapsed seconds (wall clock).
time_it <- function(expr) {
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

#' Run `expr` `n_reps` times and return the median elapsed time in seconds.
median_time <- function(expr_fn, n_reps) {
  times <- vapply(seq_len(n_reps), function(i) time_it(expr_fn()), numeric(1))
  stats::median(times)
}

#' Build a synthetic cleaning log touching `fraction` of rows across a
#' handful of columns, to exercise Data$clean()'s cleaning-log application.
make_cleaning_log_df <- function(df, uuid_col, fraction) {
  candidate_cols <- setdiff(names(df), uuid_col)
  # Prefer character/numeric columns for simple, safely-coercible edits
  candidate_cols <- candidate_cols[vapply(
    candidate_cols,
    function(cn) is.character(df[[cn]]) || is.numeric(df[[cn]]),
    logical(1)
  )]
  if (length(candidate_cols) == 0) {
    return(NULL)
  }

  n <- nrow(df)
  n_edits <- max(1L, floor(n * fraction))
  edit_rows <- sample.int(n, n_edits, replace = FALSE)
  edit_cols <- sample(candidate_cols, n_edits, replace = TRUE)

  uuids <- as.character(df[[uuid_col]][edit_rows])
  old_vals <- mapply(
    function(r, cn) as.character(df[[cn]][r]),
    edit_rows,
    edit_cols
  )
  new_vals <- mapply(
    function(cn, ov) {
      if (is.numeric(df[[cn]])) {
        as.character(suppressWarnings(as.numeric(ov)) + 1)
      } else {
        paste0(ov, "_edited")
      }
    },
    edit_cols,
    old_vals
  )

  data.frame(
    uuid = uuids,
    enum_id = "benchmark_enum",
    device_id = "benchmark_device",
    question.name = edit_cols,
    issue = "benchmark synthetic edit",
    feedback = "benchmark synthetic edit",
    changed = "yes",
    old.value = old_vals,
    new.value = new_vals,
    stringsAsFactors = FALSE
  )
}

# BENCHMARK LOOP ####

results <- list()

for (n in sample_sizes) {
  message(sprintf("\n=== Benchmarking n = %d rows ===", n))

  hh_df <- generate_household_dataset(n = n)

  # --- validate() ---
  hh <- HouseholdData$new(
    data = hh_df,
    dataset_name = paste0("BenchmarkHH_", n)
  )
  t_validate <- median_time(function() hh$validate(), n_reps)

  # --- standardize() ---
  # standardize() is not idempotent-safe to re-run identically many times on
  # the exact same object in all subclasses, so rebuild fresh objects for
  # each repetition to keep the timing honest.
  t_standardize <- median_time(
    function() {
      hh2 <- HouseholdData$new(
        data = hh_df,
        dataset_name = paste0("BenchmarkHH_", n)
      )
      hh2$validate()
      hh2$standardize()
    },
    n_reps
  )

  # --- clean() (including cleaning log application) ---
  uuid_col <- tryCatch(hh$get(field = "..uuid"), error = function(e) "uuid")
  if (is.null(uuid_col) || !uuid_col %in% names(hh_df)) {
    uuid_col <- "uuid"
  }

  t_clean <- median_time(
    function() {
      hh3 <- HouseholdData$new(
        data = hh_df,
        dataset_name = paste0("BenchmarkHH_", n)
      )
      hh3$validate()
      hh3$standardize()

      std_df <- hh3$get_data(stage = "standardized")
      log_df <- make_cleaning_log_df(std_df, uuid_col, cleaning_log_fraction)
      if (!is.null(log_df)) {
        hh3$set(
          field = "cleaning_log",
          value = CleaningLog$new(log_df = log_df)
        )
      }

      hh3$clean()
    },
    n_reps
  )

  results[[length(results) + 1L]] <- data.frame(
    n_rows = n,
    n_cleaning_log_rows = max(1L, floor(n * cleaning_log_fraction)),
    n_reps = n_reps,
    validate_sec = t_validate,
    standardize_sec = t_standardize,
    clean_sec = t_clean,
    total_sec = t_validate + t_standardize + t_clean,
    stringsAsFactors = FALSE
  )

  message(sprintf(
    "validate: %.4fs | standardize: %.4fs | clean: %.4fs",
    t_validate,
    t_standardize,
    t_clean
  ))
}

results_df <- do.call(rbind, results)

# Rows-per-second per stage: useful to spot super-linear (O(n^2)+) scaling --
# a healthy vectorized implementation should keep this roughly flat/increasing
# as n grows, while an O(n^2) loop will show it dropping sharply.
results_df$validate_rows_per_sec <- results_df$n_rows / results_df$validate_sec
results_df$standardize_rows_per_sec <- results_df$n_rows /
  results_df$standardize_sec
results_df$clean_rows_per_sec <- results_df$n_rows / results_df$clean_sec

cat("\n\n==== BENCHMARK SUMMARY ====\n")
print(results_df, row.names = FALSE)

write.csv(results_df, output_file, row.names = FALSE)
write.csv(results_df, latest_file, row.names = FALSE)

cat(sprintf("\nResults written to:\n  %s\n  %s\n", output_file, latest_file))
cat(
  "\nTo compare before/after a code change, save this file under a new\n",
  "name (e.g. _before.csv / _after.csv) and diff the two, or read both\n",
  "into R with read.csv() and compute percentage differences per stage.\n"
)
