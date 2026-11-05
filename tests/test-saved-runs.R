# Tests that the analysis never calls a model by default, on synthetic files.
# Run from the project root: testthat::test_file("tests/test-saved-runs.R")
# Needs only testthat and tibble; it does not source R/setup.R, which loads the
# full analysis package set.

library(testthat)

# testthat runs from tests/; find the project root from either place.
root <- if (file.exists("R/functions.R")) "." else ".."
at_root <- function(...) file.path(root, ...)

# The saved-run helpers and the model_runs record, read from their scripts.
source(at_root("R/functions.R"))

setup_section <- function(label) {
  lines <- readLines(at_root("R/setup.R"), warn = FALSE)
  start <- grep(paste0("^## ---- ", label, "$"), lines)
  ends <- grep("^## ---- ", lines)
  end <- c(ends[ends > start], length(lines) + 1)[1] - 1
  env <- new.env()
  eval(parse(text = lines[start:end]), envir = env)
  env
}
runs_env <- setup_section("setup-model-runs")

test_that("the analysis does not re-extract by default", {
  expect_false(runs_env$run_extraction)
})

test_that("model_runs has one row per model, all legacy", {
  runs <- runs_env$model_runs
  expect_setequal(runs$model, c("gpt", "claude", "gemini"))
  expect_equal(anyDuplicated(runs$model), 0)
  expect_true(all(runs$status == "legacy"))
})

test_that("model_runs points at the files the extraction scripts write", {
  written <- c(
    gpt = "final_results_ChatGPT.rds",
    claude = "summary_final_claude.rds",
    gemini = "summary_output_gemini.csv"
  )
  scripts <- c(gpt = "R/extract_gpt.R", claude = "R/extract_claude.R",
               gemini = "R/extract_gemini.R")
  for (m in names(written)) {
    path <- saved_run_path(runs_env$model_runs, m)
    expect_equal(basename(path), written[[m]])
    expect_true(any(grepl(written[[m]], readLines(at_root(scripts[[m]])), fixed = TRUE)))
  }
})

test_that("check_saved_runs() passes when every saved run exists", {
  dir <- tempfile("runs")
  dir.create(dir)
  runs <- tibble::tibble(model = c("a", "b"),
                         path = file.path(dir, c("a.rds", "b.csv")))
  file.create(runs$path)
  expect_silent(check_saved_runs(runs))
})

test_that("check_saved_runs() names each missing run and does not call a model", {
  dir <- tempfile("runs")
  dir.create(dir)
  runs <- tibble::tibble(model = c("a", "b"),
                         path = file.path(dir, c("a.rds", "b.csv")))
  file.create(runs$path[1])
  expect_error(check_saved_runs(runs), "b \\(")
  expect_error(check_saved_runs(runs), "does not call models")
})

test_that("saved_run_path() refuses an unknown or duplicated model", {
  runs <- tibble::tibble(model = c("a", "a"), path = c("x", "y"))
  expect_error(saved_run_path(runs, "a"), "found 2")
  expect_error(saved_run_path(runs, "z"), "found 0")
})

test_that("each extraction script stops before doing anything unless run_extraction is TRUE", {
  for (script in c("R/extract_gpt.R", "R/extract_claude.R", "R/extract_gemini.R")) {
    env <- new.env()
    expect_error(sys.source(at_root(script), envir = env), "paid model API")
    env$run_extraction <- FALSE
    expect_error(sys.source(at_root(script), envir = env), "paid model API")
    # Nothing ran before the guard: no results folder or object was created.
    expect_false(exists("results_dir", envir = env, inherits = FALSE))
  }
})
