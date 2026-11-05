# Purpose: run the definitive analysis in dependency order.
# Run from the project root in a fresh session.
#
# Needs: the paper PDFs and the reviewer spreadsheet in data-raw/, and the
# saved model runs listed in model_runs (R/setup.R) in Results/. None of these
# are in the repository.
# Produces: the tables in tables/ and figures in figures/ that the website reads.
#
# The analysis never calls a model. The extraction scripts run only when
# run_extraction is TRUE in R/setup.R (paid API calls); otherwise the saved
# runs are read, and the analysis stops with a message if one is missing.

source("R/setup.R")
source("R/functions.R")

if (isTRUE(run_extraction)) {
  source("R/extract_gpt.R")
  source("R/extract_claude.R")
  source("R/extract_gemini.R")
} else {
  message("Using the saved model runs in model_runs; no model is called.")
}

source("R/data_cleaning.R")
source("R/manual_data.R")
source("R/llm_data_full.R")
source("R/llm_data_transplant.R")
