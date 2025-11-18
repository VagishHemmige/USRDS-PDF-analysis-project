library(ellmer)
library(tidyverse)
library(pdftools)
library(purrr)
library(tibble)
library(rstudioapi)


#Define folder for status files
results_dir <- "Results/Claude"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ----- Optional: Reset all output files via RStudio popup -----
reset <- FALSE

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  response <- rstudioapi::showQuestion(
    title = "Reset Summarization Progress?",
    message = "Do you want to delete all previous progress and start fresh?",
    ok = "✅ Yes, reset",
    cancel = "❌ No, do not reset"
  )
  reset <- isTRUE(response)
}

if (reset) {
  files_to_delete <- file.path(results_dir, c(
    "processed_files_claude.txt",
    "failed_files_claude.txt",
    "summary_partial_claude.rds",
    "summary_partial_claude.csv",
    "summary_final_claude.rds",
    "summary_output_claude.csv",
    "type_summary_claude_schema.rds"
  ))
  deleted <- file.remove(files_to_delete[file.exists(files_to_delete)])
  cat("🧹 Reset complete. Deleted", sum(deleted), "files.\n")
} else {
  cat("⏩ No reset. Continuing with existing progress.\n")
}

# ----- Setup -----

folder_path <- "data-raw/PDFs"
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

source("R/abstract_schema.R")


saveRDS(type_summary, file.path(results_dir, "type_summary_claude_schema.rds"))

# ----- Paths -----
processed_files_path <- file.path(results_dir, "processed_files_claude.txt")
failed_files_path    <- file.path(results_dir, "failed_files_claude.txt")
partial_rds_path     <- file.path(results_dir, "summary_partial_claude.rds")
partial_csv_path     <- file.path(results_dir, "summary_partial_claude.csv")
final_rds_path       <- file.path(results_dir, "summary_final_claude.rds")
final_csv_path       <- file.path(results_dir, "summary_output_claude.csv")


# ----- Load State -----
processed_files <- if (file.exists(processed_files_path)) basename(readLines(processed_files_path)) else character(0)
failed_files    <- if (file.exists(failed_files_path))    basename(readLines(failed_files_path))    else character(0)
results_list    <- if (file.exists(partial_rds_path))     readRDS(partial_rds_path)                 else list()

# ----- Single PDF Extractor -----

extract_summary_single <- function(pdf_path, max_pages = 50, max_chars = 240000) {
  text_pages <- pdftools::pdf_text(pdf_path)
  truncated_text <- paste(text_pages[1:min(length(text_pages), max_pages)], collapse = "\n")
  truncated_text <- substr(truncated_text, 1, max_chars)
  print(Sys.time())
  claude1 <- chat_anthropic(
    model = "claude-sonnet-4-5",
    system_prompt = id_prompt("")
  )
  
  result <- claude1$chat_structured(truncated_text, type = type_summary)
  
  result_safe <- map(result, function(x) {
    if (is.null(x)) return(NA_character_)
    if (length(x) == 1) return(as.character(x))
    return(paste(as.character(unlist(x)), collapse = ", "))
  })
  
  result_safe$filename <- basename(pdf_path)
  return(as_tibble_row(result_safe))
}

# ----- Main Loop -----

for (i in seq_along(pdf_files)) {
  file <- pdf_files[i]
  file_name <- basename(file)
  
  if (file_name %in% processed_files) {
    cat("[", i, "/", length(pdf_files), "] Skipping already processed:", file_name, "\n")
    next
  }
  
  cat("[", i, "/", length(pdf_files), "] Processing:", file_name, "\n")
  
  # Retry logic
  max_attempts <- 5
  wait_times <- c(300, 450, 600, 900, 1200)  # in seconds
  attempt <- 1
  success <- FALSE
  result <- NULL
  
  while (attempt <= max_attempts && !success) {
    cat("🔁 Attempt", attempt, "of", max_attempts, "for", file_name, "\n")
    
    result <- tryCatch({
      extract_summary_single(file)
    }, error = function(e) {
      cat("⚠️ Error in attempt", attempt, ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(result) && is.data.frame(result)) {
      success <- TRUE
    } else if (attempt < max_attempts) {
      wait <- wait_times[attempt]
      cat("⏳ Waiting", wait, "seconds before retrying...\n")
      Sys.sleep(wait)
    }
    
    attempt <- attempt + 1
  }
  
  if (success) {
    results_list[[length(results_list) + 1]] <- result
    processed_files <- unique(c(processed_files, file_name))
    cat("✅ Success after", attempt - 1, "attempt(s):", file_name, "\n")
    
    # Save checkpoint
    all_field_names <- unique(unlist(map(results_list, names)))
    results_list_aligned <- map(results_list, ~{
      missing <- setdiff(all_field_names, names(.x))
      .x[missing] <- NA_character_
      .x[all_field_names]
    })
    results_df <- bind_rows(results_list_aligned)
    
    saveRDS(results_list, partial_rds_path)
    write.csv(results_df, partial_csv_path, row.names = FALSE)
    writeLines(processed_files, processed_files_path)
    writeLines(failed_files, failed_files_path)
  } else {
    failed_files <- unique(c(failed_files, file_name))
    cat("❌ Failed after", max_attempts, "attempts:", file_name, "\n")
    writeLines(failed_files, failed_files_path)
  }
}

# ----- Final Save -----

all_field_names <- unique(unlist(map(results_list, names)))
results_list_aligned <- map(results_list, ~{
  missing <- setdiff(all_field_names, names(.x))
  .x[missing] <- NA_character_
  .x[all_field_names]
})
summaries_df <- bind_rows(results_list_aligned)

write.csv(summaries_df, final_csv_path, row.names = FALSE)
saveRDS(results_list, final_rds_path)
writeLines(processed_files, processed_files_path)
writeLines(failed_files, failed_files_path)
cat("✅ Final save complete. Files written with 'claude' in name.\n")
