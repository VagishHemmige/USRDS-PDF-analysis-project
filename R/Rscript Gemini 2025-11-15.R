# ---- Libraries ----
library(ellmer)
library(tidyverse)
library(pdftools)
library(rstudioapi)
library(purrr)

# Define folder for Gemini output files
results_dir <- "Results/Gemini"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Optional Reset Prompt ----
if (interactive() && rstudioapi::isAvailable()) {
  reset_confirm <- rstudioapi::showQuestion(
    title = "Reset Gemini Files",
    message = "Reset all Gemini checkpoint and log files?",
    ok = "Yes, reset",
    cancel = "No, keep"
  )
  if (reset_confirm) {
    files_to_delete <- file.path(results_dir, c(
      "processed_files_gemini.txt",
      "failed_files_gemini.txt",
      "summary_partial_gemini.rds",
      "summary_partial_gemini.csv",
      "summary_final_gemini.rds",
      "summary_output_gemini.csv",
      "type_summary_gemini_schema.rds",
      "job_status_Gemini.csv"
    ))
    deleted <- file.remove(files_to_delete[file.exists(files_to_delete)])
    message("🧹 Deleted ", sum(deleted), " files.")
  }
}

# ---- Setup ----
folder_path <- "data-raw/PDFs"
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

estimate_tokens <- function(text) ceiling(nchar(text) / 4)

#Pull schema from R script
source("R/abstract_schema.R")

# ---- Job Queue Setup ----
job_file <- file.path(results_dir, "job_status_Gemini.csv")
if (!file.exists(job_file)) {
  job_status <- tibble(
    full_path = pdf_files,
    filename = basename(pdf_files),
    status = "pending",
    retries = 0,
    last_attempt = as.POSIXct(NA)
  )
  write_csv(job_status, job_file)
} else {
  job_status <- read_csv(job_file, show_col_types = FALSE)
}

# ---- Resume Results ----
checkpoint_file <- file.path(results_dir, "summary_partial_gemini.rds")
if (file.exists(checkpoint_file)) {
  results_list <- readRDS(checkpoint_file)
} else {
  results_list <- list()
}

# ---- Sync Job Status with Saved Results ----
done_filenames <- names(results_list)
job_status <- job_status %>%
  mutate(
    status = ifelse(filename %in% done_filenames, "done", status)
  )

# ---- Global Counters ----
total_input_tokens <- 0
total_output_tokens <- 0
total_requests <- 0
total_retries <- 0

# ---- Summarizer Function ----
extract_summary_single <- function(pdf_path, max_pages = 50, max_chars = 240000, max_retries = 6, global_pause = 15 * 60) {
  if (!file.exists(pdf_path)) {
    message("❌ File not found: ", pdf_path)
    return(NULL)
  }
  
  text_pages <- pdftools::pdf_text(pdf_path)
  truncated_text <- paste(text_pages[1:min(length(text_pages), max_pages)], collapse = "\n")
  truncated_text <- substr(truncated_text, 1, max_chars)
  estimated_tokens <- estimate_tokens(truncated_text)
  
  retries <- 0
  backoff <- c(60, 120, 180, 240, 300, 600)
  
  repeat {
    cat("📏 Estimated tokens:", estimated_tokens, "from", nchar(truncated_text), "chars\n")
    result <- tryCatch({
      print(Sys.time())
      gemini1 <- chat_google_gemini(
        model = "gemini-2.5-flash",
        system_prompt = id_prompt("")
      )

      gemini1$chat_structured(truncated_text, type = type_summary)
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("429|quota|rate limit", msg)) {
        wait_time <- if (retries < length(backoff)) backoff[retries + 1] else 900
        wait_time <- runif(1, wait_time * 0.9, wait_time * 1.1)
        cat("⏳ Rate limit hit. Waiting", round(wait_time), "s (attempt", retries + 1, ")\n")
        Sys.sleep(wait_time)
        retries <<- retries + 1
        total_retries <<- total_retries + 1
        return("RETRY")
      } else {
        cat("❌ Unexpected error:", msg, "\n")
        return(NULL)
      }
    })
    
    if (identical(result, "RETRY")) {
      if (retries >= max_retries) {
        cat("❌ Max retries reached\n")
        return(NULL)
      }
    } else {
      break
    }
  }
  
  if (is.null(result)) {
    fallback <- gemini1$chat(truncated_text)
    cat("⚠️ Fallback preview:\n", substr(fallback, 1, 500), "\n")
    return(NULL)
  }
  
  result_list <- result
  result_list$filename <- basename(pdf_path)
  
  result_clean <- result_list %>% purrr::map_chr(~ {
    if (is.null(.x) || length(.x) == 0) return(NA_character_)
    if (is.atomic(.x) && length(.x) == 1) return(as.character(.x))
    if (is.atomic(.x) && length(.x) > 1) return(paste(.x, collapse = ", "))
    return(as.character(.x))
  })
  
  output_tokens <- estimate_tokens(paste(result_clean, collapse = " "))
  total_input_tokens <<- total_input_tokens + estimated_tokens
  total_output_tokens <<- total_output_tokens + output_tokens
  total_requests <<- total_requests + 1
  
  list(data = tibble::as_tibble_row(result_clean), token_count = estimated_tokens)
}

# ---- Main Loop ----
message("\n===== Starting Gemini PDF summarization =====")
message("Pending: ", sum(job_status$status == 'pending'))
message("Retry: ", sum(job_status$status == 'retry'))
message("Done: ", sum(job_status$status == 'done'))
message("Failed: ", sum(job_status$status == 'failed'))

for (i in seq_len(nrow(job_status))) {
  row <- job_status[i, ]
  cat("\n[", i, "/", nrow(job_status), "]", row$filename, "\n")
  
  if (row$status %in% c("done", "failed")) {
    cat("⏭️ Skipping (status:", row$status, ")\n")
    next
  }
  
  result <- extract_summary_single(row$full_path)
  
  if (!is.null(result)) {
    results_list[[row$filename]] <- result$data
    job_status$status[i] <- "done"
    wait_time <- ceiling(result$token_count / 1000) * 10
    wait_time <- runif(1, wait_time * 0.9, wait_time * 1.1)
    cat("🕒 Waiting", round(wait_time), "s...\n")
    Sys.sleep(wait_time)
  } else {
    job_status$retries[i] <- job_status$retries[i] + 1
    job_status$status[i] <- ifelse(job_status$retries[i] >= 5, "failed", "retry")
  }
  
  job_status$last_attempt[i] <- Sys.time()
  write_csv(job_status, job_file)
  saveRDS(results_list, checkpoint_file)
}

# ---- Final Save ----
final_df <- bind_rows(results_list)
write_csv(final_df, file.path(results_dir, "summary_output_gemini.csv"))

message("\n===== Summary =====")
message("Total requests: ", total_requests)
message("Input tokens: ", total_input_tokens)
message("Output tokens: ", total_output_tokens)
message("Retries: ", total_retries)
