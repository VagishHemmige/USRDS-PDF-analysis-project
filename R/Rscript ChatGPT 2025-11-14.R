# ---- Libraries ----
library(ellmer)
library(tidyverse)
library(pdftools)
library(tibble)
library(purrr)
library(dplyr)
library(rstudioapi)


# ---- Set file folder ----
results_dir <- "Results/ChatGPT"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Optional Reset Prompt ----
if (interactive() && rstudioapi::isAvailable()) {
  reset_confirm <- rstudioapi::showQuestion(
    title = "Reset Checkpoint Files",
    message = "Do you want to reset all previously saved progress (job queue, partial results, etc.)?",
    ok = "Reset Everything",
    cancel = "No, keep my files"
  )
  
  if (reset_confirm) {
    files_to_delete <- file.path(results_dir, c(
      "job_status_ChatGPT.csv",
      "partial_results_ChatGPT.rds",
      "final_results_ChatGPT.csv",
      "final_results_ChatGPT.rds"
    ))
    deleted <- file.remove(files_to_delete[file.exists(files_to_delete)])
    message("🧹 Reset complete. Deleted ", sum(deleted), " files.")
  } else {
    message("🔁 Continuing with existing progress files.")
  }
}

# ---- Optional Retry Failed Prompt ----
job_status_file <- file.path(results_dir, "job_status_ChatGPT.csv")
if (file.exists(job_status_file) && interactive() && rstudioapi::isAvailable()) {
  retry_failed <- rstudioapi::showQuestion(
    title = "Retry Previously Failed Jobs?",
    message = "Some jobs have failed after 5 attempts. Do you want to retry them again?",
    ok = "🔁 Yes, retry failed",
    cancel = "🚫 No, leave as failed"
  )
  
  if (isTRUE(retry_failed)) {
    job_status <- read.csv(job_status_file, stringsAsFactors = FALSE)
    num_failed <- sum(job_status$status == "failed")
    job_status$status[job_status$status == "failed"] <- "retry"
    write.csv(job_status, job_status_file, row.names = FALSE)
    message("🔁 Reset ", num_failed, " failed jobs to 'retry'")
  } else {
    message("🚫 Failed jobs will remain skipped.")
  }
}

# ---- Setup ----
folder_path <- "data-raw/PDFs"
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)
files_df <- tibble(full_path = pdf_files)

`%||%` <- function(x, y) if (is.null(x)) y else x
estimate_tokens <- function(text) ceiling(nchar(text) / 4)


#Pull schema from R script
source("R/abstract_schema.R")

chatgpt1 <- chat_openai(
  model = "gpt-5-mini",
  system_prompt = id_prompt("")
  )

# ---- Job Queue ----
if (!file.exists(job_status_file)) {
  job_status <- tibble(
    full_path = files_df$full_path,
    filename = basename(files_df$full_path),
    status = "pending",
    retries = 0,
    last_attempt = as.POSIXct(NA)
  )
  write.csv(job_status, job_status_file, row.names = FALSE)
} else {
  job_status <- read.csv(job_status_file, stringsAsFactors = FALSE)
  job_status$last_attempt <- as.POSIXct(job_status$last_attempt)
}

# ---- Resume ----
partial_rds <- file.path(results_dir, "partial_results_ChatGPT.rds")
results_list <- if (file.exists(partial_rds)) {
  message("🔁 Resuming from saved results...")
  readRDS(partial_rds) %>% split(seq_len(nrow(.)))
} else {
  list()
}

# ---- Global Counters ----
total_input_tokens <<- 0
total_output_tokens <<- 0
total_requests <<- 0
total_retries <<- 0

# ---- Main Extractor ----
extract_summary_row <- function(pdf_path,
                                initial_max_chars = 240000,
                                min_chars = 5000,
                                initial_wait = 300,
                                max_wait = 1200,
                                max_retries = 5) {
  if (!file.exists(pdf_path)) {
    message("Skipping missing file: ", pdf_path)
    return(tibble(filename = basename(pdf_path)))
  }
  
  text_pages <- pdftools::pdf_text(pdf_path)
  full_text <- paste(text_pages, collapse = "\n")
  full_char_length <- nchar(full_text)
  max_chars <- min(full_char_length, initial_max_chars)
  wait_time <- initial_wait
  retries <- 0
  
  while (max_chars >= min_chars && retries <= max_retries) {
    input_text <- substr(full_text, 1, max_chars)
    input_token_est <- estimate_tokens(input_text)
    message(Sys.time(), " 🔄 Attempting ", basename(pdf_path), " with ~", input_token_est, " tokens")
    
    result <- tryCatch({
      chatgpt1 <- chat_openai(
        model = "gpt-5-mini",
        system_prompt = id_prompt("")
      )
      chatgpt1$chat_structured(input_text, type = type_summary)
    }, error = function(e) {
      if (grepl("tokens per min|quota|429", e$message)) {
        message("⚠️ Rate limit or quota error. Waiting ", wait_time, "s")
        total_retries <<- total_retries + 1
        Sys.sleep(wait_time)
        return("RATE_LIMIT_RETRY")
      } else {
        message("❌ Error: ", e$message)
        return(structure("ERROR", class = "error"))
      }
    })
    
    if (identical(result, "RATE_LIMIT_RETRY")) {
      retries <- retries + 1
      wait_time <- min(wait_time * 1.5, max_wait)
      max_chars <- floor(max_chars * 0.9)
      next
    }
    
    if (inherits(result, "error") || is.null(result)) {
      chatgpt1 <- chat_openai(
        model = "gpt-5-mini",
        system_prompt = id_prompt("")
      )
      fallback <- chatgpt1$chat(input_text)
      output_token_est <- estimate_tokens(fallback)
      total_output_tokens <<- total_output_tokens + output_token_est
      message("⚠️ Fallback preview:\n", substr(fallback, 1, 500))
      break
    }
    
    output_token_est <- estimate_tokens(paste(result$title %||% "", result$journal %||% "", result$authors %||% ""))
    total_input_tokens <<- total_input_tokens + input_token_est
    total_output_tokens <<- total_output_tokens + output_token_est
    total_requests <<- total_requests + 1
    
    message("✅ Success: ", basename(pdf_path))
    return(as_tibble_row(
      c(
        list(filename = basename(pdf_path), text = input_text, char_length = nchar(input_text), raw_json = list(result)),
        map(result, ~ if (is.null(.x)) NA_character_ else paste(as.character(unlist(.x)), collapse = "; "))
      )
    ))
  }
  
  message("❌ Final failure for ", basename(pdf_path), " after ", retries, " retries.")
  tibble(filename = basename(pdf_path), text = substr(full_text, 1, min_chars), char_length = min_chars)
}

# ---- Main Loop ----
message("\n===== Starting ChatGPT PDF summarization =====")
message("Pending: ", sum(job_status$status == 'pending'))
message("Retry: ", sum(job_status$status == 'retry'))
message("Done: ", sum(job_status$status == 'done'))
message("Failed: ", sum(job_status$status == 'failed'))

for (i in seq_len(nrow(job_status))) {
  row <- job_status[i, ]
  message("\n📄 Processing file ", i, "/", nrow(job_status), ": ", row$filename)
  
  if (row$status == "done") {
    message("⏭️ Skipping (done): ", row$filename)
    next
  }
  if (row$status == "failed") {
    message("⚠️ Skipping (failed): ", row$filename)
    next
  }
  
  result <- extract_summary_row(row$full_path)
  
  if (!"authors" %in% names(result) || is.na(result$authors[1])) {
    job_status$retries[i] <- job_status$retries[i] + 1
    job_status$status[i] <- ifelse(job_status$retries[i] >= 5, "failed", "retry")
  } else {
    job_status$status[i] <- "done"
    results_list[[length(results_list) + 1]] <- result
  }
  
  job_status$last_attempt[i] <- Sys.time()
  message("💾 Saving checkpoint...")
  write.csv(job_status, job_status_file, row.names = FALSE)
  saveRDS(bind_rows(results_list), partial_rds)
  Sys.sleep(60)
}

# ---- Final Save ----
results_df <- bind_rows(results_list)

# Flatten list-columns for CSV export
results_df_flat <- results_df %>%
  mutate(across(where(is.list), ~ sapply(.x, function(val) {
    if (is.null(val) || all(is.na(val))) return(NA_character_)
    if (length(val) == 0) return(NA_character_)
    paste(as.character(val), collapse = "; ")
  })))

# Save final outputs
write.csv(results_df_flat, file.path(results_dir, "final_results_ChatGPT.csv"))
saveRDS(results_df, file.path(results_dir, "final_results_ChatGPT.rds"))
write.csv(job_status, job_status_file)

# ---- Summary ----
cat("\n===== ✅ ChatGPT Job Summary =====\n")
cat("🧾 Total requests:      ", total_requests, "\n")
cat("🔡 Total input tokens:  ", total_input_tokens, "\n")
cat("📤 Total output tokens: ", total_output_tokens, "\n")
cat("🔁 Total retries:       ", total_retries, "\n")
cat("📂 Output written to:   final_results_ChatGPT.csv / .rds\n")
