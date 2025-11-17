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
folder_path <- "data-raw\PDFs"
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)
files_df <- tibble(full_path = pdf_files)

`%||%` <- function(x, y) if (is.null(x)) y else x
estimate_tokens <- function(text) ceiling(nchar(text) / 4)



id_prompt <- function(abstract) {
  glue::glue("
You are an expert reviewer of USRDS-based research papers. 
Extract metadata using STRICT, SEQUENTIAL, and ANTI-HALLUCINATION rules.

Your job is to fill ONLY the fields in the schema.
Return no commentary and never invent information.

=========================================================
SECTION 1 — GLOBAL PRINCIPLES
=========================================================

1. If a detail is NOT explicitly stated in the abstract or text → return FALSE or NULL.
2. NEVER infer programming languages, crosswalks, or data sources.
3. If the abstract does not explicitly mention USRDS OR Medicare ESRD program data:
      → is_new_data_analysis = FALSE
      → files_used_* = FALSE
      → data_abstracted = empty
      → covered_by_usrds = FALSE
4. If information is ambiguous → treat as “uncertain”.

=========================================================
SECTION 2 — TOP-LEVEL SEQUENTIAL LOGIC
=========================================================

STEP 1 — Extract bibliographic metadata  
- authors  
- title  
- journal  
- year  

STEP 2 — Identify whether the study is a **new analysis using USRDS**  
Set is_new_data_analysis = TRUE only if the abstract is a new analysis of patient level data.
If the paper is a review article, this is false

STEP 3 — If is_new_data_analysis = FALSE  
Then ALL of the following MUST be forced:  
- files_used_* = FALSE  
- data_abstracted = empty  
- file_choice_appropriate = FALSE  
- covered_by_usrds = FALSE  
- tasks_not_covered = empty or NULL  

=========================================================
SECTION 3 — PROGRAMMING LANGUAGES
=========================================================

languages_used_* booleans should be TRUE ONLY if explicitly mentioned:

Examples that trigger TRUE:
- “Analysis performed in R 4.2”
- “SAS 9.4”
- “Python 3.10”
- “Stata MP”
- “SQL queries”
- “MATLAB toolbox”
- “SPSS statistics”
- “Julia scripts”

NEVER infer from vague phrases such as:
- “statistical software”
- “standard packages”
- “custom scripts”
- “statistical computing environment”

=========================================================
SECTION 4 — FILE SOURCE DETERMINATION
=========================================================

file_source_type must be:
- “explicit” → USRDS explicitly named  
- “inferred” → Medicare ESRD claims implied but not explicitly USRDS  
- “uncertain” → unclear or unspecified

=========================================================
SECTION 5 — USRDS FILE IDENTIFICATION VIA KEY PHRASES
=========================================================

Set each files_used_* boolean TRUE only if explicit textual evidence supports it.

-------------------------
A. CORE FILE (demographics, ESRD onset, 2728 form)
-------------------------
Trigger phrases:
- “ESRD Medical Evidence Form 2728”
- “Form 2728” / “CMS-2728”
- “date of ESRD onset”
- “initial Medicare eligibility”
- “cause of ESRD”
- “comorbid conditions from USRDS”
- “baseline demographics from USRDS Core”
- “ZIP code from USRDS”

-------------------------
B. TRANSPLANT FILE
-------------------------
Trigger phrases:
- “transplant date from USRDS”
- “waitlist information”
- “donor type (living/deceased)”
- “graft failure date”
- “US transplant registry linkage via USRDS”

-------------------------
C. PART D (medications)
-------------------------
Trigger phrases:
- “Medicare Part D prescription claims”
- “Part D drug fills”
- “immunosuppressive medication fills from Part D”

-------------------------
D. INSTITUTION FILE (dialysis facilities)
-------------------------
Trigger phrases:
- “facility characteristics”
- “facility profit status”
- “dialysis center chain affiliation”
- “facility-level variables from USRDS”


-------------------------
E. PHYSICIAN/SUPPLIER FILE
-------------------------
Trigger phrases:
- “physician claims”
- “provider services”
- “HCPCS codes”
- “CPT-coded outpatient procedures”
- “Part B claims”
- “Immunosuppression”

-------------------------
F. HOSPITAL FILE
-------------------------
Trigger phrases:
- “inpatient hospitalizations”
- “Medicare inpatient stays”
- “DRG codes”
- “hospitalization rates from USRDS”

If unsure → files_used_* = FALSE.

=========================================================
SECTION 6 — DATA ABSTRACTED (types of data)
=========================================================

Set TRUE only if explicitly mentioned:

- “baseline demographics”
- “ZIP code” / “residential location”
- “transplant-specific variables”
- “ICD-9 codes”
- “ICD-10 codes”
- “CPT codes”
- “Part D prescription fills”
- “Medicare costs” or “reimbursement amounts”

=========================================================
SECTION 7 — FILE APPROPRIATENESS
=========================================================

file_choice_appropriate = TRUE ONLY IF:
- the USRDS files used are clearly appropriate for the questions asked.

Examples (appropriate):
- “Evaluated hospitalization rates” → Hospital File
- “Identified medication adherence” → Part D

Examples (inappropriate):
- Studying drug use WITHOUT Part D
- Studying transplant type WITHOUT Transplant File

file_choice_appropriate_reason must be a SHORT STRING.

=========================================================
SECTION 8 — CROSSWALK USE
=========================================================

Trigger TRUE only if explicitly stated:
- “UNOS crosswalk”  
- “provider crosswalk”  
- “physician crosswalk”  
- “NPI linkage”  
- “UNOS–USRDS linkage”  

=========================================================
SECTION 9 — OUTPUT FORMAT (CRITICAL)
=========================================================

Return ONLY the following schema fields:

- authors
- title
- journal
- year

- is_new_data_analysis
- is_transplant_related

- stated_language
- languages_used_R
- languages_used_SAS
- languages_used_Stata
- languages_used_Python
- languages_used_SQL
- languages_used_Matlab
- languages_used_Julia
- languages_used_SPSS

- file_source_type

- files_used_Core
- files_used_Transplant
- files_used_PartD
- files_used_Institution
- files_used_PhysicianSupplier
- files_used_Hospital

- file_choice_appropriate_boolean
- file_choice_appropriate_reason

- comments
- shared_code

- funded
- funder

- covered_by_usrds
- tasks_not_covered

- crosswalk_used_boolean
- crosswalk_used_UNOS
- crosswalk_used_physician
- crosswalk_used_provider
")
}

chatgpt1 <- chat_openai(
  model = "gpt-5-mini",
  system_prompt = id_prompt("")
  )




# ---- Schema ----
type_summary <- type_object(
  "Schema for summarizing USRDS publication metadata.",
  authors = type_string("Authors in Last, First format"),
  title = type_string("Article title"),
  journal = type_string("Journal name"),
  year = type_integer("Publication year"),
  
  is_new_data_analysis = type_boolean("TRUE only if new USRDS analysis performed"),
  is_transplant_related = type_boolean("TRUE only if study explicitly about transplantation"),
  
  stated_language = type_boolean("Did paper explicitly state the programming language?"),
  languages_used_R=type_boolean("Did paper use the R programming language?"),
  languages_used_SAS=type_boolean("Did paper use the SAS programming language?"),
  languages_used_Stata=type_boolean("Did paper use the Stata programming language?"),
  languages_used_Python=type_boolean("Did paper use the Python programming language?"),
  languages_used_SQL=type_boolean("Did paper use the SQL programming language?"),
  languages_used_Matlab=type_boolean("Did paper use the Matlab programming language?"),
  languages_used_Julia=type_boolean("Did paper use the Julia programming language?"),
  languages_used_SPSS=type_boolean("Did paper use the SPSS programming language?"),
    
  file_source_type = type_enum("How USRDS file use was determined",
                               values = c("explicit", "inferred", "uncertain")
  ),
  
  files_used_Core= type_boolean("Was the Core file used?"),
  files_used_Transplant= type_boolean("Was the Transplant file used?"),
  files_used_PartD= type_boolean("Was the Medication Part D file used?"),
  files_used_Institution= type_boolean("Was the Institution file used?"),
  files_used_PhysicianSupplier= type_boolean("Was the Physician Supplier file used?"),
  files_used_Hospital= type_boolean("Was the Hospital file used?"),

  file_choice_appropriate_boolean = type_boolean("Were file choices appropriate?"),
  file_choice_appropriate_reason = type_string("Reason for choice of answer for file_choice_appropriate_boolean?"),
  comments = type_string("Comments, free text"),
  shared_code = type_boolean("Was the code shared?"),
  
  funded = type_boolean("Did the paper state funding?"),
  funder = type_string("Funder name"),
  
  covered_by_usrds = type_boolean("Could usRds R package (https://vagishhemmige.github.io/usRds/) reproduce this?"),
  tasks_not_covered = type_string("Tasks not supported in usrdsR"),
  
  crosswalk_used_boolean = type_boolean("Did the paper use any crosswalks?"),
  crosswalk_used_UNOS=type_boolean("Did the paper use the UNOS crosswalk?"),
  crosswalk_used_physician=type_boolean("Did the paper use the physician crosswalk?"),
  crosswalk_used_provider=type_boolean("Did the paper use the provider crosswalk?")
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
