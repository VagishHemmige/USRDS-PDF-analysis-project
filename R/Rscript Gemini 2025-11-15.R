# ---- Libraries ----
library(ellmer)
library(tidyverse)
library(pdftools)
library(rstudioapi)

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
      "type_summary_gemini_schema.rds"
    ))
    deleted <- file.remove(files_to_delete[file.exists(files_to_delete)])
    message("🧹 Deleted ", sum(deleted), " files.")
  }
}

# ---- Setup ----
folder_path <- "data-raw/PDFs"
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

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
