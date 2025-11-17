library(ellmer)
library(tidyverse)
library(pdftools)


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
