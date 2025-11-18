#This file defines the abstract schema for the three LLM models

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
