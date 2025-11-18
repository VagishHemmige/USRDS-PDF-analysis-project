#This file defines the abstract schema for the three LLM models

id_prompt <- function(abstract) {
  glue::glue("
You are an expert reviewer of USRDS-based research papers. 
Extract metadata using STRICT, SEQUENTIAL, and ANTI-HALLUCINATION rules.

Return ONLY the fields in the schema below.
Return no commentary, no reasoning, and never invent data.

=========================================================
SECTION 1 — GLOBAL PRINCIPLES
=========================================================

1. If a detail is NOT explicitly stated in the abstract or text → return FALSE or NULL.
2. NEVER infer:
   - programming languages
   - crosswalk use
   - USRDS files used
3. If the abstract does not explicitly mention USRDS OR Medicare ESRD program data:
      → is_this_paper_a_new_data_analysis = FALSE  
      → all files_used_* = FALSE  
      → all data components = FALSE  
         (component_basicdemographics, component_icd9, component_icd10,
          component_cptcode, component_medicationD, component_transplant,
          component_costs, component_residential)
      → does_the_choice_of_files_codes_seem_appropriate_for_the_research_question = FALSE
      → would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package = FALSE
      → which_tasks_would_not_be_covered = NULL
      → were_any_crosswalks_used = FALSE
4. If information is ambiguous → treat as “uncertain” where allowed; otherwise FALSE.

=========================================================
SECTION 2 — SEQUENTIAL LOGIC
=========================================================

STEP 1 — Extract bibliographic metadata:
- authors  
- title  
- journal  
- year  

STEP 2 — Identify whether this is a new analysis:
is_this_paper_a_new_data_analysis = TRUE only if:
- the study conducts original analyses using USRDS or Medicare ESRD patient-level data.

STEP 3 — Identify whether the study is transplant-related:
Set is_this_a_transplant_related_study = TRUE only if transplantation is explicitly studied.
This should be false if transplant date is only used as a censoring date, not as an explicit outcome

STEP 4 — Classify transplant study type:
which_type_of_transplant_study_is_this must be one of:
- “Pre-transplant access to care”
- “Pre-transplant management/outcomes”
- “Post-transplant”
- “NA” if is_this_a_transplant_related_study=FALSE

=========================================================
SECTION 3 — PROGRAMMING LANGUAGES
=========================================================

did_the_authors_state_what_programming_language_they_used = TRUE only if the abstract explicitly names the language.

languages_used_* booleans should be TRUE ONLY if explicitly mentioned:
- R, SAS, Stata, Python, SQL, Matlab, Julia, SPSS

Do NOT infer from:
- “statistical software”
- “common packages”
- “analysis performed in standard software”

=========================================================
SECTION 4 — FILE SOURCE DETERMINATION
=========================================================

file_source_type must be exactly:
- “explicit” → USRDS named directly  
- “inferred” → Medicare ESRD claims implied but USRDS not named  
- “uncertain” → unclear  

=========================================================
SECTION 5 — USRDS FILE IDENTIFICATION
=========================================================

Set each files_used_* = TRUE only if explicitly mentioned:

CORE → 2728 form, Medical Evidence form, ESRD onset, cause of ESRD, baseline demographics.  Date of transplant. 
Residence. Payer.  Cause of ESRD.  Baseline comorbidities.
TRANSPLANT → graft failure, donor type, waitlist.  Most transplant details except date of transplant require this file.
PART D → Medicare Part D drug fills (any medication except immunosuppression, which is Part B i.e. Physician Supplier)  
INSTITUTION → Any billing by hospitals, clinics, hospice, home health, inpatient, outpatient (Part A)
PHYSICIAN/SUPPLIER → Part B claims Physician/supplier claims are bills covering physician, laboratory, 
  and radiology services, as well as medical supplies/DME 
HOSPITAL → inpatient stays, DRG codes, hospitalization rates.  Costs are not present and require the INSTITUTION file  
CROWNWeb → “CROWNWeb” or facility clinical metrics.

=========================================================
SECTION 6 — DATA COMPONENTS
=========================================================

Each component_* must be TRUE only if explicitly mentioned:

- component_basicdemographics  
- component_icd9  
- component_icd10  
- component_cptcode  
- component_medicationD  
- component_transplant  
- component_costs  
- component_residential  

=========================================================
SECTION 7 — FILE APPROPRIATENESS
=========================================================

does_the_choice_of_files_codes_seem_appropriate_for_the_research_question = TRUE only when:
- the files used clearly match the research aims.

file_choice_appropriate_reason = SHORT STRING.

=========================================================
SECTION 8 — CROSSWALK USE
=========================================================

were_any_crosswalks_used = TRUE only if explicitly stated.

Trigger TRUE for specific crosswalks only if explicitly named:
- UNOS crosswalk  
- physician crosswalk  
- provider crosswalk  

crosswalk_used_UNOS  
crosswalk_used_physician  
crosswalk_used_provider  

If ANY of these are TRUE → were_any_crosswalks_used must be TRUE.

=========================================================
SECTION 9 — USRDS PACKAGE COVERAGE
=========================================================

would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package:
TRUE only if study tasks can be performed using usRds.

which_tasks_would_not_be_covered:
List tasks only if the above is FALSE.

=========================================================
SECTION 10 — OUTPUT FORMAT (CRITICAL)
=========================================================

Return ONLY the following fields, with EXACT names:

- authors
- title
- journal
- year

- is_this_paper_a_new_data_analysis
- is_this_a_transplant_related_study
- which_type_of_transplant_study_is_this

- did_the_authors_state_what_programming_language_they_used
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
- files_used_CROWNWeb

- does_the_choice_of_files_codes_seem_appropriate_for_the_research_question
- file_choice_appropriate_reason

- comments
- did_the_authors_share_the_code_they_used

- component_basicdemographics
- component_icd9
- component_icd10
- component_cptcode
- component_medicationD
- component_transplant
- component_costs
- component_residential

- was_the_study_funded
- who_funded_the_study

- would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package
- which_tasks_would_not_be_covered

- were_any_crosswalks_used
- crosswalk_used_UNOS
- crosswalk_used_physician
- crosswalk_used_provider")
}


# ---- Schema ----
type_summary <- type_object(
  "Schema for summarizing USRDS publication metadata.",
  authors = type_string("Authors in Last, First format"),
  title = type_string("Article title"),
  journal = type_string("Journal name"),
  year = type_integer("Publication year"),
  
  is_this_paper_a_new_data_analysis = type_boolean("TRUE only if new USRDS analysis performed"),
  is_this_a_transplant_related_study = type_boolean("TRUE only if study explicitly about transplantation"),
  
  which_type_of_transplant_study_is_this = type_enum(
    "What type of transplant study is this? NA if not transplant-related",
    values = c("Pre-transplant access to care", 
               "Pre-transplant management/outcomes", 
               "Post-transplant", 
               "NA")
  ),
  
  did_the_authors_state_what_programming_language_they_used = type_boolean("Did paper explicitly state the programming language?"),
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
  files_used_CROWNWeb= type_boolean("Was the CROWNWeb file used?"),
  
  
  does_the_choice_of_files_codes_seem_appropriate_for_the_research_question = type_boolean("Were file choices appropriate?"),
  file_choice_appropriate_reason = type_string("Reason for choice of answer for file_choice_appropriate_boolean?"),
  comments = type_string("Comments, free text"),
  did_the_authors_share_the_code_they_used = type_boolean("Was the code shared?"),
  
  component_basicdemographics = type_boolean("Did the study abstract basic demographics?"),
  component_icd9 = type_boolean("Did the study abstract ICD-9 codes?"),
  component_icd10 = type_boolean("Did the study abstract ICD-10 codes?"),
  component_cptcode = type_boolean("Did the study abstract CPT codes?"),
  component_medicationD = type_boolean("Did the study abstract Medicare Part D medications?"),
  component_transplant = type_boolean("Did the study abstract transplant-specific information?"),
  component_costs = type_boolean("Did the study abstract Medicare costs or reimbursements?"),
  component_residential = type_boolean("Did the study abstract ZIP code or residential location?"),
  
  was_the_study_funded = type_boolean("Did the paper state funding?"),
  who_funded_the_study = type_string("Funder name"),
  
  would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package = 
    type_boolean("Could usRds R package (https://vagishhemmige.github.io/usRds/) reproduce this?"),
  which_tasks_would_not_be_covered = type_string("Tasks not supported in usrdsR"),
  
  were_any_crosswalks_used = type_boolean("Did the paper use any crosswalks?"),
  crosswalk_used_UNOS=type_boolean("Did the paper use the UNOS crosswalk?"),
  crosswalk_used_physician=type_boolean("Did the paper use the physician crosswalk?"),
  crosswalk_used_provider=type_boolean("Did the paper use the provider crosswalk?")
)
