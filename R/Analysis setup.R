
library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(janitor)
library(irr)
library(gtsummary)
library(gt)
library(officer)
library(tidyr)
library(stringr)
library(flowchart)
library(ellmer)
library(pdftools)
library(rstudioapi)
library(purrr)



label_list <- list(
  did_the_authors_state_what_programming_language_they_used = "Programming language stated",
  did_the_authors_explicitly_state_what_files_they_used = "USRDS files explicitly stated",
  does_the_choice_of_files_codes_seem_appropriate_for_the_research_question = "Files/codes appropriate for research question",
  did_the_authors_share_the_code_they_used = "Authors shared code",
  was_the_study_funded = "Study funded",
  would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package = "Covered by proposed usRds package",
  were_any_crosswalks_used = "Any crosswalks used",
  languages_used_R = "R used",
  languages_used_SAS = "SAS used",
  languages_used_Stata = "Stata used",
  languages_used_SPSS = "SPSS used",
  languages_used_Python = "Python used",
  
  files_used_Core = "Core file used",
  files_used_Institution = "Institution file used",
  files_used_PhysicianSupplier = "Physician/Supplier file used",
  files_used_Hospital = "Hospital file used",
  files_used_Transplant = "Transplant file used",
  files_used_PartD= "Part D file used",
  files_used_CROWNWeb = "CROWNWeb file used",
  
  component_basicdemographics = "Basic demographics abstracted",
  component_icd9 = "ICD-9 codes abstracted",
  component_icd10 = "ICD-10 codes abstracted",
  component_cptcode = "CPT codes abstracted",
  component_medicationD = "Part D medication data abstracted",
  component_transplant = "Transplant-specific data abstracted",
  component_costs = "Cost data abstracted",
  component_residential = "Residential location abstracted",
  
  is_this_a_transplant_related_study = "Transplant-related study",
  is_this_paper_a_new_data_analysis = "New patient-level data analysis"
)

#Table of times to run the analysis
timing_table <- tibble(
  `Provider (Company)` = c(
    "ChatGPT (OpenAI)",
    "Claude (Anthropic)",
    "Gemini (Google)"
  ),
  Model = c(
    "Gpt 5.1",
    "Claude Sonnet 4.5",
    "Gemini 2.5 Flash"
  ),
  `Time (minutes)` = c(
    165,
    62,
    672
  )
)
