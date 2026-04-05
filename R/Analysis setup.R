
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
library(labelled)



label_list <- list(
  did_the_authors_state_what_programming_language_they_used = "Programming language stated",
  did_the_authors_explicitly_state_what_files_they_used = "USRDS files explicitly stated",
  does_the_choice_of_files_codes_seem_appropriate_for_the_research_question = "Files/codes appropriate for research question",
  did_the_authors_share_the_code_they_used = "Authors shared code",
  was_the_study_funded = "Study funded",
  would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package = "Covered by proposed usRds package",
  were_any_crosswalks_used = "Any crosswalks used",
  language_r = "R used",
  language_sas = "SAS used",
  language_stata = "Stata used",
  language_spss = "SPSS used",
  file_core = "Core file used",
  file_institution = "Institution file used",
  file_ps = "Physician/Supplier file used",
  file_hospital = "Hospital file used",
  file_transplant = "Transplant file used",
  file_partd = "Part D file used",
  file_CROWNWeb = "CROWNWeb file used",
  component_basicdemographics = "Basic demographics abstracted",
  component_icd9 = "ICD-9 codes abstracted",
  component_icd10 = "ICD-10 codes abstracted",
  component_cptcode = "CPT codes abstracted",
  component_medicationD = "Part D medication data abstracted",
  component_transplant = "Transplant-specific data abstracted",
  component_costs = "Cost data abstracted",
  component_residential = "Residential location abstracted"
)